use crate::delta::*;
use crate::*;
use dyn_clone::DynClone;
use fxhash::FxHashMap;
use std::collections::{hash_map::Entry, HashMap};
use std::fmt::Debug;
use std::marker::PhantomData;

/// The outcome of a finished [`Game`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Outcome {
    /// The winner of the game has been decided.
    Decisive { winner: PlayerIndex },
    /// The game resulted in a draw.
    Draw,
}

/// [`VictoryCondition`]s evaluate the outcome of a game after every played move.
/// They also decide which pseudo-legal moves are legal, or, in other words, rejects certain
/// generated moves.
pub trait VictoryCondition<G: BoardGeometry>: Send + Sync + Debug + DynClone {
    /// Determines the outcome of a game at the current state.
    /// Not to be called directly, use [`Game::get_outcome`] instead.
    fn evaluate(
        &self,
        game: &Game<G, NotEvaluated>,
        available_moves: &AvailableMoves<G>,
    ) -> Option<Outcome>;

    /// Determines whether a pseudo-legal move is legal.
    fn is_move_legal(&self, game: &Game<G, NotEvaluated>, mv: &ReversibleGameStateDelta<G>)
        -> bool;
}

dyn_clone::clone_trait_object!(<G> VictoryCondition<G> where G: BoardGeometry);

/// A [`VictoryCondition`] with no possible victories and no illegal moves.
/// The game continues until all but one players concede.
#[derive(Debug, Clone, Default)]
pub struct NoVictoryCondition;

impl<G: BoardGeometry> VictoryCondition<G> for NoVictoryCondition {
    fn evaluate(
        &self,
        _game: &Game<G, NotEvaluated>,
        _available_moves: &AvailableMoves<G>,
    ) -> Option<Outcome> {
        None
    }

    fn is_move_legal(
        &self,
        _game: &Game<G, NotEvaluated>,
        _mv: &ReversibleGameStateDelta<G>,
    ) -> bool {
        true
    }
}

/// If the current player appears in a stalemate, how is the game evaluated?
#[derive(Debug, Clone, Copy)]
pub enum StalemateEvaluation {
    Draw,
    Loss,
}

// FIXME: Enforce that the the game rules are made for exactly 2 players.
/// A [`VictoryCondition`] which predicts an inevitable victory 2 turns ahead and makes
/// moves which would result in a loss in the next turn illegal.
///
/// A combination of `PredictiveVictoryCondition` and `RoyalVictoryCondition` can provide
/// victory conditions typical for international chess.
#[derive(Debug, Clone)]
pub struct PredictiveVictoryCondition<G: BoardGeometry, C: VictoryCondition<G>> {
    inner: C,
    stalemate_evaluation: StalemateEvaluation,
    __marker: PhantomData<G>,
}

impl<G, C> PredictiveVictoryCondition<G, C>
where
    G: BoardGeometry + Send + Sync + Debug + Clone,
    C: Send + Sync + Debug + Clone + VictoryCondition<G>,
{
    pub fn new(stalemate_evaluation: StalemateEvaluation, inner: C) -> Self {
        Self {
            inner,
            stalemate_evaluation,
            __marker: Default::default(),
        }
    }
}

impl<G, C> VictoryCondition<G> for PredictiveVictoryCondition<G, C>
where
    G: BoardGeometry + Send + Sync + Debug + Clone,
    C: Send + Sync + Debug + Clone + VictoryCondition<G> + 'static,
{
    fn evaluate(
        &self,
        game: &Game<G, NotEvaluated>,
        available_moves: &AvailableMoves<G>,
    ) -> Option<Outcome> {
        if let Some(outcome) = self.inner.evaluate(game, available_moves) {
            return Some(outcome);
        }

        if available_moves.moves().count() == 0 {
            // The next player, who may be the winner.
            let next_player = (game.move_log().current_state().current_player_index() + 1)
                % game.rules().players().get() as PlayerIndex;
            let loss_outcome = Outcome::Decisive {
                winner: next_player,
            };

            match self.stalemate_evaluation {
                StalemateEvaluation::Loss => Some(loss_outcome),
                StalemateEvaluation::Draw => {
                    let mut game = game.clone_with_victory_conditions(Box::new(self.inner.clone()));

                    if !game.move_log().is_empty() {
                        // If a move was played, replay that move with its `next_player` set to the
                        // current `next_player`.
                        let mut last_mv = game.move_log.undo(&mut game.move_cache, 1).remove(0);
                        last_mv.set_next_player(next_player);
                        game = game.append_delta_unchecked_without_evaluation(last_mv);
                    } else {
                        // Otherwise, just change the current state, which is the initial state.
                        game.move_log.current_state.current_player_index = next_player;
                    }

                    // Does another move of the potential winner exist, that would decide the game?
                    let game = game.evaluate();
                    let loss = game.available_moves().deltas().any(|delta| {
                        let game = game.clone().append_delta_unchecked(delta.clone());

                        game.get_outcome() == Some(&loss_outcome)
                    });

                    if loss {
                        // Yes, it does, the game is decided.
                        Some(loss_outcome)
                    } else {
                        // Draw by stalemate.
                        Some(Outcome::Draw)
                    }
                }
            }
        } else {
            None
        }
    }

    fn is_move_legal(
        &self,
        game: &Game<G, NotEvaluated>,
        mv: &ReversibleGameStateDelta<G>,
    ) -> bool {
        // FIXME: Replace Box with Arc so as to avoid unnecessary allocations
        let defending_game = game.clone_with_victory_conditions(Box::new(self.inner.clone()));

        // Play the defending move of the current player.
        let defending_game = defending_game.append_delta_unchecked(mv.clone());

        // Find an attacking move that would decide the game.
        for attacking_move in defending_game.available_moves().moves() {
            let attacking_game = defending_game.clone();
            let attacking_game = attacking_game.append_unchecked(attacking_move.clone());
            let outcome = attacking_game.get_outcome();

            if let Some(Outcome::Decisive { winner }) = outcome {
                if *winner != game.move_log().current_state().current_player_index() {
                    // This defending move is invalid, because it can be countered by an attacking move
                    // that decides the game.
                    return false;
                }
            }
        }

        true
    }
}

/// The type of [`RoyalVictoryCondition`].
#[derive(Clone, Debug, Copy)]
pub enum RoyalVictoryType {
    /// A player loses the game if they lose **at least one** of their royal pieces.
    Absolute,
    /// A player loses the game if they lose **all** of their royal pieces.
    Extinction,
}

#[derive(Clone, Debug, Copy)]
enum Quantifier {
    All,
    Any,
}

/// A [`VictoryCondition`] which marks certain piece definitions (kinds of pieces) as _royal_,
/// and then decides the loss of a player based upon the number of their _royal_ pieces alive.
///
/// See [`RoyalVictoryType`] for more information about how the loss is decided upon.
#[derive(Clone, Debug)]
pub struct RoyalVictoryCondition {
    /// (Player, Piece Definition) -> Initial Count
    initial_piece_count_per_player: FxHashMap<(u8, u16), usize>,
    /// (Player, Piece Definition) -> Min Count
    min_piece_count_per_player: FxHashMap<(u8, u16), usize>,
    loss_when_insufficient_count_by: Quantifier,
}

impl RoyalVictoryCondition {
    pub fn new<G: BoardGeometry>(ty: RoyalVictoryType, initial_state: &GameState<G>) -> Self {
        let initial_piece_count_per_player = initial_state
            .pieces
            .values()
            .map(|piece| (piece.owner(), piece.definition_index()))
            .fold(HashMap::default(), |mut acc, key| {
                match acc.entry(key) {
                    Entry::Vacant(entry) => {
                        entry.insert(1);
                    }
                    Entry::Occupied(ref mut entry) => {
                        *entry.get_mut() += 1;
                    }
                }

                acc
            });

        Self {
            initial_piece_count_per_player,
            min_piece_count_per_player: Default::default(),
            loss_when_insufficient_count_by: match ty {
                RoyalVictoryType::Absolute => Quantifier::Any,
                RoyalVictoryType::Extinction => Quantifier::All,
            },
        }
    }

    pub fn with_min_piece_count(
        mut self,
        player_index: PlayerIndex,
        piece_definition_index: PieceDefinitionIndex,
        min_piece_count: usize,
    ) -> Self {
        if min_piece_count > 0 {
            self.min_piece_count_per_player
                .insert((player_index, piece_definition_index), min_piece_count);
        } else {
            self.min_piece_count_per_player
                .remove(&(player_index, piece_definition_index));
        }

        self
    }

    pub fn with_max_takes_of_piece(
        self,
        player_index: PlayerIndex,
        piece_definition_index: PieceDefinitionIndex,
        max_takes: usize,
    ) -> Self {
        if let Some(initial_count) = self
            .initial_piece_count_per_player
            .get(&(player_index, piece_definition_index))
            .cloned()
        {
            self.with_max_takes_of_piece(
                player_index,
                piece_definition_index,
                initial_count.saturating_sub(max_takes),
            )
        } else {
            self
        }
    }
}

impl<G: BoardGeometry> VictoryCondition<G> for RoyalVictoryCondition {
    fn evaluate(
        &self,
        game: &Game<G, NotEvaluated>,
        _available_moves: &AvailableMoves<G>,
    ) -> Option<Outcome> {
        let state = game.move_log().current_state();
        let rules = game.rules();
        // Initialize piece counts with all piece definitions and counts of 0
        let piece_counts_per_player = (0..game.rules().piece_set().definitions().len())
            .flat_map(|definition_index| {
                (0..game.rules().players().get()).map(move |player_index| {
                    (
                        (
                            player_index as PlayerIndex,
                            definition_index as PieceDefinitionIndex,
                        ),
                        0,
                    )
                })
            })
            .collect::<FxHashMap<(PlayerIndex, PieceDefinitionIndex), usize>>();
        let piece_counts_per_player = state
            .pieces
            .values()
            .map(|piece| (piece.owner(), piece.definition_index()))
            .fold(piece_counts_per_player, |mut acc, key| {
                *acc.get_mut(&key).unwrap() += 1;

                acc
            });
        let initial_player_evaluation = match self.loss_when_insufficient_count_by {
            Quantifier::All => true,
            Quantifier::Any => false,
        };
        let initial_player_evaluations = (0..rules.players().get())
            .map(|player| (player as PlayerIndex, initial_player_evaluation))
            .collect::<FxHashMap<PlayerIndex, bool>>();

        let players_alive: Vec<PlayerIndex> = self
            .min_piece_count_per_player
            .iter()
            .map(move |(&(player, piece), min_count)| {
                let insufficient_count = piece_counts_per_player
                    .get(&(player, piece))
                    .map(|count| *count < *min_count)
                    .unwrap_or(false);

                (player, insufficient_count)
            })
            .fold(
                initial_player_evaluations,
                |mut acc, (player, insufficient_count)| {
                    let loss = acc.get_mut(&player).unwrap();

                    match self.loss_when_insufficient_count_by {
                        Quantifier::All => *loss &= insufficient_count,
                        Quantifier::Any => *loss |= insufficient_count,
                    }

                    acc
                },
            )
            .into_iter()
            .filter(|(_, loss)| !loss)
            .map(|(player, _)| player)
            .collect();

        if players_alive.is_empty() {
            Some(Outcome::Draw)
        } else if players_alive.len() == 1 {
            Some(Outcome::Decisive {
                winner: *players_alive.first().unwrap(),
            })
        } else {
            None
        }
    }

    fn is_move_legal(
        &self,
        game: &Game<G, NotEvaluated>,
        mv: &ReversibleGameStateDelta<G>,
    ) -> bool {
        let current_player = game.move_log().current_state().current_player_index();
        let game = game
            .clone()
            .append_delta_unchecked_without_evaluation(mv.clone());

        // Do not let the player make a move that result in them lose the game.
        // Evaluation does not require available moves to be known, hence we can pass
        // `AvailableMoves::empty()`.
        if let Some(Outcome::Decisive { winner }) = self.evaluate(&game, &AvailableMoves::empty()) {
            if winner != current_player {
                return false;
            }
        }

        true
    }
}
