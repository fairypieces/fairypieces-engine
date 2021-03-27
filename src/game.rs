use std::marker::PhantomData;
use std::fmt::Debug;
use std::sync::Arc;
use std::ops::{Sub, Deref};
use std::num::NonZeroUsize;
use std::collections::{HashMap, VecDeque, BTreeSet, BTreeMap, HashSet, hash_map::Entry};
use dyn_clone::DynClone;
use replace_with::replace_with_or_default;
use crate::*;

#[derive(Clone, Debug)]
pub struct MoveLog<G: BoardGeometry> {
    pub(crate) initial_state: GameState<G>,
    pub(crate) current_state: GameState<G>,
    /// A list of normalized deltas.
    /// Applying all deltas in the given order to `initial_state` produces `current_state`.
    pub(crate) moves: Vec<ReversibleGameStateDelta<G>>,
}

#[derive(Copy, Clone, Debug)]
pub enum PastTileError {
    /// The tile from the requested move precedes the start of the game.
    PrecedingStart,
    /// The tile is not on the board.
    MissingTile,
}

impl<G: BoardGeometry> MoveLog<G> {
    pub fn new(initial_state: GameState<G>) -> Self {
        Self {
            current_state: initial_state.clone(),
            initial_state,
            moves: Default::default(),
        }
    }

    pub fn append(&mut self, delta: ReversibleGameStateDelta<G>) {
        let previous_game_state = self.current_state.clone();

        self.moves.push(delta.clone());
        replace_with_or_default(&mut self.current_state, |current_state| current_state.apply(delta.forward.clone()));

        // Ensure all appended moves are reversible. Ensured using induction.
        debug_assert_eq!(previous_game_state, self.current_state.clone().apply(delta.backward));

        // Disabled condition that checks all recorded moves -- wasteful.
        // debug_assert!(self.initial_state == self.moves.iter().rev().fold(self.current_state.clone(), |state, mv| state.apply(mv.backward.clone())));
    }

    // pub fn append(&mut self, mv: Move<G>) {
    //     self.moves.push(mv.delta.clone());
    //     replace_with_or_default(&mut self.current_state, move |current_state| current_state.apply(mv.delta.forward));

    //     // Ensure all appended moves are reversible
    //     debug_assert!(self.initial_state == self.moves.iter().rev().fold(self.current_state.clone(), |state, mv| state.apply(mv.backward.clone())));
    // }

    pub fn initial_state(&self) -> &GameState<G> {
        &self.initial_state
    }

    pub fn current_state(&self) -> &GameState<G> {
        &self.current_state
    }

    pub fn moves(&self) -> &[ReversibleGameStateDelta<G>] {
        &self.moves
    }
}

#[derive(Default, Clone, PartialEq, Eq, Debug)]
pub struct AvailableMoves<G: BoardGeometry> {
    pub(crate) moves_from: HashMap<<G as BoardGeometryExt>::Tile, HashSet<Move<G>>>,
    pub(crate) moves: HashSet<Move<G>>,
    pub(crate) deltas: HashSet<ReversibleGameStateDelta<G>>,
}

impl<G: BoardGeometry> AvailableMoves<G> {
    pub fn empty() -> Self {
        Default::default()
    }

    /// Returns an iterator over all valid moves with a piece at the specified tile.
    pub fn moves_from(&self, tile: <G as BoardGeometryExt>::Tile) -> impl Iterator<Item=&Move<G>> {
        self.moves_from.get(&tile).into_iter().flat_map(|set| set.into_iter())
    }

    /// Returns an iterator over all valid moves.
    pub fn moves(&self) -> impl Iterator<Item=&Move<G>> {
        self.moves.iter()
    }

    /// Returns an iterator over all valid move deltas.
    pub fn deltas(&self) -> impl Iterator<Item=&ReversibleGameStateDelta<G>> {
        self.deltas.iter()
    }

    /// Returns `true` if there exists a valid move for the piece at the specified `tile`
    /// with a matching `delta` and `final_tile`.
    pub fn is_move_available_from(&self, tile: <G as BoardGeometryExt>::Tile, mv: &Move<G>) -> bool {
        self.moves_from.get(&tile).map(|moves| moves.contains(mv)).unwrap_or(false)
    }

    /// Returns `true` if there exists a valid move with a matching `delta` and `final_tile`.
    pub fn is_move_available(&self, mv: &Move<G>) -> bool {
        self.moves.contains(mv)
    }

    /// Returns `true` if there exists a valid move with a matching `delta`. Does not check
    /// `Move::final_tile` as `Self::is_move_available()` does.
    pub fn is_delta_available(&self, delta: &ReversibleGameStateDelta<G>) -> bool {
        self.deltas.contains(delta)
    }
}

#[derive(Clone, Debug)]
pub struct Game<G: BoardGeometry> {
    pub(crate) rules: Arc<GameRules<G>>,
    pub(crate) move_log: MoveLog<G>,
    pub(crate) available_moves: Arc<AvailableMoves<G>>,
    pub(crate) outcome: Option<Outcome>,
}

impl<G: BoardGeometry> Game<G> {
    pub fn new(rules: GameRules<G>, initial_state: GameState<G>) -> Self {
        let mut result = Self {
            rules: Arc::new(rules),
            move_log: MoveLog::new(initial_state),
            available_moves: Default::default(),
            outcome: None,
        };

        result.evaluate();

        result
    }

    pub fn clone_with_victory_conditions(&self, victory_conditions: Box<dyn VictoryConditions<G>>) -> Self {
        let mut result = self.clone();
        Arc::make_mut(&mut result.rules).victory_conditions = victory_conditions;

        result.evaluate();

        result
    }

    /// Returns the outcome of the game at the current state.
    pub fn get_outcome(&self) -> Option<&Outcome> {
        self.outcome.as_ref()
    }

    /// Returns a collection of all valid moves.
    pub fn available_moves(&self) -> &AvailableMoves<G> {
        &self.available_moves
    }

    #[must_use = "The move may not be available."]
    pub fn append_delta(&mut self, delta: ReversibleGameStateDelta<G>) -> Result<(), ()> {
        if self.outcome.is_some() {
            return Err(());
        }

        self.available_moves().is_delta_available(&delta).then(|| {
            self.append_delta_unchecked(delta)
        }).ok_or(())
    }

    pub fn append_delta_unchecked(&mut self, delta: ReversibleGameStateDelta<G>) {
        self.move_log.append(delta);
        self.evaluate();
    }

    #[must_use = "The move may not be available."]
    pub fn normalize_and_append_delta(&mut self, delta: GameStateDelta<G>) -> Result<(), ()> {
        let normalized = delta.normalize(&self.move_log.current_state);

        self.append_delta(normalized)
    }

    #[must_use = "The move may not be available."]
    pub fn append(&mut self, mv: Move<G>) -> Result<(), ()> {
        if self.outcome.is_some() {
            return Err(());
        }

        self.available_moves().is_move_available(&mv).then(|| {
            self.append_unchecked(mv);
        }).ok_or(())
    }

    fn append_unchecked(&mut self, mv: Move<G>) {
        self.append_unchecked_without_evaluation(mv);
        self.evaluate();
    }

    fn append_unchecked_without_evaluation(&mut self, mv: Move<G>) {
        self.move_log.append(mv.delta);
    }

    /// Returns the piece at tile `tile` on the board `before_moves` moves ago.
    pub fn past_tile(&self, before_moves: usize, tile: <G as BoardGeometryExt>::Tile) -> Result<Option<&Piece<G>>, PastTileError> {
        if before_moves > self.move_log.moves.len() {
            return Err(PastTileError::PrecedingStart);
        }

        if !self.rules.board.tiles.contains(tile) {
            return Err(PastTileError::MissingTile);
        }

        if before_moves > 0 {
            let moves_played_since = &self.move_log.moves[(self.move_log.moves.len() - before_moves)..];

            for mv in moves_played_since {
                if let Some(piece) = mv.backward.affected_pieces.get(&tile) {
                    return Ok(piece.as_ref());
                }
            }
        }

        Ok(self.move_log.current_state.pieces.get(&tile))
    }

    fn evaluate(&mut self) {
        self.evaluate_available_moves();
        self.evaluate_outcome();

        if self.outcome.is_some() {
            self.available_moves = Arc::new(AvailableMoves::empty());
        }
    }

    fn evaluate_outcome(&mut self) {
        self.outcome = self.rules.victory_conditions.evaluate(&self);
    }

    fn evaluate_available_moves(&mut self) {
        // dbg!(backtrace::Backtrace::new());
        let mut available_moves = AvailableMoves::empty();

        let current_player = self.move_log.current_state.currently_playing_player_index;

        for (tile, piece) in &self.move_log.current_state.pieces {
            if piece.owner == current_player {
                if let Ok(current_available_moves) = self.evaluate_available_moves_from_tile(*tile) {
                    available_moves.deltas.extend(current_available_moves.iter().map(|mv| mv.delta.clone()));
                    available_moves.moves_from.entry(*tile)
                        .or_insert_with(|| Default::default())
                        .extend(current_available_moves.clone());
                    available_moves.moves.extend(current_available_moves);
                }
            }
        }

        self.available_moves = Arc::new(available_moves);
    }

    fn evaluate_available_moves_from_tile(&self, tile: <G as BoardGeometryExt>::Tile) -> Result<HashSet<Move<G>>, ()> {
        // TODO: caching (avoid exploring duplicate states)
        // TODO: generalize
        let next_player = (self.move_log.current_state.currently_playing_player_index + 1) % self.rules.players.get();
        let tile_ref = self.move_log.current_state.tile(&self.rules.board, tile).ok_or(())?;
        let piece = tile_ref.get_piece().ok_or(())?.clone();

        if self.move_log.current_state.currently_playing_player_index != piece.owner {
            return Ok(HashSet::new());
        }

        let definition = piece.get_definition(&self.rules.piece_set);

        struct QueueItem<G2: BoardGeometry> {
            tile: <G2 as BoardGeometryExt>::Tile,
            axis_permutation: AxisPermutation<G2>,
            delta: GameStateDelta<G2>,
            state_index: usize,
        }

        let mut potential_moves = BTreeSet::new();
        let mut queue = VecDeque::<QueueItem<G>>::new();

        for initial_state_index in &*definition.initial_states {
            queue.push_back(QueueItem {
                tile,
                axis_permutation: Default::default(),
                delta: GameStateDelta::with_next_player(next_player),
                state_index: *initial_state_index,
            });
        }

        // DFS the moves
        while let Some(QueueItem { tile, axis_permutation, mut delta, state_index }) = queue.pop_front() {
            let state = &definition.states[state_index];
            let game_state = self.move_log.current_state.clone().apply(delta.clone());
            let piece = game_state.tile(&self.rules.board, tile).ok_or(())?.get_piece().ok_or(())?.clone();
            let isometry = Isometry::from(axis_permutation.clone() * piece.transformation.clone()) * Isometry::translation(tile);

            let moves: Vec<(_, _, _)> = match state.action.clone() {
                Action::Move { condition, actions, move_choices } => {
                    if !condition.evaluate(self, &isometry) {
                        continue;
                    }

                    for action in actions {
                        match action {
                            ActionEnum::SetTile { target, piece } => {
                                let target = isometry.apply(target);

                                delta.affected_pieces.insert(target, piece.map(|piece| piece.clone_moved()));
                            },
                            ActionEnum::CopyTile { source, target } => {
                                let source = isometry.apply(source);
                                let target = isometry.apply(target);
                                let piece = game_state.tile(&self.rules.board, source).and_then(|tile| tile.get_piece().cloned());

                                delta.affected_pieces.insert(target, piece.map(|piece| piece.clone_moved()));
                            },
                        }
                    }

                    move_choices.into_iter().filter_map(|move_choice| {
                        if let Some(move_choice) = move_choice {
                            let move_choice = isometry.apply(move_choice);
                            let mut delta = delta.clone();

                            if tile != move_choice {
                                delta.affected_pieces.insert(move_choice, Some(piece.clone_moved()));
                                delta.affected_pieces.insert(tile, None);
                            }

                            Some((move_choice, axis_permutation.clone(), delta))
                        } else {
                            None
                        }
                    }).collect()
                },
                Action::Symmetry { symmetries } => {
                    symmetries.into_iter().map(|axis_permutation| {
                        // let mut delta = delta.clone();
                        // let mut piece = piece.clone();
                        // piece.transformation = axis_permutation * piece.transformation;

                        // delta.affected_pieces.insert(tile.clone(), Some(piece));

                        (tile, axis_permutation, delta.clone())
                    }).collect()
                },
            };

            for (tile, _, delta) in &moves {
                if state.is_final {
                    potential_moves.insert(Move::from(
                        &self.move_log.current_state,
                        delta.clone(),
                        *tile,
                    ));
                }
            }

            for successor_state_index in &*state.successor_indices {
                for (tile, axis_permutation, delta) in &moves {
                    queue.push_back(QueueItem {
                        tile: *tile,
                        axis_permutation: axis_permutation.clone(),
                        delta: delta.clone(),
                        state_index: *successor_state_index,
                    });
                }
            }
        }

        let valid_moves: HashSet<_> = potential_moves.into_iter().enumerate()
            .filter(|(index, mv)| {
                // println!("Checking if move #{} is valid...", index);
                self.rules().victory_conditions.is_move_valid(self, mv)
            })
            .map(|(index, mv)| mv)
            .collect();

        // println!("Valid moves: {}", valid_moves.len());

        Ok(valid_moves)
    }

    pub fn move_log(&self) -> &MoveLog<G> {
        &self.move_log
    }

    pub fn rules(&self) -> &GameRules<G> {
        &self.rules
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Outcome {
    Decisive {
        winner: usize,
    },
    Draw,
}

pub trait VictoryConditions<G: BoardGeometry>: Send + Sync + Debug + DynClone {
    fn evaluate(&self, game: &Game<G>) -> Option<Outcome>;

    fn is_move_valid(&self, current_state: &Game<G>, mv: &Move<G>) -> bool {
        true
    }
}

dyn_clone::clone_trait_object!(<G> VictoryConditions<G> where G: BoardGeometry);

#[derive(Debug, Clone)]
pub struct NoVictoryConditions;

impl<G: BoardGeometry> VictoryConditions<G> for NoVictoryConditions {
    fn evaluate(&self, _game: &Game<G>) -> Option<Outcome> {
        None
    }
}

#[derive(Debug, Clone)]
pub struct PredictiveVictoryConditions<G: BoardGeometry, C: VictoryConditions<G>> {
    inner: C,
    __marker: PhantomData<G>,
}

impl<G, C> PredictiveVictoryConditions<G, C>
where
    G: BoardGeometry + Send + Sync + Debug + Clone,
    C: Send + Sync + Debug + Clone + VictoryConditions<G>,
{
    pub fn new(inner: C) -> Self {
        Self {
            inner,
            __marker: Default::default(),
        }
    }
}

impl<G, C> VictoryConditions<G> for PredictiveVictoryConditions<G, C>
where
    G: BoardGeometry + Send + Sync + Debug + Clone,
    C: Send + Sync + Debug + Clone + VictoryConditions<G> + 'static,
{
    fn evaluate(&self, game: &Game<G>) -> Option<Outcome> {
        // let mut inner_game = game.clone();
        // Arc::make_mut(&mut inner_game.rules).victory_conditions = Box::new(self.inner.clone());

        // let mut decisive_outcome: Option<Option<Outcome>> = None;

        // dbg!(inner_game.available_moves().moves().count());

        // // For every defending move...
        // 'defending:
        // for defending_move in inner_game.available_moves().moves() {
        //     let mut defending_game = inner_game.clone();

        //     defending_game.append(defending_move.clone()).unwrap();

        //     let attacking_moves = defending_game.available_moves().moves();

        //     // Find an attacking move that decides the game.
        //     'attacking:
        //     for attacking_move in attacking_moves {
        //         let mut attacking_game = defending_game.clone();

        //         attacking_game.append(attacking_move.clone()).unwrap();

        //         let outcome = attacking_game.get_outcome();

        //         if let Some(decisive_outcome) = decisive_outcome.as_ref() {
        //             if decisive_outcome.as_ref() == outcome {
        //                 continue 'defending;
        //             } else {
        //                 continue 'attacking;
        //             }
        //         } else {
        //             if let outcome @ Some(Outcome::Decisive { .. }) = outcome {
        //                 decisive_outcome = Some(outcome.cloned());
        //                 continue 'defending;
        //             } else {
        //                 continue 'attacking;
        //             }
        //         }
        //     }

        //     println!("Defending move: {:?}", defending_move);

        //     // No decisive attacking move found for the current defending move,
        //     // thus the game does not have a certain outcome within 2 moves yet.
        //     decisive_outcome = None;
        //     break 'defending;
        // }

        // dbg!(&decisive_outcome);

        // if let Some(decisive_outcome) = decisive_outcome {
        //     return decisive_outcome;
        // }

        if game.available_moves().moves().count() == 0 {
            // TODO: stalemate detection
            return Some(Outcome::Decisive {
                winner: (game.move_log().current_state().current_player_index() + 1) % game.rules().players().get(),
            });
        }

        self.inner.evaluate(game)
    }

    fn is_move_valid(&self, current_state: &Game<G>, mv: &Move<G>) -> bool {
        let mut defending_game = current_state.clone_with_victory_conditions(Box::new(self.inner.clone()));

        // Play the defending move of the current player.
        defending_game.append_unchecked(mv.clone());

        // Find an attacking move that would decide the game.
        for attacking_move in defending_game.available_moves().moves() {
            let mut attacking_game = defending_game.clone();

            attacking_game.append_unchecked(attacking_move.clone());

            let outcome = attacking_game.get_outcome();

            if let Some(Outcome::Decisive { winner }) = outcome {
                if *winner != current_state.move_log().current_state().current_player_index() {
                    // This defending move is invalid, because it can be countered by an attacking move
                    // that decides the game.
                    return false;
                }
            }
        };

        true
    }
}

#[derive(Clone, Debug, Copy)]
pub enum RoyalVictoryType {
    Absolute,
    Extinction,
}

#[derive(Clone, Debug, Copy)]
enum Quantifier {
    All,
    Any,
}

#[derive(Clone, Debug)]
pub struct RoyalVictoryConditions {
    /// (Player, Piece Definition) -> Min Count
    initial_piece_count_per_player: HashMap<(usize, usize), usize>,
    min_piece_count_per_player: HashMap<(usize, usize), usize>,
    loss_when_insufficient_count_by: Quantifier,
}

impl RoyalVictoryConditions {
    pub fn new<G: BoardGeometry>(ty: RoyalVictoryType, initial_state: &GameState<G>) -> Self {
        let initial_piece_count_per_player = initial_state.pieces.values()
            .map(|piece| (piece.owner(), piece.definition_index()))
            .fold(HashMap::new(), |mut acc, key| {
                match acc.entry(key) {
                    Entry::Vacant(entry) => {
                        entry.insert(1);
                    },
                    Entry::Occupied(ref mut entry) => {
                        *entry.get_mut() += 1;
                    },
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

    pub fn with_min_piece_count(mut self, player_index: usize, piece_definition_index: usize, min_piece_count: usize) -> Self {
        if min_piece_count > 0 {
            self.min_piece_count_per_player.insert((player_index, piece_definition_index), min_piece_count);
        } else {
            self.min_piece_count_per_player.remove(&(player_index, piece_definition_index));
        }

        self
    }

    pub fn with_max_takes_of_piece(self, player_index: usize, piece_definition_index: usize, max_takes: usize) -> Self {
        if let Some(initial_count) = self.initial_piece_count_per_player.get(&(player_index, piece_definition_index)).cloned() {
            self.with_max_takes_of_piece(player_index, piece_definition_index, initial_count.saturating_sub(max_takes))
        } else {
            self
        }
    }
}

impl<G: BoardGeometry> VictoryConditions<G> for RoyalVictoryConditions {
    fn evaluate(&self, game: &Game<G>) -> Option<Outcome> {
        let state = game.move_log().current_state();
        let rules = game.rules();
        // Initialize piece counts with all piece definitions and counts of 0
        let piece_counts_per_player = (0..game.rules().piece_set().definitions().len()).into_iter()
            .flat_map(|definition_index| {
                (0..game.rules().players().get()).into_iter()
                    .map(move |player_index| ((player_index, definition_index), 0))
            }).collect::<HashMap<(usize, usize), usize>>();
        let piece_counts_per_player = state.pieces.values()
            .map(|piece| (piece.owner(), piece.definition_index()))
            .fold(piece_counts_per_player, |mut acc, key| {
                *acc.get_mut(&key).unwrap() += 1;

                acc
            });
        let initial_player_evaluation = match self.loss_when_insufficient_count_by {
            Quantifier::All => true,
            Quantifier::Any => false,
        };
        let initial_player_evaluations = (0..rules.players().get()).into_iter()
            .map(|player| (player, initial_player_evaluation))
            .collect::<HashMap<usize, bool>>();

        let players_alive: Vec<usize> = self.min_piece_count_per_player.iter()
            .map(move |(&(player, piece), min_count)| {
                let insufficient_count = piece_counts_per_player.get(&(player, piece))
                    .map(|count| *count < *min_count)
                    .unwrap_or(false);

                (player, insufficient_count)
            })
            .fold(initial_player_evaluations, |mut acc, (player, insufficient_count)| {
                let loss = acc.get_mut(&player).unwrap();

                match self.loss_when_insufficient_count_by {
                    Quantifier::All => *loss &= insufficient_count,
                    Quantifier::Any => *loss |= insufficient_count,
                }

                acc
            })
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

    fn is_move_valid(&self, current_state: &Game<G>, mv: &Move<G>) -> bool {
        let current_player = current_state.move_log().current_state().current_player_index();
        let mut game = current_state.clone();

        game.append_unchecked_without_evaluation(mv.clone());

        // Do not let the player make a move that result in them lose the game.
        if let Some(Outcome::Decisive { winner }) = game.get_outcome() {
            if *winner != current_player {
                return false;
            }
        }

        true
    }
}

#[derive(Debug, Clone)]
pub struct GameRules<G: BoardGeometry> {
    pub(crate) board: Board<G>,
    pub(crate) piece_set: PieceSet<G>,
    pub(crate) players: NonZeroUsize,
    pub(crate) victory_conditions: Box<dyn VictoryConditions<G>>,
}

impl<G: BoardGeometry> GameRules<G> {
    pub fn board(&self) -> &Board<G> {
        &self.board
    }

    pub fn piece_set(&self) -> &PieceSet<G> {
        &self.piece_set
    }

    pub fn players(&self) -> &NonZeroUsize {
        &self.players
    }

}

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct GameState<G: BoardGeometry> {
    pub pieces: HashMap<<G as BoardGeometryExt>::Tile, Piece<G>>,
    pub currently_playing_player_index: usize,
}

impl<G: BoardGeometry> GameState<G> {
    pub fn tile(&self, board: &Board<G>, tile: <G as BoardGeometryExt>::Tile) -> Option<TileRef<'_, G>> {
        board.tiles().contains(tile).then(move || {
            TileRef {
                tile,
                pieces: &self.pieces,
            }
        })
    }

    pub fn tile_mut(&mut self, board: &Board<G>, tile: <G as BoardGeometryExt>::Tile) -> Option<TileRefMut<'_, G>> {
        board.tiles().contains(tile).then(move || {
            TileRefMut {
                tile,
                pieces: &mut self.pieces,
            }
        })
    }

    pub fn apply(self, delta: GameStateDelta<G>) -> Self {
        delta.apply_to(self)
    }

    pub fn current_player_index(&self) -> usize {
        self.currently_playing_player_index
    }
}

impl<'a, G: BoardGeometry> Sub for &'a GameState<G> {
    type Output = GameStateDelta<G>;

    fn sub(self, rhs: Self) -> Self::Output {
        let mut delta = GameStateDelta::with_next_player(self.currently_playing_player_index);

        for (key, lhs_value) in &self.pieces {
            if rhs.pieces.get(key).map(|rhs_value| rhs_value != lhs_value).unwrap_or(true) {
                delta.affected_pieces.insert(*key, Some(lhs_value.clone()));
            }
        }

        for (key, _rhs_value) in &rhs.pieces {
            if self.pieces.get(key).is_none() {
                delta.affected_pieces.insert(*key, None);
            }
        }

        delta
    }
}

/// A move is made up of a normalized state delta and a final tile which is used for selecting this
/// move in UI.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Move<G: BoardGeometry> {
    delta: ReversibleGameStateDelta<G>,
    final_tile: <G as BoardGeometryExt>::Tile,
}

impl<G: BoardGeometry> Move<G> {
    pub fn from(
        original_state: &GameState<G>,
        delta: GameStateDelta<G>,
        final_tile: <G as BoardGeometryExt>::Tile,
    ) -> Self {
        Self {
            delta: delta.normalize(original_state),
            final_tile,
        }
    }

    pub fn into_delta(self) -> ReversibleGameStateDelta<G> {
        self.delta
    }

    pub fn delta(&self) -> &ReversibleGameStateDelta<G> {
        &self.delta
    }

    pub fn final_tile(&self) -> &<G as BoardGeometryExt>::Tile {
        &self.final_tile
    }
}

/// Inner fields not accessible through a mutable reference to keep consistency.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ReversibleGameStateDelta<G: BoardGeometry> {
    forward: GameStateDelta<G>,
    backward: GameStateDelta<G>,
}

impl<G: BoardGeometry> ReversibleGameStateDelta<G> {
    pub fn into_forward(self) -> GameStateDelta<G> {
        self.forward
    }

    pub fn forward(&self) -> &GameStateDelta<G> {
        &self.forward
    }

    pub fn backward(&self) -> &GameStateDelta<G> {
        &self.backward
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct GameStateDelta<G: BoardGeometry> {
    affected_pieces: BTreeMap<<G as BoardGeometryExt>::Tile, Option<Piece<G>>>,
    next_player: usize,
}

impl<G: BoardGeometry> GameStateDelta<G> {
    pub fn with_next_player(next_player: usize) -> Self {
        Self {
            affected_pieces: Default::default(),
            next_player,
        }
    }

    pub fn set(&mut self, tile: <G as BoardGeometryExt>::Tile, piece: Option<Piece<G>>) {
        self.affected_pieces.insert(tile, piece);
    }

    pub fn unset(&mut self, tile: <G as BoardGeometryExt>::Tile) {
        self.affected_pieces.remove(&tile);
    }

    pub fn next_player(&self) -> usize {
        self.next_player
    }

    pub fn iter(&self) -> impl Iterator<Item=(&'_ <G as BoardGeometryExt>::Tile, &'_ Option<Piece<G>>)> {
        self.affected_pieces.iter()
    }

    /// Returns `None` if `tile` is not affected.
    /// Returns `Some(None)` if `tile` is emptied.
    /// Returns `Some(Some(piece))` if `piece` is placed on `tile`.
    pub fn get(&self, tile: <G as BoardGeometryExt>::Tile) -> Option<Option<&Piece<G>>> {
        self.affected_pieces.get(&tile).map(|piece| piece.as_ref())
    }

    /// Removes ineffective actions.
    /// For example, if the delta changes 
    pub fn normalize(self, state: &GameState<G>) -> ReversibleGameStateDelta<G> {
        ReversibleGameStateDelta {
            forward: &state.clone().apply(self.clone()) - state,
            backward: state - &state.clone().apply(self),
        }
    }

    pub fn apply_to(self, mut state: GameState<G>) -> GameState<G> {
        for (tile, piece_or_none) in self.affected_pieces {
            if let Some(piece) = piece_or_none {
                state.pieces.insert(tile, piece);
            } else {
                state.pieces.remove(&tile);
            }
        }

        state.currently_playing_player_index = self.next_player;

        state
    }

    pub fn subset_of(&self, rhs: &Self) -> bool {
        self.next_player == rhs.next_player
            && self.affected_pieces.iter().all(|(tile, piece)| {
                rhs.affected_pieces.get(tile)
                    .map(|expected_piece| piece == expected_piece)
                    .unwrap_or(false)
            })
    }

    pub fn superset_of(&self, rhs: &Self) -> bool {
        rhs.subset_of(self)
    }
}

#[derive(Clone)]
pub struct TileRef<'a, G: BoardGeometry> {
    tile: <G as BoardGeometryExt>::Tile,
    pieces: &'a HashMap<<G as BoardGeometryExt>::Tile, Piece<G>>,
}

impl<G: BoardGeometry> TileRef<'_, G> {
    pub fn get_piece(&self) -> Option<&Piece<G>> {
        self.pieces.get(&self.tile)
    }
}

pub struct TileRefMut<'a, G: BoardGeometry> {
    tile: <G as BoardGeometryExt>::Tile,
    pieces: &'a mut HashMap<<G as BoardGeometryExt>::Tile, Piece<G>>,
}

impl<G: BoardGeometry> TileRefMut<'_, G> {
    pub fn get_piece(&self) -> Option<&Piece<G>> {
        self.pieces.get(&self.tile)
    }

    pub fn set_piece(&mut self, piece: Option<Piece<G>>) {
        if let Some(piece) = piece {
            self.pieces.insert(self.tile, piece);
        } else {
            self.pieces.remove(&self.tile);
        }
    }
}
