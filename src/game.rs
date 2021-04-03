use std::marker::PhantomData;
use std::fmt::Debug;
use std::sync::Arc;
use std::ops::Sub;
use std::num::NonZeroUsize;
use std::collections::{HashMap, VecDeque, BTreeSet, BTreeMap, HashSet, hash_map::Entry};
use fxhash::{FxHashMap, FxHashSet};
use dyn_clone::DynClone;
use replace_with::replace_with_or_default;
use hibitset::BitSet;
use crate::*;

pub type PlayerIndex = u8;

// TODO: Use an immutable list instead of `Vec` to improve cloning performance.
//       Either wrap `initial_state` in an `Arc` or get rid of it completely, as
//       it can always be derived from `current_state` and `moves`, which are reversible.
/// The list of all moves played during a [`Game`].
#[derive(Clone, Debug)]
pub struct MoveLog<G: BoardGeometry> {
    pub(crate) current_state: GameState<G>,
    /// A list of normalized deltas.
    /// Applying all deltas in the given order to `initial_state` produces `current_state`.
    pub(crate) moves: im::Vector<ReversibleGameStateDelta<G>>,
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
            current_state: initial_state,
            moves: Default::default(),
        }
    }

    pub fn len(&self) -> usize {
        self.moves.len()
    }

    pub fn append(&mut self, delta: ReversibleGameStateDelta<G>) {
        let previous_game_state = self.current_state.clone();

        self.moves.push_back(delta.clone());
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

    pub fn current_state(&self) -> &GameState<G> {
        &self.current_state
    }

    pub fn moves(&self) -> impl Iterator<Item=&ReversibleGameStateDelta<G>> + DoubleEndedIterator {
        self.moves.iter()
    }
}

#[derive(Default, Clone, PartialEq, Eq, Debug)]
pub struct AvailableMoves<G: BoardGeometry> {
    pub(crate) moves_from: FxHashMap<<G as BoardGeometryExt>::Tile, FxHashSet<Move<G>>>,
    pub(crate) moves: FxHashSet<Move<G>>,
    pub(crate) deltas: FxHashSet<ReversibleGameStateDelta<G>>,
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

/// A [`Game`] is made up of rules and its state.
/// This is the main type to validate/evaluate/simulate games with.
#[derive(Clone, Debug)]
pub struct Game<G: BoardGeometry> {
    pub(crate) rules: Arc<GameRules<G>>,
    pub(crate) move_log: MoveLog<G>,
    pub(crate) available_moves: Arc<AvailableMoves<G>>,
    pub(crate) outcome: Option<Outcome>,
}

impl<G: BoardGeometry> Game<G> {
    // TODO: Validate that arguments are compatible.
    /// Creates a new game with the provided `rules` and the `initial_state` corresponding to those
    /// rules.
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

    pub fn reset_with_state(&mut self, initial_state: GameState<G>) {
        self.move_log = MoveLog::new(initial_state);

        self.evaluate();
    }

    /// Clone the game with altered victory conditions.
    fn clone_with_victory_conditions(&self, victory_conditions: Box<dyn VictoryCondition<G>>) -> Self {
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

    /// If the provided `delta` corresponds to a legal move, plays that move.
    /// Otherwise, results in an `Err(_)`.
    #[must_use = "The move may not be available."]
    pub fn append_delta(&mut self, delta: ReversibleGameStateDelta<G>) -> Result<(), ()> {
        if self.outcome.is_some() {
            return Err(());
        }

        self.available_moves().is_delta_available(&delta).then(|| {
            self.append_delta_unchecked(delta)
        }).ok_or(())
    }

    /// Appends a delta without checking whether that delta is available as a legal move.
    fn append_delta_unchecked(&mut self, delta: ReversibleGameStateDelta<G>) {
        self.append_delta_unchecked_without_evaluation(delta);
        self.evaluate();
    }

    /// Appends a delta without checking whether that delta is available as a legal move.
    /// Does not evaluate the outcome of the game nor does it generate legal moves.
    fn append_delta_unchecked_without_evaluation(&mut self, delta: ReversibleGameStateDelta<G>) {
        self.move_log.append(delta);
    }

    /// See [`Game::append_delta_unchecked_without_evaluation`].
    fn normalize_and_append_delta_unchecked_without_evaluation(&mut self, delta: GameStateDelta<G>, move_type: MoveType) {
        let normalized = delta.normalize(&self.move_log.current_state, self.rules().board(), move_type);

        self.append_delta_unchecked_without_evaluation(normalized)
    }

    /// If the provided `delta`, normalized, corresponds to a legal move, plays that move.
    /// Otherwise, results in an `Err(_)`.
    #[must_use = "The move may not be available."]
    fn normalize_and_append_delta(&mut self, delta: GameStateDelta<G>, move_type: MoveType) -> Result<(), ()> {
        let normalized = delta.normalize(&self.move_log.current_state, self.rules().board(), move_type);

        self.append_delta(normalized)
    }

    /// If the provided move is legal (is an element of [`Game::available_moves`]), plays that
    /// move.
    /// Otherwise, results in an `Err(_)`.
    #[must_use = "The move may not be available."]
    pub fn append(&mut self, mv: Move<G>) -> Result<(), ()> {
        if self.outcome.is_some() {
            return Err(());
        }

        self.available_moves().is_move_available(&mv).then(|| {
            self.append_unchecked(mv);
        }).ok_or(())
    }

    /// Appends a move without checking whether that move is legal (is an element of [`Game::available_moves`]).
    fn append_unchecked(&mut self, mv: Move<G>) {
        self.append_unchecked_without_evaluation(mv);
        self.evaluate();
    }

    // FIXME: Should we make this compile-time safe?
    // It would be possible to encode whether the game is evaluated using a generic boolean
    // parameter.
    /// Appends a move without checking whether that move is legal (is an element of [`Game::available_moves`]).
    /// Does not evaluate the outcome of the game nor does it generate legal moves.
    #[deprecated = "safety hazard -- no evaluation is performed"]
    pub fn append_unchecked_without_evaluation(&mut self, mv: Move<G>) {
        self.move_log.append(mv.delta);
    }

    /// Returns the piece at tile `tile` on the board `before_moves` moves ago.
    pub(crate) fn past_tile_piece(&self, before_moves: usize, tile: <G as BoardGeometryExt>::Tile) -> Result<Option<&Piece<G>>, PastTileError> {
        if before_moves > self.move_log.moves.len() {
            return Err(PastTileError::PrecedingStart);
        }

        if !self.rules.board.tiles.contains(tile) {
            return Err(PastTileError::MissingTile);
        }

        if before_moves > 0 {
            let moves_played_since = self.move_log.moves.iter().rev().take(before_moves);

            for mv in moves_played_since {
                if let Some(piece) = mv.backward.affected_pieces.get(&tile) {
                    return Ok(piece.as_ref());
                }
            }
        }

        Ok(self.move_log.current_state.pieces.get(&tile))
    }

    /// Returns whether the flag `flag` was set at tile `tile` on the board `before_moves` moves ago.
    pub(crate) fn past_tile_flag(&self, before_moves: usize, tile: <G as BoardGeometryExt>::Tile, flag: u32) -> Result<bool, PastTileError> {
        if before_moves > self.move_log.moves.len() {
            return Err(PastTileError::PrecedingStart);
        }

        if !self.rules.board.tiles.contains(tile) {
            return Err(PastTileError::MissingTile);
        }

        if before_moves > 0 {
            let moves_played_since = self.move_log.moves.iter().rev().take(before_moves);

            for mv in moves_played_since {
                if let Some(affected_flags) = mv.backward.affected_flags.get(&tile) {
                    if let Some(value) = affected_flags.get(&flag) {
                        return Ok(*value);
                    }
                }
            }
        }

        Ok(self.move_log.current_state.flags.contains_flag(tile, flag))
    }

    pub fn evaluate(&mut self) {
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
        let mut available_moves = AvailableMoves::empty();
        let current_player = self.move_log.current_state.current_player_index();

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

    // TODO: Cache symmetrical deltas for non-chiral pieces.
    /// Executes the piece definition's state machine to generate moves from the piece on the given
    /// `tile`.
    /// The state machine is executed using breadth-first search.
    fn evaluate_available_moves_from_tile(&self, tile: <G as BoardGeometryExt>::Tile) -> Result<FxHashSet<Move<G>>, ()> {
        /// An item within the BFS queue.
        #[derive(Clone, Hash, Eq, PartialEq)]
        struct QueueItem<G2: BoardGeometry> {
            tile: <G2 as BoardGeometryExt>::Tile,
            axis_permutation: AxisPermutation<G2>,
            delta: GameStateDelta<G2>,
            visited_marked_states: im::OrdSet<usize>,
            state_index: usize,
        }

        let current_player = self.move_log.current_state.current_player_index();
        // TODO: Generalize: The next player should be determined according to the customizable
        // rules of the game.
        let next_player = (current_player + 1) % self.rules.players.get() as PlayerIndex;
        let move_index = self.move_log().len();

        // Get the piece definition to execute the state machine of.
        let (definition, definition_index, debug) = {
            let tile_ref = self.move_log.current_state.tile(&self.rules.board, tile).ok_or(())?;
            let piece = tile_ref.get_piece().ok_or(())?.clone();

            // If the currently playing player does not own the piece on this tile, it cannot be
            // moved during this turn.
            if current_player != piece.owner {
                return Ok(HashSet::default());
            }

            let definition_index = piece.definition_index();
            let definition = piece.get_definition(&self.rules.piece_set);
            let debug = definition.debug;

            (definition, definition_index, debug)
        };

        // Moves generated by the state machine; to be checked for validity by
        // `VictoryCondition`.
        let mut potential_moves = BTreeSet::new();

        // The queue for BFS.
        let mut queue = VecDeque::<QueueItem<G>>::new();

        // Cache to prevent processing the same `QueueItem` within BFS multiple times.
        // If the cache contains the `QueueItem`, it has already been processed.
        let mut cache = HashSet::<QueueItem<G>>::new();

        // Fill the BFS queue with initial states.
        for initial_state_index in &*definition.initial_states {
            queue.push_back(QueueItem {
                tile,
                axis_permutation: Default::default(),
                delta: GameStateDelta::with_next_player(next_player),
                visited_marked_states: Default::default(),
                state_index: *initial_state_index,
            });
        }

        // BFS
        while let Some(queue_item) = queue.pop_front() {
            // Prevent processing already processed `QueueItem`s.
            if cache.contains(&queue_item) {
                continue;
            }

            cache.insert(queue_item.clone());

            let QueueItem { tile, axis_permutation, mut delta, visited_marked_states, state_index } = queue_item;
            let state = &definition.states[state_index];

            let mut move_type = MoveType {
                definition: definition_index,
                final_state: state_index,
                visited_marked_states,
            };

            // The current game (self) with the applied `GameStateDelta` of the current `QueueItem`.
            let game = {
                let mut game = self.clone();
                let mut partial_delta = delta.clone();
                partial_delta.next_player = current_player;

                game.normalize_and_append_delta_unchecked_without_evaluation(partial_delta, move_type.clone());

                game
            };

            if debug {
                println!("State before evaluating condition:\n{game}", game=G::print(&game));
                println!("Pieces before evaluating condition:");

                for (tile, piece) in &game.move_log().current_state().pieces {
                    println!("{tile:?}: {piece:?}");
                }
            }

            let game_state = game.move_log().current_state();
            let piece = game_state.tile(&self.rules.board, tile).ok_or(())?.get_piece().ok_or(())?.clone();
            let isometry = Isometry::from(axis_permutation.clone() * piece.transformation.clone()) * Isometry::translation(tile);

            let moves: Vec<(_, _, _)> = match state.action.clone() {
                Action::Move { condition, actions, move_choices } => {
                    // If state conditions are not satisfied, skip the BFS branch.
                    //
                    // TODO: Should `Action::Symmetry` have conditions as well? Consider creating a
                    //       `ConditionalAction` type to store conditions and the `Action`.
                    if !condition.evaluate(&game, &isometry, debug) {
                        continue;
                    }

                    for action in actions {
                        match action {
                            ActionEnum::SetTile { target, piece: piece_definition, } => {
                                let target = isometry.apply(target);
                                let new_piece = piece_definition.map(|piece_definition| Piece {
                                    transformation: piece.transformation.clone(),
                                    definition: piece_definition as u16,
                                    owner: piece.owner,
                                    affecting_moves: Default::default(),
                                });

                                delta.set(target, new_piece, move_index);
                            },
                            ActionEnum::CopyTile { source, target } => {
                                let source = isometry.apply(source);
                                let target = isometry.apply(target);
                                let piece = game_state.tile(&game.rules.board, source).and_then(|tile| tile.get_piece().cloned());

                                delta.set(target, piece, move_index);
                            },
                        }
                    }

                    move_choices.into_iter().filter_map(|move_choice| {
                        let piece = delta.affected_pieces.get(&tile).cloned().unwrap_or_else(||
                            game_state.tile(&game.rules.board, tile).and_then(|tile| tile.get_piece().cloned()));
                        let move_choice = isometry.apply(move_choice);
                        let mut delta = delta.clone();

                        if tile != move_choice {
                            if game_state.tile(&game.rules.board, move_choice).is_none() {
                                // Target tile is missing and its coordinates differ from source tile's.
                                // This would result in a piece being moved off the board. Since
                                // that doesn't make much sense, we cancel such moves.
                                return None;
                            }

                            delta.set(tile, None, move_index);
                        }

                        delta.set(move_choice, piece, move_index);

                        Some((move_choice, axis_permutation.clone(), delta))
                    }).collect()
                },
                Action::Symmetry { symmetries } => {
                    symmetries.into_iter().map(|axis_permutation| {
                        (tile, axis_permutation, delta.clone())
                    }).collect()
                },
            };

            // Add visited marked state for successor states.
            if state.is_marked {
                move_type.visited_marked_states.insert(state_index);
            }

            // If the current state is final, add the moves to potential moves.
            if state.is_final {
                for (tile, _, delta) in &moves {
                    let mv = Move::from(
                        &self.move_log.current_state,
                        self.rules().board(),
                        delta.clone(),
                        *tile,
                        move_type.clone(),
                    );

                    potential_moves.insert(mv);
                }
            }

            // Add successor states for each delta to the BFS queue.
            for successor_state_index in &*state.successor_indices {
                for (next_tile, axis_permutation, delta) in &moves {
                    queue.push_back(QueueItem {
                        tile: *next_tile,
                        axis_permutation: axis_permutation.clone(),
                        delta: delta.clone(),
                        visited_marked_states: move_type.visited_marked_states.clone(),
                        state_index: *successor_state_index,
                    });
                }
            }
        }

        if debug {
            println!("Pseudo-legal moves:");

            for mv in &potential_moves {
                let mut game = self.clone();

                game.append_unchecked_without_evaluation(mv.clone());
                println!("Pseudo-legal move:\n{game}", game=G::print(&game));
            } 
        }

        // Retain only valid moves, according to the game's `VictoryCondition`.
        let valid_moves: FxHashSet<_> = potential_moves.into_iter()
            .filter(|mv| {
                self.rules().victory_conditions.is_move_valid(self, mv)
            })
            .collect();

        Ok(valid_moves)
    }

    pub fn move_log(&self) -> &MoveLog<G> {
        &self.move_log
    }

    pub fn rules(&self) -> &GameRules<G> {
        &self.rules
    }
}

/// The outcome of a finished [`Game`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Outcome {
    /// The winner of the game has been decided.
    Decisive {
        winner: PlayerIndex,
    },
    /// The game resulted in a draw.
    Draw,
}

/// [`VictoryCondition`]s evaluate the outcome of a game after every played move.
/// They also decide which pseudo-legal moves are legal, or, in other words, rejects certain
/// generated moves.
pub trait VictoryCondition<G: BoardGeometry>: Send + Sync + Debug + DynClone {
    /// Determines the outcome of a game at the current state.
    /// Not to be called directly, use [`Game::get_outcome`] instead.
    fn evaluate(&self, game: &Game<G>) -> Option<Outcome>;

    /// Determines whether a pseudo-legal move is legal.
    fn is_move_valid(&self, current_state: &Game<G>, mv: &Move<G>) -> bool;
}

dyn_clone::clone_trait_object!(<G> VictoryCondition<G> where G: BoardGeometry);

/// A [`VictoryCondition`] with no possible victories and no illegal moves.
/// The game continues until all but one players concede.
#[derive(Debug, Clone)]
pub struct NoVictoryCondition;

impl<G: BoardGeometry> VictoryCondition<G> for NoVictoryCondition {
    fn evaluate(&self, _game: &Game<G>) -> Option<Outcome> {
        None
    }

    fn is_move_valid(&self, _current_state: &Game<G>, _mv: &Move<G>) -> bool {
        true
    }
}

/// A [`VictoryCondition`] which predicts an inevitable victory 2 turns ahead and makes
/// moves which would result in a loss in the next turn illegal.
///
/// A combination of `PredictiveVictoryCondition` and `RoyalVictoryCondition` can provide
/// victory conditions typical for international chess.
#[derive(Debug, Clone)]
pub struct PredictiveVictoryCondition<G: BoardGeometry, C: VictoryCondition<G>> {
    inner: C,
    __marker: PhantomData<G>,
}

impl<G, C> PredictiveVictoryCondition<G, C>
where
    G: BoardGeometry + Send + Sync + Debug + Clone,
    C: Send + Sync + Debug + Clone + VictoryCondition<G>,
{
    pub fn new(inner: C) -> Self {
        Self {
            inner,
            __marker: Default::default(),
        }
    }
}

impl<G, C> VictoryCondition<G> for PredictiveVictoryCondition<G, C>
where
    G: BoardGeometry + Send + Sync + Debug + Clone,
    C: Send + Sync + Debug + Clone + VictoryCondition<G> + 'static,
{
    fn evaluate(&self, game: &Game<G>) -> Option<Outcome> {
        if game.available_moves().moves().count() == 0 {
            // TODO: stalemate detection
            return Some(Outcome::Decisive {
                winner: (game.move_log().current_state().current_player_index() + 1) % game.rules().players().get() as PlayerIndex,
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
        let initial_piece_count_per_player = initial_state.pieces.values()
            .map(|piece| (piece.owner(), piece.definition_index()))
            .fold(HashMap::default(), |mut acc, key| {
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

    pub fn with_min_piece_count(mut self, player_index: PlayerIndex, piece_definition_index: PieceDefinitionIndex, min_piece_count: usize) -> Self {
        if min_piece_count > 0 {
            self.min_piece_count_per_player.insert((player_index, piece_definition_index), min_piece_count);
        } else {
            self.min_piece_count_per_player.remove(&(player_index, piece_definition_index));
        }

        self
    }

    pub fn with_max_takes_of_piece(self, player_index: PlayerIndex, piece_definition_index: PieceDefinitionIndex, max_takes: usize) -> Self {
        if let Some(initial_count) = self.initial_piece_count_per_player.get(&(player_index, piece_definition_index)).cloned() {
            self.with_max_takes_of_piece(player_index, piece_definition_index, initial_count.saturating_sub(max_takes))
        } else {
            self
        }
    }
}

impl<G: BoardGeometry> VictoryCondition<G> for RoyalVictoryCondition {
    fn evaluate(&self, game: &Game<G>) -> Option<Outcome> {
        let state = game.move_log().current_state();
        let rules = game.rules();
        // Initialize piece counts with all piece definitions and counts of 0
        let piece_counts_per_player = (0..game.rules().piece_set().definitions().len()).into_iter()
            .flat_map(|definition_index| {
                (0..game.rules().players().get()).into_iter()
                    .map(move |player_index| ((player_index as PlayerIndex, definition_index as PieceDefinitionIndex), 0))
            }).collect::<FxHashMap<(PlayerIndex, PieceDefinitionIndex), usize>>();
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
            .map(|player| (player as PlayerIndex, initial_player_evaluation))
            .collect::<FxHashMap<PlayerIndex, bool>>();

        let players_alive: Vec<PlayerIndex> = self.min_piece_count_per_player.iter()
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

/// Game rules define the constraints of the game and the victory conditions.
/// Game rules do not change throughout a game.
#[derive(Debug, Clone)]
pub struct GameRules<G: BoardGeometry> {
    pub(crate) board: Board<G>,
    pub(crate) piece_set: PieceSet<G>,
    pub(crate) players: NonZeroUsize,
    pub(crate) victory_conditions: Box<dyn VictoryCondition<G>>,
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

pub type Pieces<G> = FxHashMap<<G as BoardGeometryExt>::Tile, Piece<G>>;

#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct Flags<G: BoardGeometry> {
    map: FxHashMap<<G as BoardGeometryExt>::Tile, BitSet>,
}

impl<G: BoardGeometry> Flags<G> {
    pub fn get_flags(&self, tile: <G as BoardGeometryExt>::Tile) -> Option<&BitSet> {
        self.map.get(&tile)
    }

    pub fn contains_flag(&self, tile: <G as BoardGeometryExt>::Tile, flag: u32) -> bool {
        self.map.get(&tile).map(|flags| flags.contains(flag)).unwrap_or(false)
    }

    pub fn set_flag(&mut self, tile: <G as BoardGeometryExt>::Tile, flag: u32, value: bool) {
        if value {
            self.map.entry(tile).or_insert_with(|| BitSet::new()).add(flag);
        } else {
            let flags_empty = if let Some(flags) = self.map.get_mut(&tile) {
                flags.remove(flag) && *flags == BitSet::new()
            } else {
                false
            };

            if flags_empty {
                self.map.remove(&tile);
            }
        }
    }
}

/// A `GameState` characterizes the state of the board after zero or more moves.
/// It tracks the pieces on the board, the tiles' flags and the index of the player whose turn it
/// is.
#[derive(Clone, Default, Debug, PartialEq, Eq)]
pub struct GameState<G: BoardGeometry> {
    pub pieces: Pieces<G>,
    pub flags: Flags<G>,
    pub current_player_index: PlayerIndex,
}

impl<G: BoardGeometry> GameState<G> {
    pub fn tile(&self, board: &Board<G>, tile: <G as BoardGeometryExt>::Tile) -> Option<TileRef<'_, G>> {
        board.tiles().contains(tile).then(move || {
            TileRef {
                tile,
                pieces: &self.pieces,
                flags: &self.flags,
            }
        })
    }

    pub fn tile_mut(&mut self, board: &Board<G>, tile: <G as BoardGeometryExt>::Tile) -> Option<TileRefMut<'_, G>> {
        board.tiles().contains(tile).then(move || {
            TileRefMut {
                tile,
                pieces: &mut self.pieces,
                flags: &mut self.flags,
            }
        })
    }

    pub fn apply(self, delta: GameStateDelta<G>) -> Self {
        delta.apply_to(self)
    }

    pub fn current_player_index(&self) -> PlayerIndex {
        self.current_player_index
    }
}

impl<'a, G: BoardGeometry> Sub for &'a GameState<G> {
    type Output = GameStateDelta<G>;

    fn sub(self, rhs: Self) -> Self::Output {
        let mut delta = GameStateDelta::with_next_player(self.current_player_index());

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

        let tiles_with_flags: FxHashSet<_> = self.flags.map.keys().chain(rhs.flags.map.keys()).copied().collect();

        for tile in tiles_with_flags {
            match (self.flags.get_flags(tile), rhs.flags.get_flags(tile)) {
                (Some(lhs_flags), Some(rhs_flags)) => {
                    for add_flag in lhs_flags & !rhs_flags {
                        delta.affected_flags.entry(tile).or_default().insert(add_flag, true);
                    }

                    for remove_flag in !lhs_flags & rhs_flags {
                        delta.affected_flags.entry(tile).or_default().insert(remove_flag, false);
                    }
                },
                (Some(lhs_flags), None) => {
                    delta.affected_flags.entry(tile).or_default()
                        .extend(lhs_flags.into_iter().map(|flag| (flag, true)));
                },
                (None, Some(rhs_flags)) => {
                    delta.affected_flags.entry(tile).or_default()
                        .extend(rhs_flags.into_iter().map(|flag| (flag, false)));
                },
                (None, None) => unreachable!(),
            }
        }

        // let set_flags = self.flags.map & !rhs.flags.map;
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
        board: &Board<G>,
        delta: GameStateDelta<G>,
        final_tile: <G as BoardGeometryExt>::Tile,
        move_type: MoveType,
    ) -> Self {
        Self {
            delta: delta.normalize(original_state, board, move_type),
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

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct MoveType {
    pub definition: PieceDefinitionIndex,
    pub final_state: usize,
    pub visited_marked_states: im::OrdSet<usize>,
}

impl MoveType {
    pub fn has_visited_marked_state(&self, state_index: usize) -> bool {
        self.visited_marked_states.contains(&state_index)
    }
}

/// A [`GameStateDelta`], which is reversible for the [`GameState`] it was created for using
/// [`GameStateDelta::normalize`].
/// For that game state, the following equality holds:
/// `state == delta.backward().apply_to(delta.forward().apply_to(state))`
///
/// Inner fields not accessible through a mutable reference to ensure consistency.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ReversibleGameStateDelta<G: BoardGeometry> {
    forward: GameStateDelta<G>,
    backward: GameStateDelta<G>,
    move_type: MoveType,
}

impl<G: BoardGeometry> ReversibleGameStateDelta<G> {
    pub fn into_forward(self) -> GameStateDelta<G> {
        self.forward
    }

    pub fn move_type(&self) -> &MoveType {
        &self.move_type
    }

    pub fn forward(&self) -> &GameStateDelta<G> {
        &self.forward
    }

    pub fn backward(&self) -> &GameStateDelta<G> {
        &self.backward
    }
}

/// An operation applicable to a [`GameState`], producing an altered [`GameState`].
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct GameStateDelta<G: BoardGeometry> {
    affected_pieces: BTreeMap<<G as BoardGeometryExt>::Tile, Option<Piece<G>>>,
    affected_flags: BTreeMap<<G as BoardGeometryExt>::Tile, BTreeMap<u32, bool>>,
    // TODO: Reconsider whether this is necessary:
    next_player: PlayerIndex,
}

impl<G: BoardGeometry> GameStateDelta<G> {
    pub fn with_next_player(next_player: PlayerIndex) -> Self {
        Self {
            affected_pieces: Default::default(),
            affected_flags: Default::default(),
            next_player,
        }
    }

    pub fn set(&mut self, tile: <G as BoardGeometryExt>::Tile, mut piece: Option<Piece<G>>, move_index: usize) {
        if let Some(piece) = piece.as_mut() {
            piece.push_affecting_move(move_index);
        }

        self.affected_pieces.insert(tile, piece);
    }

    pub fn set_initial(&mut self, tile: <G as BoardGeometryExt>::Tile, mut piece: Option<Piece<G>>) {
        if let Some(piece) = piece.as_mut() {
            piece.make_initial();
        }

        self.affected_pieces.insert(tile, piece);
    }

    pub fn unset(&mut self, tile: <G as BoardGeometryExt>::Tile) {
        self.affected_pieces.remove(&tile);
    }

    pub fn next_player(&self) -> PlayerIndex {
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

    /// Returns `true` if and only if `rhs` affects all pieces and tiles that `self` affects, in
    /// the exact same way.
    ///
    /// In other words, returns `true` if `rhs` is `lhs` and something extra.
    pub fn is_part_of(&self, rhs: &GameStateDelta<G>) -> bool {
        true
            && self.next_player == rhs.next_player
            && self.affected_pieces.len() <= rhs.affected_pieces.len()
            && self.affected_flags.len() <= rhs.affected_flags.len()
            && self.affected_pieces.iter().all(|(tile, piece)| {
                rhs.get(*tile) == Some(piece.as_ref())
            })
            && self.affected_flags.iter().all(|(tile, lhs_flags)| {
                if let Some(rhs_flags) = rhs.affected_flags.get(tile) {
                    lhs_flags.iter().all(|(flag, value)| {
                        rhs_flags.get(flag) == Some(value)
                    })
                } else {
                    false
                }
            })
    }

    /// Removes ineffective actions.
    pub fn normalize(self, state: &GameState<G>, board: &Board<G>, move_type: MoveType) -> ReversibleGameStateDelta<G> {
        #[cfg(debug_assertions)]
        let self_clone = self.clone();

        let mut result = ReversibleGameStateDelta {
            forward: self,
            backward: GameStateDelta::with_next_player(state.current_player_index()),
            move_type,
        };

        // Keep altered pieces only.
        //
        // Note: Using `drain` on a `BTreeMap` is only efficient if the number of removed elements is
        // relatively low. In other cases, it is best to create a new tree from an iterator of
        // key-value pairs.
        result.forward.affected_pieces.retain(|tile, forward_piece| {
            if let Some(current_tile) = state.tile(board, *tile) {
                current_tile.get_piece() != forward_piece.as_ref()
            } else {
                false
            }
        });

        result.backward.affected_pieces = result.forward.affected_pieces.keys().map(|tile| {
            // Unwrap Safety: Only pieces of existing tiles were retained in `forward`.
            let backward_tile = state.tile(board, *tile).unwrap();
            let backward_piece = backward_tile.get_piece().cloned();

            (*tile, backward_piece)
        }).collect();

        // Keep altered flags only.
        result.forward.affected_flags.retain(|tile, forward_flags| {
            if let Some(current_tile) = state.tile(board, *tile) {
                if let Some(current_flags) = current_tile.get_flags() {
                    forward_flags.retain(|flag, value| {
                        current_flags.contains(*flag) != *value
                    });

                    !forward_flags.is_empty()
                } else {
                    false
                }
            } else {
                false
            }
        });

        result.backward.affected_flags = result.forward.affected_flags.clone();

        result.backward.affected_flags.values_mut().for_each(|backward_flags| {
            for value in backward_flags.values_mut() {
                *value ^= true;
            }
        });

        // In debug mode, verify the validity of created `ReversibleGameStateDelta`.
        #[cfg(debug_assertions)]
        debug_assert_eq!(result, ReversibleGameStateDelta {
            forward: &state.clone().apply(self_clone.clone()) - state,
            backward: state - &state.clone().apply(self_clone),
            move_type: result.move_type.clone(),
        });

        result
    }

    pub fn apply_to(self, mut state: GameState<G>) -> GameState<G> {
        for (tile, piece_or_none) in self.affected_pieces {
            if let Some(piece) = piece_or_none {
                state.pieces.insert(tile, piece);
            } else {
                state.pieces.remove(&tile);
            }
        }

        for (tile, affected_flags) in self.affected_flags {
            for (flag, value) in affected_flags {
                state.flags.set_flag(tile, flag, value);
            }
        }

        state.current_player_index = self.next_player;

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

/// A reference to an existing tile.
#[derive(Clone)]
pub struct TileRef<'a, G: BoardGeometry> {
    tile: <G as BoardGeometryExt>::Tile,
    pieces: &'a Pieces<G>,
    flags: &'a Flags<G>,
}

impl<G: BoardGeometry> TileRef<'_, G> {
    pub fn get_piece(&self) -> Option<&Piece<G>> {
        self.pieces.get(&self.tile)
    }

    pub fn get_flags(&self) -> Option<&BitSet> {
        self.flags.get_flags(self.tile)
    }

    pub fn contains_flag(&self, flag: u32) -> bool {
        self.flags.contains_flag(self.tile, flag)
    }
}

/// A mutable reference to an existing tile.
pub struct TileRefMut<'a, G: BoardGeometry> {
    tile: <G as BoardGeometryExt>::Tile,
    pieces: &'a mut Pieces<G>,
    flags: &'a mut Flags<G>,
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

    pub fn get_flags(&self) -> Option<&BitSet> {
        self.flags.get_flags(self.tile)
    }

    pub fn contains_flag(&self, flag: u32) -> bool {
        self.flags.contains_flag(self.tile, flag)
    }

    pub fn set_flag(&mut self, flag: u32, value: bool) {
        self.flags.set_flag(self.tile, flag, value)
    }
}
