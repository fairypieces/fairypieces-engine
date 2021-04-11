use std::marker::PhantomData;
use std::fmt::Debug;
use std::sync::Arc;
use std::ops::Sub;
use std::num::NonZeroUsize;
use std::collections::{HashMap, VecDeque, BTreeSet, BTreeMap, HashSet, hash_map::Entry};
use fxhash::{FxHashMap, FxHashSet};
use replace_with::replace_with_or_default;
use hibitset::BitSet;
use sealed::sealed;
use crate::victory_conditions::*;
use crate::delta::*;
use crate::*;

pub type PlayerIndex = u8;

#[derive(Copy, Clone, Debug)]
pub enum PastTileError {
    /// The tile from the requested move precedes the start of the game.
    PrecedingStart,
    /// The tile is not on the board.
    MissingTile,
}

// TODO: Use an immutable list instead of `Vec` to improve cloning performance.
//       Either wrap `initial_state` in an `Arc` or get rid of it completely, as
//       it can always be derived from `current_state` and `moves`, which are reversible.
/// The list of all moves played during a [`Game`].
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct MoveLog<G: BoardGeometry> {
    pub(crate) current_state: GameState<G>,
    /// A list of normalized deltas.
    /// Applying all deltas in the given order to `initial_state` produces `current_state`.
    pub(crate) moves: im::Vector<ReversibleGameStateDelta<G>>,
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

    /// Implementation safety: This function invalidates moves from `cache`. See `MoveCache`
    /// for further description.
    pub(crate) fn undo(&mut self, cache: &mut MoveCache<G>, len: usize) -> im::Vector<ReversibleGameStateDelta<G>> {
        assert!(len <= self.moves.len(), "Attempted to undo {} moves from a move log with the length {}.", len, self.moves.len());

        // Remove `len` moves from the end
        let new_len = self.moves.len() - len;
        let removed_moves = self.moves.split_off(new_len);
        let mut removed_moves_during_invalidation = Vec::new();

        // Update the current state
        for mv in removed_moves.iter().rev() {
            self.current_state = self.current_state.clone().apply(mv.backward().clone());

            // FIXME: Possible optimization: Do not invalidate moves that are not even cached.
            cache.invalidate(mv, &mut removed_moves_during_invalidation);
        }

        cache.applied_moves = std::cmp::min(cache.applied_moves, new_len);

        removed_moves
    }

    pub fn compute_nth_back(&self, index_back: usize) -> GameState<G> {
        let mut result = self.current_state.clone();

        for mv in self.moves.iter().rev().take(index_back) {
            result = result.apply(mv.backward().clone());
        }

        result
    }

    pub fn compute_nth(&self, index: usize) -> GameState<G> {
        self.compute_nth_back(self.len() - index)
    }

    pub fn append(&mut self, delta: ReversibleGameStateDelta<G>) {
        let previous_game_state = self.current_state.clone();

        self.moves.push_back(delta.clone());
        replace_with_or_default(&mut self.current_state, |current_state| current_state.apply(delta.forward().clone()));

        // Ensure all appended moves are reversible. Ensured using induction.
        debug_assert_eq!(previous_game_state, self.current_state.clone().apply(delta.into_backward()));

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

/// A collection of legal moves that can be played during the current turn.
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

    pub fn from(mut moves_from: FxHashMap<<G as BoardGeometryExt>::Tile, FxHashSet<Move<G>>>, move_index: usize) -> Self {
        // Append the `move_index` to all affected pieces.
        for move_set in moves_from.values_mut() {
            replace_with_or_default(move_set, |move_set| {
                move_set.into_iter().map(|mut mv| {
                    mv.push_affecting_move(move_index);
                    mv
                }).collect()
            });
        }

        let moves: FxHashSet<_> = moves_from.values().flat_map(|move_set| move_set.iter()).cloned().collect();
        let deltas: FxHashSet<_> = moves.iter().map(|mv| mv.delta.clone()).collect();

        Self {
            moves_from,
            moves,
            deltas,
        }
    }

    pub(crate) fn extend(&mut self, piece_tile: <G as BoardGeometryExt>::Tile, moves: &PieceMoves<G>) {
        self.deltas.extend(moves.moves.iter().map(|mv| mv.delta.clone()));
        self.moves_from.entry(piece_tile)
            .or_insert_with(|| Default::default())
            .extend(moves.moves.clone());
        self.moves.extend(moves.moves.iter().cloned());
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

pub trait GameEvaluation<G: BoardGeometry>: crate::util::Sealed + Clone + std::fmt::Debug + Default {}

#[derive(Clone, Debug, Default)]
pub struct NotEvaluated;

impl crate::util::Sealed for NotEvaluated {}
impl<G: BoardGeometry> GameEvaluation<G> for NotEvaluated {}

#[derive(Clone, Debug, Default)]
pub struct Evaluated<G: BoardGeometry> {
    /// Legal moves.
    pub(crate) available_moves: Arc<AvailableMoves<G>>,
    /// The outcome of the game, if it has been determined by the victory conditions.
    pub(crate) outcome: Option<Outcome>,
}

impl<G: BoardGeometry> crate::util::Sealed for Evaluated<G> {}
impl<G: BoardGeometry> GameEvaluation<G> for Evaluated<G> {}

/// A [`Game`] is made up of rules and its state.
/// This is the main type to validate/evaluate/simulate games with.
#[derive(Clone, Debug, Default)]
pub struct Game<G: BoardGeometry, E: GameEvaluation<G> = Evaluated<G>> {
    pub(crate) rules: Arc<GameRules<G>>,
    pub(crate) move_log: MoveLog<G>,
    /// Pseudo-legal moves
    pub(crate) pseudo_legal_moves: Option<Arc<PseudoLegalMoves<G>>>,
    /// Cache of pseudo-legal moves
    pub(crate) move_cache: MoveCache<G>,
    pub(crate) evaluation: E,
}

impl<G: BoardGeometry, E: GameEvaluation<G>> Game<G, E> {
    fn replace_evaluation<R: GameEvaluation<G>>(self, replace: impl FnOnce(E) -> R) -> Game<G, R> {
        Game {
            rules: self.rules,
            move_log: self.move_log,
            pseudo_legal_moves: self.pseudo_legal_moves,
            move_cache: self.move_cache,
            evaluation: (replace)(self.evaluation),
        }
    }

    /// Alter the game's victory conditions
    pub(crate) fn with_victory_conditions(mut self, victory_conditions: Box<dyn VictoryCondition<G>>) -> Game<G, NotEvaluated> {
        Arc::make_mut(&mut self.rules).victory_conditions = victory_conditions;

        self.replace_evaluation(|_| NotEvaluated)
    }

    /// Clone the game with altered victory conditions.
    pub(crate) fn clone_with_victory_conditions(&self, victory_conditions: Box<dyn VictoryCondition<G>>) -> Game<G, NotEvaluated> {
        self.clone().with_victory_conditions(victory_conditions)
    }

    /// Appends a delta without checking whether that delta is available as a legal move.
    pub(crate) fn append_delta_unchecked(self, delta: ReversibleGameStateDelta<G>) -> Game<G, Evaluated<G>> {
        let result = self.append_delta_unchecked_without_evaluation(delta);
        result.evaluate()
    }

    /// Appends a move without checking whether that move is legal (is an element of [`Game::available_moves`]).
    pub(crate) fn append_unchecked(mut self, mv: Move<G>) -> Game<G, Evaluated<G>> {
        let result = self.append_unchecked_without_evaluation(mv);
        result.evaluate()
    }

    /// Appends a delta without checking whether that delta is available as a legal move.
    /// Does not evaluate the outcome of the game nor does it generate legal moves.
    pub(crate) fn append_delta_unchecked_without_evaluation(mut self, delta: ReversibleGameStateDelta<G>) -> Game<G, NotEvaluated> {
        self.move_log.append(delta);
        self.replace_evaluation(|_| NotEvaluated)
    }

    /// Appends a move without checking whether that move is legal (is an element of [`Game::available_moves`]).
    /// Does not evaluate the outcome of the game nor does it generate legal moves.
    pub(crate) fn append_unchecked_without_evaluation(self, mv: Move<G>) -> Game<G, NotEvaluated> {
        self.append_delta_unchecked_without_evaluation(mv.delta)
            .replace_evaluation(|_| NotEvaluated)
    }

    /// See [`Game::append_delta_unchecked_without_evaluation`].
    pub(crate) fn normalize_and_append_delta_unchecked_without_evaluation(self, delta: GameStateDelta<G>, move_type: MoveType) -> Game<G, NotEvaluated> {
        let normalized = delta.normalize(&self.move_log.current_state, self.rules().board(), move_type);

        self.append_delta_unchecked_without_evaluation(normalized)
    }

    pub fn move_log(&self) -> &MoveLog<G> {
        &self.move_log
    }

    pub fn rules(&self) -> &GameRules<G> {
        &self.rules
    }
}

impl<G: BoardGeometry> Game<G, NotEvaluated> {
    pub fn evaluate(mut self) -> Game<G, Evaluated<G>> {
        let mut available_moves = self.generate_available_moves();
        let outcome = self.rules.victory_conditions.evaluate(&self, &available_moves);

        if outcome.is_some() {
            available_moves = AvailableMoves::empty();
        }

        self.replace_evaluation(|_| Evaluated {
            available_moves: Arc::new(available_moves),
            outcome,
        })
    }

    fn generate_available_moves(&mut self) -> AvailableMoves<G> {
        // Removed invalidated cached moves
        if self.move_cache.invalidate_recent(&self.move_log) {
            // A move has been played since the last time this method was called, regenerate
            // pseudo-legal moves.
            self.pseudo_legal_moves = None;
        }

        if self.pseudo_legal_moves.is_none() {
            self.generate_pseudo_legal_moves();
        }

        // Retain only valid moves, according to the game's `VictoryCondition`.
        let move_index = self.move_log().len();
        let mut moves_from = self.pseudo_legal_moves.as_ref().unwrap().as_ref().clone();

        #[cfg(feature = "concurrency")]
        {
            use rayon::prelude::*;

            moves_from = moves_from.into_par_iter()
                .map(|(tile, mut move_set)| {
                    move_set = move_set.into_par_iter()
                        .filter(|mv| {
                            self.rules().victory_conditions.is_move_legal(self, mv.delta())
                        })
                        .map(|mv| {
                            let mut moves = FxHashSet::default();
                            moves.insert(mv);
                            moves
                        })
                        .reduce(
                            || Default::default(),
                            |mut lhs, rhs| {
                                lhs.extend(rhs);
                                lhs
                            },
                        );

                    let mut moves_from = FxHashMap::default();
                    moves_from.insert(tile, move_set);
                    moves_from
                })
                .reduce(
                    || FxHashMap::default(),
                    |mut lhs, rhs| {
                        lhs.extend(rhs);
                        lhs
                    },
                );
        }

        #[cfg(not(feature = "concurrency"))]
        {
            for move_set in moves_from.values_mut() {
                move_set.retain(|mv| {
                    self.rules().victory_conditions.is_move_legal(self, mv.delta())
                });
            }
        }

        AvailableMoves::from(moves_from, move_index)
    }

    /// Stores generated pseudo-legal moves in `Self::pseudo_legal_moves`.
    fn generate_pseudo_legal_moves(&mut self) {
        // Reuse the previous collection, but make sure it is empty.
        let mut moves_from: PseudoLegalMoves<G> = Default::default();

        let current_player = self.move_log.current_state.current_player_index();

        // A parallel implementation, but probably not worth the overhead.
        //
        // let (moves_from, generated_moves) = self.move_log.current_state.pieces
        //     .par_iter()
        //     .filter(|(tile, piece)| piece.owner == current_player)
        //     .map(|(tile, piece)| {
        //         let mut moves_from = PseudoLegalMoves::<G>::default();
        //         let mut generated_moves = Vec::new();

        //         if let Some(cached_moves) = self.move_cache.get_cached_moves_from(*tile) {
        //             #[cfg(debug_assertions)]
        //             if let Ok(Some(generated_moves)) = self.generate_pseudo_legal_moves_from_tile(*tile) {
        //                 if &generated_moves != cached_moves {
        //                     panic!("Cached move discrepancy #{mv} from tile {tile:?}:\n\tcached:    {cached_moves:?}\n\tgenerated: {generated_moves:?}", mv=self.move_log().len());
        //                 }
        //             }

        //             moves_from.insert(*tile, cached_moves.moves.clone());
        //             // available_moves.extend(*tile, cached_moves);
        //         } else if let Ok(Some(current_generated_moves)) = self.generate_pseudo_legal_moves_from_tile(*tile) {
        //             moves_from.insert(*tile, current_generated_moves.moves.clone());
        //             // available_moves.extend(*tile, &generated_moves);
        //             generated_moves.push((*tile, current_generated_moves));
        //         }

        //         (moves_from, generated_moves)
        //     })
        //     .reduce(
        //         || (PseudoLegalMoves::<G>::default(), Vec::new()),
        //         |(mut lhs_moves, mut lhs_generated_moves), (rhs_moves, rhs_generated_moves)| {
        //             lhs_moves.extend(rhs_moves);
        //             lhs_generated_moves.extend(rhs_generated_moves);

        //             (lhs_moves, lhs_generated_moves)
        //         }
        //     );

        // for (tile, generated_moves) in generated_moves {
        //     self.move_cache.cache_moves(tile, generated_moves);
        // }

        for (tile, piece) in &self.move_log.current_state.pieces {
            if piece.owner == current_player {
                if let Some(cached_moves) = self.move_cache.get_cached_moves_from(*tile) {
                    #[cfg(debug_assertions)]
                    if let Ok(Some(generated_moves)) = self.generate_pseudo_legal_moves_from_tile(*tile) {
                        if &generated_moves != cached_moves {
                            panic!("Cached move discrepancy #{mv} from tile {tile:?}:\n\tcached:    {cached_moves:?}\n\tgenerated: {generated_moves:?}", mv=self.move_log().len());
                        }
                    }

                    moves_from.insert(*tile, cached_moves.moves.clone());
                    // available_moves.extend(*tile, cached_moves);
                } else if let Ok(Some(generated_moves)) = self.generate_pseudo_legal_moves_from_tile(*tile) {
                    moves_from.insert(*tile, generated_moves.moves.clone());
                    // available_moves.extend(*tile, &generated_moves);
                    self.move_cache.cache_moves(*tile, generated_moves);
                }
            }
        }

        self.pseudo_legal_moves = Some(Arc::new(moves_from));
    }

    // TODO: Cache symmetrical deltas for non-chiral pieces.
    /// Executes the piece definition's state machine to generate moves from the piece on the given
    /// `tile`.
    /// The state machine is executed using breadth-first search.
    fn generate_pseudo_legal_moves_from_tile(&self, tile: <G as BoardGeometryExt>::Tile) -> Result<Option<PieceMoves<G>>, ()> {
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
                return Ok(None);
            }

            let definition_index = piece.definition_index();
            let definition = piece.get_definition(&self.rules.piece_set);
            let debug = definition.debug;

            (definition, definition_index, debug)
        };

        // Moves generated by the state machine; to be checked for validity by
        // `VictoryCondition`.
        let mut potential_moves = FxHashSet::default();

        let mut checked_state = CheckedState::default();

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
                let game = self.clone();
                let partial_delta = delta.clone();

                game.normalize_and_append_delta_unchecked_without_evaluation(partial_delta, move_type.clone())
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
                    let mut condition_evaluation_context = ConditionEvaluationContext {
                        game: &game,
                        previous_game: self,
                        current_player,
                        isometry: &isometry,
                        checked_state: &mut checked_state,
                        debug,
                    };

                    if !condition.evaluate(&mut condition_evaluation_context) {
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

                                delta.set(target, new_piece);
                            },
                            ActionEnum::CopyTile { source, target } => {
                                let source = isometry.apply(source);
                                let target = isometry.apply(target);
                                let piece = game_state.tile(&game.rules.board, source).and_then(|tile| tile.get_piece().cloned());

                                checked_state.register_checked_piece_tile(source);
                                delta.set(target, piece);
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

                            delta.set(tile, None);
                            checked_state.register_checked_piece_tile(tile);
                        }

                        checked_state.register_checked_piece_tile(move_choice);
                        delta.set(move_choice, piece);

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
                let game = self.clone().append_unchecked_without_evaluation(mv.clone());

                println!("Pseudo-legal move:\n{game}", game=G::print(&game));
            } 
        }

        Ok(Some(PieceMoves {
            moves: potential_moves,
            checked_state,
        }))
    }
}

impl<G: BoardGeometry> Game<G, Evaluated<G>> {
    // TODO: Validate that arguments are compatible.
    /// Creates a new game with the provided `rules` and the `initial_state` corresponding to those
    /// rules.
    pub fn new(rules: GameRules<G>, initial_state: GameState<G>) -> Self {
        let result = Game {
            rules: Arc::new(rules),
            move_log: MoveLog::new(initial_state),
            pseudo_legal_moves: None,
            move_cache: Default::default(),
            evaluation: NotEvaluated,
        };

        result.evaluate()
    }

    pub fn reset_with_state(&mut self, initial_state: GameState<G>) {
        self.move_log = MoveLog::new(initial_state);

        replace_with_or_default(self, |result| {
            result.replace_evaluation(|_| NotEvaluated).evaluate()
        });
    }

    /// Returns the outcome of the game at the current state.
    pub fn get_outcome(&self) -> Option<&Outcome> {
        self.evaluation.outcome.as_ref()
    }

    /// Returns a collection of all valid moves.
    pub fn available_moves(&self) -> &AvailableMoves<G> {
        &self.evaluation.available_moves
    }

    /// If the provided `delta` corresponds to a legal move, plays that move.
    /// Otherwise, results in an `Err(_)`.
    #[must_use = "The move may not be available."]
    pub fn append_delta(&mut self, delta: ReversibleGameStateDelta<G>) -> Result<(), ()> {
        // The game has already been decided.
        if self.evaluation.outcome.is_some() {
            return Err(());
        }

        self.available_moves().is_delta_available(&delta).then(|| {
            replace_with_or_default(self, |result| {
                result.append_delta_unchecked(delta)
            })
        }).ok_or(())
    }

    /// If the provided `delta`, normalized, corresponds to a legal move, plays that move.
    /// Otherwise, results in an `Err(_)`.
    #[must_use = "The move may not be available."]
    pub fn normalize_and_append_delta(&mut self, delta: GameStateDelta<G>, move_type: MoveType) -> Result<(), ()> {
        let normalized = delta.normalize(&self.move_log.current_state, self.rules().board(), move_type);

        self.append_delta(normalized)
    }

    /// If the provided move is legal (is an element of [`Game::available_moves`]), plays that
    /// move.
    /// Otherwise, results in an `Err(_)`.
    #[must_use = "The move may not be available."]
    pub fn append(&mut self, mv: Move<G>) -> Result<(), ()> {
        // The game has already been decided.
        if self.evaluation.outcome.is_some() {
            return Err(());
        }

        self.available_moves().is_move_available(&mv).then(|| {
            replace_with_or_default(self, |result| {
                result.append_unchecked(mv)
            })
        }).ok_or(())
    }

    /// If the provided `delta` corresponds to a legal move, plays that move.
    /// Otherwise, results in an `Err(_)`.
    #[must_use = "The move may not be available."]
    pub fn append_delta_without_evaluation(self, delta: ReversibleGameStateDelta<G>) -> Result<Game<G, NotEvaluated>, ()> {
        // The game has already been decided.
        if self.evaluation.outcome.is_some() {
            return Err(());
        }

        self.available_moves().is_delta_available(&delta).then(|| {
            self.append_delta_unchecked_without_evaluation(delta)
        }).ok_or(())
    }

    /// If the provided `delta`, normalized, corresponds to a legal move, plays that move.
    /// Otherwise, results in an `Err(_)`.
    #[must_use = "The move may not be available."]
    pub fn normalize_and_append_delta_without_evaluation(self, delta: GameStateDelta<G>, move_type: MoveType) -> Result<Game<G, NotEvaluated>, ()> {
        let normalized = delta.normalize(&self.move_log.current_state, self.rules().board(), move_type);

        self.append_delta_without_evaluation(normalized)
    }

    /// If the provided move is legal (is an element of [`Game::available_moves`]), plays that
    /// move.
    /// Otherwise, results in an `Err(_)`.
    #[must_use = "The move may not be available."]
    pub fn append_without_evaluation(self, mv: Move<G>) -> Result<Game<G, NotEvaluated>, ()> {
        // The game has already been decided.
        if self.evaluation.outcome.is_some() {
            return Err(());
        }

        self.available_moves().is_move_available(&mv).then(|| {
            self.append_unchecked_without_evaluation(mv)
        }).ok_or(())
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

impl<G: BoardGeometry> Default for GameRules<G> {
    fn default() -> Self {
        Self {
            board: Default::default(),
            piece_set: Default::default(),
            players: NonZeroUsize::new(2).unwrap(),
            victory_conditions: Box::new(NoVictoryCondition),
        }
    }
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

    pub fn victory_conditions(&self) -> &dyn VictoryCondition<G> {
        &*self.victory_conditions
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
