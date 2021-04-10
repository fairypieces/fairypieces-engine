use std::collections::BTreeSet;
use fxhash::{FxHashSet, FxHashMap};
use crate::{Game, MoveLog};
use crate::game::PlayerIndex;
use crate::delta::{Move, ReversibleGameStateDelta};
use crate::math::*;
use crate::board::*;

// TODO: Improve error types for validation.

pub type PieceDefinitionIndex = u16;
pub type TileFlagIndex = u32;

/// A set of piece definitions whose movesets may refer to each other.
#[derive(Clone, Debug)]
pub struct PieceSet<G: BoardGeometry> {
    definitions: Box<[PieceDefinition<G>]>,
}

impl<G: BoardGeometry> PieceSet<G> {
    /// Validates the pieces and creates a piece set.
    pub fn from(unvalidated_definitions: impl IntoIterator<Item=PieceDefinitionUnvalidated<G>>) -> Result<Self, ()> {
        let unvalidated_definitions = unvalidated_definitions.into_iter().collect::<Vec<_>>();
        let definitions = unvalidated_definitions.clone().into_iter()
            .map(|definition| definition.validate(&unvalidated_definitions))
            .collect::<Result<_, _>>()?;

        Ok(Self {
            definitions,
        })
    }

    pub fn definitions(&self) -> &[PieceDefinition<G>] {
        &self.definitions
    }
}

/// A piece on the game `Board`.
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Piece<G: BoardGeometry> {
    /// The index of the piece definition.
    pub(crate) definition: u16,
    /// The piece's rotation and/or reflection.
    pub(crate) transformation: AxisPermutation<G>,
    /// The player who owns this piece.
    pub(crate) owner: u8,
    /// Indices of moves in which this move was affected, in ascending order.
    pub(crate) affecting_moves: im::Vector<u16>,
}

impl<G: BoardGeometry> Piece<G> {
    // TODO: Actually validate pieces that are added to the board.
    // TODO: Possibly create an `UnvalidatedPiece` type to make it type safe.
    pub(crate) fn check_validity(&self, definitions: &[PieceDefinitionUnvalidated<G>]) -> Result<(), ()> {
        // TODO validate owner (team)?
        if self.definition as usize >= definitions.len() {
            return Err(());
        }

        Ok(())
    }

    pub fn was_affected_by_move(&self, move_index: usize) -> bool {
        self.affecting_moves.binary_search(&(move_index as u16)).is_ok()
    }

    pub fn affecting_moves(&self) -> impl Iterator<Item=u16> + DoubleEndedIterator + '_ {
        self.affecting_moves.iter().cloned()
    }

    pub fn push_affecting_move(&mut self, move_index: usize) {
        let move_index = move_index as u16;
        let push = if let Some(last_affecting_move) = self.affecting_moves().last() {
            debug_assert!(last_affecting_move <= move_index);

            last_affecting_move < move_index
        } else {
            true
        };

        if push {
            self.affecting_moves.push_back(move_index);
        }
    }

    pub fn make_initial(&mut self) {
        self.affecting_moves.clear();
    }

    pub fn definition_index(&self) -> u16 {
        self.definition
    }

    pub fn owner(&self) -> u8 {
        self.owner
    }

    pub fn is_initial(&self) -> bool {
        self.affecting_moves.is_empty()
    }

    pub fn get_definition<'a>(&'_ self, piece_set: &'a PieceSet<G>) -> &'a PieceDefinition<G> {
        &piece_set.definitions[self.definition as usize]
    }

//     /// Clones the piece but sets the `initial` field to `false`, marking the piece
//     /// as having been moved or affected by another piece's move.
//     pub fn clone_moved(&self) -> Self {
//         Self {
//             transformation: self.transformation.clone(),
//             definition: self.definition.clone(),
//             owner: self.owner.clone(),
//             affecting_moves:
//         }
//     }
}

/// An unvalidated piece definition is used to construct a [`PieceSet`]. See [`PieceDefinition`]
/// for the purpose and structure of the validated counterpart to this type.
/// A `PieceDefinitionUnvalidated` used in a successful construction of a [`PieceSet`] becomes
/// a validated [`PieceDefinition`].
#[derive(Default, Debug, Clone, Hash, PartialEq, Eq)]
pub struct PieceDefinitionUnvalidated<G: BoardGeometry> {
    /// The user-facing (currently not localized) title of the piece.
    pub title: String,
    /// A list of states making up the state machine, required to be non-empty during [`PieceSet`]
    /// construction.
    pub states: Vec<StateUnvalidated<G>>,
    /// A list of indices of initial states, required to be non-empty during [`PieceSet`]
    /// construction.
    pub initial_states: Vec<usize>,
    pub debug: bool,
}

impl<G: BoardGeometry> PieceDefinitionUnvalidated<G> {
    /// Creates a new `PieceDefinitionUnvalidated` with the given title and default attributes.
    pub fn new(title: impl ToString) -> Self {
        Self {
            title: title.to_string(),
            states: Default::default(),
            initial_states: Default::default(),
            debug: false,
        }
    }

    pub fn with_debug(mut self, debug: bool) -> Self {
        self.debug = debug;
        self
    }

    /// A builder function to add an initial state to the piece definition's state machine.
    pub fn with_initial_state(mut self, state: StateUnvalidated<G>) -> Self {
        self.states.push(state);
        self.initial_states.push(self.states.len() - 1);
        self
    }

    /// A builder function to add an internal state to the piece definition's state machine.
    pub fn with_state(mut self, state: StateUnvalidated<G>) -> Self {
        self.states.push(state);
        self
    }

    fn check_validity(&self, definitions: &[PieceDefinitionUnvalidated<G>]) -> Result<(), ()> {
        if self.initial_states.is_empty() {
            return Err(());
        }

        for initial_state_index in &self.initial_states {
            if *initial_state_index >= self.states.len() {
                return Err(());
            }
        }

        for state in &self.states {
            state.check_validity(self, definitions)?;
        }

        Ok(())
    }

    fn assume_validated(mut self) -> PieceDefinition<G> {
        // Ensure piece definition equality is invariant over initial state order
        self.initial_states.sort_unstable();
        self.initial_states.dedup();

        PieceDefinition {
            title: self.title,
            states: self.states.into_iter().map(|state| state.assume_validated()).collect(),
            initial_states: self.initial_states.into_boxed_slice(),
            debug: self.debug,
        }
    }

    fn validate(self, definitions: &[PieceDefinitionUnvalidated<G>]) -> Result<PieceDefinition<G>, ()> {
        self.check_validity(definitions).map(move |()| {
            self.assume_validated()
        })
    }
}

/// A piece definition defines the rules by which the piece moves and affects the game board.
/// The valid moves of a piece are encoded using a state machine with context.
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct PieceDefinition<G: BoardGeometry> {
    /// The user-facing (currently not localized) title of the piece.
    pub(crate) title: String,
    /// Non-empty list of states making up the state machine.
    pub(crate) states: Box<[State<G>]>,
    /// Non-empty list of indices of initial states.
    pub(crate) initial_states: Box<[usize]>,
    pub(crate) debug: bool,
}

// TODO: Consider splitting this type into two separate types of predicates and operators.
// TODO: A lot of the predicates are of the form: _assumption_ implies _condition_. If _assumption_
//       does not hold, then the entire expression is evaluated as true. It might be possible to
//       cache all of the _assumptions_ that have already been evaluated for this state and reuse
//       their results.
/// A condition that must be satisfied in order for a `State` to be processed during
/// the move generation of a piece.
#[derive(PartialOrd, Ord, PartialEq, Eq, Hash, Clone, Debug)]
pub enum ConditionEnum<G: BoardGeometry> {
    /// A boolean OR operation or "disjunction". Evaluates to `true` if any of the inner conditions
    /// evaluate to `true`.
    Any(BTreeSet<ConditionEnum<G>>),
    /// A boolean AND operation or "conjunction". Evaluates to `true` if all of the inner conditions
    /// evaluate to `true`.
    All(BTreeSet<ConditionEnum<G>>),
    // TODO: Investigate how applying De Morgan's laws could improve the performance.
    /// Evaluates to `true` if the inner condition evaluates to `false` and vice versa.
    Not(Box<ConditionEnum<G>>),
    /// Evaluates to `true` if the number of moves played during the game (excluding the current
    /// temporary move) is greater than or equal to the inner value.
    MovesPlayedGreaterThanOrEqual(usize),
    /// Evaluates to `true` if not enough moves were played to include `past_move`,
    /// or if the move was generated by a piece with the piece definition with the
    /// given index `piece_definition`.
    MoveGeneratedByPiece {
        /// The move index from the last move (including the current, temporary move).
        /// Move 0 refers to the current, temporary move; move 1 refers to the last played move;
        /// move 2 refers to the second last played move, etc.
        past_move: usize,
        piece_definition: PieceDefinitionIndex,
    },
    /// Evaluates to `true` if not enough moves were played to include `past_move`,
    /// or if the move was generated by the final state given by the index `state_index`.
    MoveGeneratedByFinalState {
        /// The move index from the last move (including the current, temporary move).
        /// Move 0 refers to the current, temporary move; move 1 refers to the last played move;
        /// move 2 refers to the second last played move, etc.
        past_move: usize,
        state_index: usize,
    },
    /// Evaluates to `true` if not enough moves were played to include `past_move`,
    /// or if during the move's generation, the marked state of the given index was visited.
    MoveGeneratedByVisitingMarkedState {
        /// The move index from the last move (including the current, temporary move).
        /// Move 0 refers to the current, temporary move; move 1 refers to the last played move;
        /// move 2 refers to the second last played move, etc.
        past_move: usize,
        state_index: usize,
    },
    /// Evaluates to `true` if a tile with those coordinates is present.
    TilePresent {
        tile: <G as BoardGeometryExt>::Tile,
    },
    /// Evaluates to `true` if the tile at the given coordinates matches the type.
    TileTypeIs {
        tile: <G as BoardGeometryExt>::Tile,
        type_index: usize,
    },
    /// Evaluates to `true` if the tile at the given coordinates has the provided flag.
    TileFlagPresent {
        tile: <G as BoardGeometryExt>::Tile,
        flag: TileFlagIndex,
    },
    /// Evaluates to `true` if a piece is present on the given tile.
    PiecePresent {
        tile: <G as BoardGeometryExt>::Tile,
    },
    /// Evaluates to `true` if not enough moves were played to include `past_move`,
    /// if there is no piece at the given tile,
    /// or if the piece at the given coordinates was affected during that move.
    PieceAffectedByMove {
        tile: <G as BoardGeometryExt>::Tile,
        /// The move index from the last move (excluding the current move).
        /// Move 0 refers to the last move that was played, move 1 refers to the
        /// second last, etc.
        past_move: usize,
    },
    /// Evaluates to `true` if the piece has not been moved since the beginning of the game.
    PieceInitial {
        tile: <G as BoardGeometryExt>::Tile,
    },
    /// Evaluates to `true` if there is no piece on the tile or if the piece's definition index
    /// matches `definition_index`.
    PieceTypeIs {
        tile: <G as BoardGeometryExt>::Tile,
        definition_index: PieceDefinitionIndex,
    },
    /// Evaluates to `true` if there is no piece on the tile or if the piece is controlled by the
    /// specified player.
    PieceControlledBy {
        tile: <G as BoardGeometryExt>::Tile,
        player: PlayerIndex,
    },
    /// Evaluates to `true` if there is no piece on the tile or if the piece is allied to the
    /// current player.
    PieceControlledByAlly {
        tile: <G as BoardGeometryExt>::Tile,
    },
    /// Evaluates to `true` if there is no piece on the tile or if the piece is an enemy to the
    /// current player.
    PieceControlledByEnemy {
        tile: <G as BoardGeometryExt>::Tile,
    },
    /// Evaluates to `true` if the move that is currently being generated, made up of the actions
    /// executed on this evaluation branch, would be a legal move.
    UnfinishedMoveLegal,
}

pub(crate) type PseudoLegalMoves<G> = FxHashMap<<G as BoardGeometryExt>::Tile, FxHashSet<Move<G>>>;

/// A cache of generated pseudo-legal moves.
/// Moves are cached every turn for the current player, under the assumption that pieces are
/// controlled by a single player throughout the entire game.
#[derive(Default, Clone, Debug)]
pub(crate) struct MoveCache<G: BoardGeometry> {
    applied_moves: usize,
    moves: im::OrdMap<<G as BoardGeometryExt>::Tile, PieceMoves<G>>,
    influence: PieceMovesInfluence<G>,
}

impl<G: BoardGeometry> MoveCache<G> {
    pub(crate) fn get_cached_moves_from(&self, tile: <G as BoardGeometryExt>::Tile) -> Option<&PieceMoves<G>> {
        self.moves.get(&tile)
    }

    pub(crate) fn cache_moves(&mut self, tile: <G as BoardGeometryExt>::Tile, moves: PieceMoves<G>) {
        if matches!(&moves.checked_state, CheckedState::PossiblyInvalid { .. }) {
            self.influence.add(tile, &moves.checked_state);
            self.moves.insert(tile, moves);
        }
    }

    /// Invalidates moves based on moves played since this function was last called.
    pub(crate) fn invalidate_recent(
        &mut self,
        move_log: &MoveLog<G>,
    ) -> bool {
        if self.applied_moves >= move_log.len() {
            debug_assert_eq!(self.applied_moves, move_log.len());

            return false;
        }

        let mut removed_moves = Vec::new();

        for mv in move_log.moves().skip(self.applied_moves) {
            self.invalidate(mv, &mut removed_moves);
        }

        self.applied_moves = move_log.len();

        true
    }

    /// Invalidates moves dependent on the state affected by the provided `delta`.
    /// Returns a set of tiles for which moves need to be regenerated.
    fn invalidate(
        &mut self,
        delta: &ReversibleGameStateDelta<G>,
        removed_moves: &mut Vec<(<G as BoardGeometryExt>::Tile, PieceMoves<G>)>,
    ) {
        for piece_affected_tile in delta.forward().affected_pieces.keys() {
            // FIXME: Requires Clone, consider using Arc?
            if let Some(moves) = self.moves.remove(piece_affected_tile) {
                removed_moves.push((*piece_affected_tile, moves));
            }

            for second_order_tile in self.influence.piece_influenced_tiles.get(piece_affected_tile).into_iter().flatten() {
                // FIXME: Requires Clone, consider using Arc?
                if let Some(moves) = self.moves.remove(second_order_tile) {
                    removed_moves.push((*second_order_tile, moves));
                }
            }
        }

        for flag_affected_tile in delta.forward().affected_flags.keys() {
            // FIXME: Requires Clone, consider using Arc?
            if let Some(moves) = self.moves.remove(flag_affected_tile) {
                removed_moves.push((*flag_affected_tile, moves));
            }

            for second_order_tile in self.influence.flag_influenced_tiles.get(flag_affected_tile).into_iter().flatten() {
                // FIXME: Requires Clone, consider using Arc?
                if let Some(moves) = self.moves.remove(second_order_tile) {
                    removed_moves.push((*second_order_tile, moves));
                }
            }
        }

        for (tile, removed_moves) in removed_moves.drain(..) {
            self.influence.remove(tile, &removed_moves.checked_state);
        }
    }
}

/// Moves generated by a piece
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct PieceMoves<G: BoardGeometry> {
    pub(crate) moves: FxHashSet<Move<G>>,
    pub(crate) checked_state: CheckedState<G>,
}

/// Maps source tiles to target tiles if the piece at the target tiles would be invalidated by
/// influencing the piece/flag on the source tile.
/// Basically the reverse of multiple `CheckedState`s.
#[derive(Default, Clone, Debug)]
pub(crate) struct PieceMovesInfluence<G: BoardGeometry> {
    /// If a piece on the tile of the key was influenced, invalidate all moves generated by pieces
    /// in the values.
    pub(crate) piece_influenced_tiles: im::OrdMap<<G as BoardGeometryExt>::Tile, im::OrdSet<<G as BoardGeometryExt>::Tile>>,
    /// If the flags on the tile of the key were influenced, invalidate all moves generated by pieces
    /// in the values.
    pub(crate) flag_influenced_tiles: im::OrdMap<<G as BoardGeometryExt>::Tile, im::OrdSet<<G as BoardGeometryExt>::Tile>>,
}

impl<G: BoardGeometry> PieceMovesInfluence<G> {
    pub(crate) fn remove(&mut self, piece_tile: <G as BoardGeometryExt>::Tile, checked_state: &CheckedState<G>) {
        if let CheckedState::PossiblyInvalid {
            invalidating_piece_tiles,
            invalidating_flag_tiles,
        } = checked_state {
            let mut removed_from = FxHashSet::default();

            for invalidating_piece_tile in invalidating_piece_tiles {
                self.piece_influenced_tiles.entry(*invalidating_piece_tile)
                    .or_default().remove(&piece_tile);

                removed_from.insert(*invalidating_piece_tile);
            }

            for invalidating_flag_tile in invalidating_flag_tiles {
                self.flag_influenced_tiles.entry(*invalidating_flag_tile)
                    .or_default().remove(&piece_tile);
            }
        } else {
            unimplemented!()
        }
    }

    pub(crate) fn add(&mut self, piece_tile: <G as BoardGeometryExt>::Tile, checked_state: &CheckedState<G>) {
        if let CheckedState::PossiblyInvalid {
            invalidating_piece_tiles,
            invalidating_flag_tiles,
        } = checked_state {
            for invalidating_piece_tile in invalidating_piece_tiles {
                self.piece_influenced_tiles.entry(*invalidating_piece_tile)
                    .or_default().insert(piece_tile);
            }

            for invalidating_flag_tile in invalidating_flag_tiles {
                self.flag_influenced_tiles.entry(*invalidating_flag_tile)
                    .or_default().insert(piece_tile);
            }
        } else {
            unimplemented!()
        }
    }
}

/// All state that has been checked during the generation of a piece's moves.
/// If none of the checks performed are influenced by the next move, the piece's moves can be kept
/// instead of being regenerated. The moves are kept for each player separately, as only the owner
/// of a piece may move it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum CheckedState<G: BoardGeometry> {
    /// Regenerate moves if any of the following conditions are met.
    PossiblyInvalid {
        /// If any of these tiles' pieces are affected, regenerate moves.
        invalidating_piece_tiles: FxHashSet<<G as BoardGeometryExt>::Tile>,
        /// If any of these tiles' flags are affected, regenerate moves.
        invalidating_flag_tiles: FxHashSet<<G as BoardGeometryExt>::Tile>,
    },
    /// Always regenerate moves.
    CertainlyInvalid,
}

impl<G: BoardGeometry> Default for CheckedState<G> {
    fn default() -> Self {
        Self::PossiblyInvalid {
            invalidating_piece_tiles: Default::default(),
            invalidating_flag_tiles: Default::default(),
        }
    }
}

impl<G: BoardGeometry> CheckedState<G> {
    pub fn invalidate(&mut self) {
        *self = Self::CertainlyInvalid;
    }

    pub fn register_checked_piece_tile(&mut self, tile: <G as BoardGeometryExt>::Tile) {
        if let &mut Self::PossiblyInvalid { ref mut invalidating_piece_tiles, .. } = self {
            invalidating_piece_tiles.insert(tile);
        }
    }

    pub fn register_checked_flag_tile(&mut self, tile: <G as BoardGeometryExt>::Tile) {
        if let &mut Self::PossiblyInvalid { ref mut invalidating_flag_tiles, .. } = self {
            invalidating_flag_tiles.insert(tile);
        }
    }
}

pub(crate) struct ConditionEvaluationContext<'a, G: BoardGeometry> {
    pub(crate) game: &'a Game<G>,
    pub(crate) previous_game: &'a Game<G>,
    pub(crate) current_player: PlayerIndex,
    pub(crate) isometry: &'a Isometry<G>,
    pub(crate) checked_state: &'a mut CheckedState<G>,
    pub(crate) debug: bool,
}

impl<G: BoardGeometry> ConditionEnum<G> {
    pub fn always() -> Self {
        ConditionEnum::All(Default::default())
    }

    pub fn never() -> Self {
        ConditionEnum::Any(Default::default())
    }

    pub fn not(inner: Self) -> Self {
        ConditionEnum::Not(Box::new(inner))
    }

    pub fn any(conditions: impl IntoIterator<Item=ConditionEnum<G>>) -> Self {
        ConditionEnum::Any(conditions.into_iter().collect())
    }

    pub fn all(conditions: impl IntoIterator<Item=ConditionEnum<G>>) -> Self {
        ConditionEnum::All(conditions.into_iter().collect())
    }

    pub(crate) fn evaluate(&self, context: &mut ConditionEvaluationContext<'_, G>) -> bool {
        use ConditionEnum::*;
        let result = match self {
            Any(children) => {
                children.iter().any(|child| child.evaluate(context))
            },
            All(children) => {
                children.iter().all(|child| child.evaluate(context))
            },
            Not(child) => !child.evaluate(context),
            MovesPlayedGreaterThanOrEqual(moves) => {
                context.checked_state.invalidate();

                // Subtract 1 from the move log length, because the current state
                // of the board contains a temporary move (which was not played by any of the
                // players) used for computing the resulting move.
                context.game.move_log.moves.len() >= *moves + 1
            },
            MoveGeneratedByPiece { past_move, piece_definition } => {
                if !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*past_move), context) {
                    return true;
                }

                context.checked_state.invalidate();

                let mv = context.game.move_log().moves().nth_back(*past_move).unwrap();
                let move_type = mv.move_type();

                move_type.definition == *piece_definition
            },
            MoveGeneratedByFinalState { past_move, state_index } => {
                if !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*past_move), context) {
                    return true;
                }

                context.checked_state.invalidate();

                let mv = context.game.move_log().moves().nth_back(*past_move).unwrap();
                let move_type = mv.move_type();

                move_type.final_state == *state_index
            },
            MoveGeneratedByVisitingMarkedState { past_move, state_index } => {
                if !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*past_move), context) {
                    return true;
                }

                context.checked_state.invalidate();

                let mv = context.game.move_log().moves().nth_back(*past_move).unwrap();
                let move_type = mv.move_type();

                move_type.has_visited_marked_state(*state_index)
            },
            TilePresent { tile } => {
                // Tiles are not added or removed throughout the game, therefore no checks are
                // recorded in `context.checked_state`.

                let tile = context.isometry.apply(*tile);

                context.game.rules().board().tiles().contains(tile)
            },
            TileTypeIs { tile, type_index } => {
                if !Self::evaluate(&TilePresent { tile: *tile }, context) {
                    return true;
                }

                // The types of tiles are not changed throughout the game, therefore no checks are
                // recorded in `context.checked_state`.

                let tile = context.isometry.apply(*tile);
                <G as BoardGeometry>::get_tile_type(tile) == *type_index
            },
            TileFlagPresent { tile, flag } => {
                if !Self::evaluate(&TilePresent { tile: *tile }, context) {
                    return true;
                }

                let current_state = context.game.move_log().current_state();
                let tile = context.isometry.apply(*tile);

                context.checked_state.register_checked_flag_tile(tile);
                current_state.flags.contains_flag(tile, *flag)
            },
            PiecePresent { tile } => {
                if !Self::evaluate(&TilePresent { tile: *tile }, context) {
                    return true;
                }

                let current_state = context.game.move_log().current_state();
                let tile = context.isometry.apply(*tile);

                context.checked_state.register_checked_piece_tile(tile);

                let tile = current_state.tile(context.game.rules().board(), tile).unwrap();

                tile.get_piece().is_some()
            },
            PieceAffectedByMove { tile, past_move } => {
                if !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*past_move), context)
                    || !Self::evaluate(&TilePresent { tile: *tile }, context)
                    || !Self::evaluate(&PiecePresent { tile: *tile }, context)
                    || !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*past_move), context) {
                    return true;
                }

                let current_state = context.game.move_log().current_state();
                let tile = context.isometry.apply(*tile);
                let tile = current_state.tile(context.game.rules().board(), tile).unwrap();
                let piece = tile.get_piece().unwrap();
                let absolute_move = context.game.move_log().len() - 1 - past_move;

                context.checked_state.invalidate();
                piece.was_affected_by_move(absolute_move)
            },
            PieceInitial { tile } => {
                if !Self::evaluate(&TilePresent { tile: *tile }, context)
                    || !Self::evaluate(&PiecePresent { tile: *tile }, context) {
                    return true;
                }

                let current_state = context.game.move_log().current_state();
                let tile = context.isometry.apply(*tile);

                context.checked_state.register_checked_piece_tile(tile);

                let tile = current_state.tile(context.game.rules().board(), tile).unwrap();
                let piece = tile.get_piece().unwrap();

                piece.is_initial()
            },
            PieceTypeIs { tile, definition_index } => {
                if !Self::evaluate(&TilePresent { tile: *tile }, context)
                    || !Self::evaluate(&PiecePresent { tile: *tile }, context) {
                    return true;
                }

                let current_state = context.game.move_log().current_state();
                let tile = context.isometry.apply(*tile);

                context.checked_state.register_checked_piece_tile(tile);

                let tile = current_state.tile(context.game.rules().board(), tile).unwrap();
                let piece = tile.get_piece().unwrap();

                piece.definition == *definition_index
            },
            PieceControlledBy { tile, player } => {
                if !Self::evaluate(&TilePresent { tile: *tile }, context)
                    || !Self::evaluate(&PiecePresent { tile: *tile }, context) {
                    return true;
                }

                let current_state = context.game.move_log().current_state();
                let tile = context.isometry.apply(*tile);

                context.checked_state.register_checked_piece_tile(tile);

                let tile = current_state.tile(context.game.rules().board(), tile).unwrap();
                let piece = tile.get_piece().unwrap();

                piece.owner == *player
            },
            PieceControlledByAlly { tile } => {
                if !Self::evaluate(&TilePresent { tile: *tile }, context)
                    || !Self::evaluate(&PiecePresent { tile: *tile }, context) {
                    return true;
                }

                let current_state = context.game.move_log().current_state();
                let tile = context.isometry.apply(*tile);

                context.checked_state.register_checked_piece_tile(tile);

                let tile = current_state.tile(context.game.rules().board(), tile).unwrap();
                let piece = tile.get_piece().unwrap();

                piece.owner == context.current_player
            },
            PieceControlledByEnemy { tile } => {
                if !Self::evaluate(&TilePresent { tile: *tile }, context)
                    || !Self::evaluate(&PiecePresent { tile: *tile }, context) {
                    return true;
                }

                let current_state = context.game.move_log().current_state();
                let tile = context.isometry.apply(*tile);

                context.checked_state.register_checked_piece_tile(tile);

                let tile = current_state.tile(context.game.rules().board(), tile).unwrap();
                let piece = tile.get_piece().unwrap();

                piece.owner != context.current_player
            },
            UnfinishedMoveLegal => {
                // Unwrap safety: The last move is the unfinished move currently being generated.
                let mv = context.game.move_log().moves().last().unwrap();

                context.checked_state.invalidate();
                context.previous_game.rules().victory_conditions().is_move_legal(context.previous_game, mv)
            },
        };

        if context.debug {
            println!("{result} = {self:#?}");
        }

        result
    }
}

/// An unvalidated state is used to construct a [`PieceDefinitionUnvalidated`].
/// See [`State`] for the purpose and structure of the validated counterpart to this type.
/// A `StateUnvalidated` used in a successful construction of a [`PieceSet`] becomes
/// a validated [`State`].
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct StateUnvalidated<G: BoardGeometry> {
    /// The action to perform when this state is visited.
    pub(crate) action: Action<G>,
    /// The indices of the successor states.
    pub(crate) successor_indices: Vec<usize>,
    /// If the state is marked as final, the resulting resulting delta (the state of the board
    /// after the action has been executed) will be used as a pseudo-legal move. A pseudo-legal
    /// move is a move that would be legal if the victory conditions were not considered (check,
    /// checkmates in case of international chess).
    pub(crate) is_final: bool,
    /// If a marked state is visited during execution, that fact is recorded for the resulting
    /// move's delta, similarly to the final state. The number of visited marked states is
    /// arbitrary.
    pub(crate) is_marked: bool,
}

impl<G: BoardGeometry> StateUnvalidated<G> {
    pub fn new(action: Action<G>) -> Self {
        Self {
            action,
            successor_indices: Vec::new(),
            is_final: false,
            is_marked: false,
        }
    }

    pub fn with_successor(mut self, successor_index: usize) -> Self {
        self.successor_indices.push(successor_index);
        self
    }

    pub fn with_successors(mut self, successor_indices: impl IntoIterator<Item=usize>) -> Self {
        for successor_index in successor_indices {
            self = self.with_successor(successor_index);
        }

        self
    }

    pub fn with_final(mut self, is_final: bool) -> Self {
        self.is_final = is_final;
        self
    }

    pub fn with_marked(mut self, is_marked: bool) -> Self {
        self.is_marked = is_marked;
        self
    }

    fn check_validity(&self, piece: &PieceDefinitionUnvalidated<G>, definitions: &[PieceDefinitionUnvalidated<G>]) -> Result<(), ()> {
        for successor_index in &*self.successor_indices {
            if *successor_index >= piece.states.len() {
                return Err(());
            }
        }

        self.action.check_validity(piece, definitions)?;

        Ok(())
    }

    fn assume_validated(mut self) -> State<G> {
        // Ensure state equality is invariant over successor state order
        self.successor_indices.sort_unstable();
        self.successor_indices.dedup();

        State {
            action: self.action,
            successor_indices: self.successor_indices.into_boxed_slice(),
            is_final: self.is_final,
            is_marked: self.is_marked,
        }
    }

    pub fn validate(self, piece: &PieceDefinitionUnvalidated<G>, definitions: &[PieceDefinitionUnvalidated<G>]) -> Result<State<G>, ()> {
        self.check_validity(piece, definitions).map(move |()| {
            self.assume_validated()
        })
    }
}

/// A `State` makes up a node of the state machine that generates a piece's moves.
#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct State<G: BoardGeometry> {
    /// The action to perform when this state is visited.
    pub(crate) action: Action<G>,
    /// The indices of the successor states.
    pub(crate) successor_indices: Box<[usize]>,
    /// If the state is marked as final, the resulting resulting delta (the state of the board
    /// after the action has been executed) will be used as a pseudo-legal move. A pseudo-legal
    /// move is a move that would be legal if the victory conditions were not considered (check,
    /// checkmates in case of international chess).
    pub(crate) is_final: bool,
    /// If a marked state is visited during execution, that fact is recorded for the resulting
    /// move's delta, similarly to the final state. The number of visited marked states is
    /// arbitrary.
    pub(crate) is_marked: bool,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Hash, Clone, Debug)]
pub enum ActionEnum<G: BoardGeometry> {
    /// Replace the contents of the target tile with a specific piece or clear the target tile.
    SetTile {
        /// The index of the piece within the `PieceSet` to place on the tile
        /// or `None` to clear the tile.
        piece: Option<PieceDefinitionIndex>,
        // TODO: Should be isometry so that the piece can be rotated as well?
        /// The target tile to change the contents of.
        target: <G as BoardGeometryExt>::Tile,
    },
    /// Replace the contents of the target tile with the contents of the source tile.
    CopyTile {
        /// The source tile to copy the contents from.
        source: <G as BoardGeometryExt>::Tile,
        // TODO: Should be isometry so that the piece can be rotated as well?
        /// The target tile to paste the contents to.
        target: <G as BoardGeometryExt>::Tile,
    },
}

impl<G: BoardGeometry> ActionEnum<G> {
    fn check_validity(&self, piece_definition: &PieceDefinitionUnvalidated<G>, definitions: &[PieceDefinitionUnvalidated<G>]) -> Result<(), ()> {
        // TODO: Check relative position validity based on the source tile type
        use ActionEnum::*;

        match self {
            SetTile { piece, .. } => {
                if let Some(piece) = piece {
                    if *piece >= definitions.len() as PieceDefinitionIndex {
                        return Err(());
                    }
                }
            },
            CopyTile { .. } => (),
        }

        Ok(())
    }
}

/// An action to be performed when the [`State`] containing this action is visited during
/// move generation. The action operates on the current state (and context) of the
/// state machine, equal to the original game state with actions accumulated
/// from previously visited states.
#[derive(PartialEq, Eq, Hash, Clone, Debug, PartialOrd, Ord)]
pub enum Action<G: BoardGeometry> {
    /// Move the current piece, possibly affecting nearby tiles.
    Move {
        /// If this condition is not satisfied, the state is skipped over.
        condition: ConditionEnum<G>,
        /// These actions are performed in undefined order.
        /// For this reason, actions must be commutative.
        actions: BTreeSet<ActionEnum<G>>,
        // FIXME: Change to `Isometry` so as to allow pieces to be rotated as well as translated.
        /// Any of these moves are performed.
        move_choices: BTreeSet<<G as BoardGeometryExt>::Tile>,
    },
    /// Make the following moves symmetrical according to the provided symmetries.
    ///
    /// For example, the international chess' regular pawn capture moves
    /// (left and right) can be encoded using a state with identity and reflectional
    /// symmetry followed by a state that captures only to the right. This will
    /// result in the move being mirrored and the left capture being generated as well.
    Symmetry {
        symmetries: BTreeSet<AxisPermutation<G>>,
    },
}

impl<G: BoardGeometry> Action<G> {
    fn check_validity(&self, piece: &PieceDefinitionUnvalidated<G>, definitions: &[PieceDefinitionUnvalidated<G>]) -> Result<(), ()> {
        use Action::*;

        match self {
            Move { actions, .. } => {
                for inner in actions {
                    inner.check_validity(piece, definitions)?;
                }
            },
            Symmetry { .. } => (),
        }

        Ok(())
    }
}
