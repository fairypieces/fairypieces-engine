use std::collections::BTreeSet;
use crate::Game;
use crate::game::PlayerIndex;
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
        before_moves: usize,
        tile: <G as BoardGeometryExt>::Tile,
    },
    /// Evaluates to `true` if the tile at the given coordinates matches the type.
    TileTypeIs {
        before_moves: usize,
        tile: <G as BoardGeometryExt>::Tile,
        type_index: usize,
    },
    /// Evaluates to `true` if the tile at the given coordinates has the provided flag.
    TileFlagPresent {
        before_moves: usize,
        tile: <G as BoardGeometryExt>::Tile,
        flag: TileFlagIndex,
    },
    /// Evaluates to `true` if a piece is present on the given tile.
    PiecePresent {
        before_moves: usize,
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
        before_moves: usize,
        tile: <G as BoardGeometryExt>::Tile,
    },
    /// Evaluates to `true` if there is no piece on the tile or if the piece's definition index
    /// matches `definition_index`.
    PieceTypeIs {
        before_moves: usize,
        tile: <G as BoardGeometryExt>::Tile,
        definition_index: PieceDefinitionIndex,
    },
    /// Evaluates to `true` if there is no piece on the tile or if the piece is controlled by the
    /// specified player.
    PieceControlledBy {
        before_moves: usize,
        tile: <G as BoardGeometryExt>::Tile,
        player: PlayerIndex,
    },
    /// Evaluates to `true` if there is no piece on the tile or if the piece is allied to the
    /// current player.
    PieceControlledByAlly {
        before_moves: usize,
        tile: <G as BoardGeometryExt>::Tile,
    },
    /// Evaluates to `true` if there is no piece on the tile or if the piece is an enemy to the
    /// current player.
    PieceControlledByEnemy {
        before_moves: usize,
        tile: <G as BoardGeometryExt>::Tile,
    },
    /// Evaluates to `true` if the move that is currently being generated, made up of the actions
    /// executed on this evaluation branch, would be a legal move.
    UnfinishedMoveLegal,
}

pub struct ConditionEvaluationContext<'a, G: BoardGeometry> {
    pub game: &'a Game<G>,
    pub previous_game: &'a Game<G>,
    pub current_player: PlayerIndex,
    pub isometry: &'a Isometry<G>,
    pub debug: bool,
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

    pub fn evaluate(&self, context: &ConditionEvaluationContext<'_, G>) -> bool {
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
                // Subtract 1 from the move log length, because the current state
                // of the board contains a temporary move (which was not played by any of the
                // players) used for computing the resulting move.
                context.game.move_log.moves.len() >= *moves + 1
            },
            MoveGeneratedByPiece { past_move, piece_definition } => {
                if !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*past_move), context) {
                    return true;
                }

                let mv = context.game.move_log().moves().nth_back(*past_move).unwrap();
                let move_type = mv.move_type();

                move_type.definition == *piece_definition
            },
            MoveGeneratedByFinalState { past_move, state_index } => {
                if !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*past_move), context) {
                    return true;
                }

                let mv = context.game.move_log().moves().nth_back(*past_move).unwrap();
                let move_type = mv.move_type();

                move_type.final_state == *state_index
            },
            MoveGeneratedByVisitingMarkedState { past_move, state_index } => {
                if !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*past_move), context) {
                    return true;
                }

                let mv = context.game.move_log().moves().nth_back(*past_move).unwrap();
                let move_type = mv.move_type();

                move_type.has_visited_marked_state(*state_index)
            },
            TilePresent { before_moves, tile } => {
                if !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*before_moves), context) {
                    return true;
                }

                let tile = context.isometry.apply(*tile);
                context.game.past_tile_piece(*before_moves, tile).is_ok()
            },
            TileTypeIs { before_moves, tile, type_index } => {
                if !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*before_moves), context)
                    || !Self::evaluate(&TilePresent { before_moves: *before_moves, tile: *tile }, context) {
                    return true;
                }

                let tile = context.isometry.apply(*tile);
                <G as BoardGeometry>::get_tile_type(tile) == *type_index
            },
            TileFlagPresent { before_moves, tile, flag } => {
                if !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*before_moves), context)
                    || !Self::evaluate(&TilePresent { before_moves: *before_moves, tile: *tile }, context) {
                    return true;
                }

                let tile = context.isometry.apply(*tile);

                context.game.past_tile_flag(*before_moves, tile, *flag).unwrap()
            },
            PiecePresent { before_moves, tile } => {
                if !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*before_moves), context)
                    || !Self::evaluate(&TilePresent { before_moves: *before_moves, tile: *tile }, context) {
                    return true;
                }

                let tile = context.isometry.apply(*tile);
                let past_piece = context.game.past_tile_piece(*before_moves, tile).unwrap();

                past_piece.is_some()
            },
            PieceAffectedByMove { tile, past_move } => {
                if !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*past_move), context)
                    || !Self::evaluate(&TilePresent { before_moves: *past_move, tile: *tile }, context)
                    || !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*past_move), context) {
                    return true;
                }

                let tile = context.isometry.apply(*tile);
                let past_piece = context.game.past_tile_piece(*past_move, tile).unwrap().unwrap();
                let absolute_move = context.game.move_log().len() - 1 - past_move;

                past_piece.was_affected_by_move(absolute_move)
            },
            PieceInitial { before_moves, tile } => {
                if !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*before_moves), context)
                    || !Self::evaluate(&TilePresent { before_moves: *before_moves, tile: *tile }, context)
                    || !Self::evaluate(&PiecePresent { before_moves: *before_moves, tile: *tile }, context) {
                    return true;
                }

                let tile = context.isometry.apply(*tile);
                let past_piece = context.game.past_tile_piece(*before_moves, tile).unwrap().unwrap();

                past_piece.is_initial()
            },
            PieceTypeIs { before_moves, tile, definition_index } => {
                if !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*before_moves), context)
                    || !Self::evaluate(&TilePresent { before_moves: *before_moves, tile: *tile }, context)
                    || !Self::evaluate(&PiecePresent { before_moves: *before_moves, tile: *tile }, context) {
                    return true;
                }

                let tile = context.isometry.apply(*tile);
                let past_piece = context.game.past_tile_piece(*before_moves, tile).unwrap().unwrap();

                past_piece.definition == *definition_index
            },
            PieceControlledBy { before_moves, tile, player } => {
                if !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*before_moves), context)
                    || !Self::evaluate(&TilePresent { before_moves: *before_moves, tile: *tile }, context)
                    || !Self::evaluate(&PiecePresent { before_moves: *before_moves, tile: *tile }, context) {
                    return true;
                }

                let tile = context.isometry.apply(*tile);
                let past_piece = context.game.past_tile_piece(*before_moves, tile).unwrap().unwrap();

                past_piece.owner == *player
            },
            PieceControlledByAlly { before_moves, tile } => {
                if !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*before_moves), context)
                    || !Self::evaluate(&TilePresent { before_moves: *before_moves, tile: *tile }, context)
                    || !Self::evaluate(&PiecePresent { before_moves: *before_moves, tile: *tile }, context) {
                    return true;
                }

                let tile = context.isometry.apply(*tile);
                let past_piece = context.game.past_tile_piece(*before_moves, tile).unwrap().unwrap();

                past_piece.owner == context.current_player
            },
            PieceControlledByEnemy { before_moves, tile } => {
                if !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*before_moves), context)
                    || !Self::evaluate(&TilePresent { before_moves: *before_moves, tile: *tile }, context)
                    || !Self::evaluate(&PiecePresent { before_moves: *before_moves, tile: *tile }, context) {
                    return true;
                }

                let tile = context.isometry.apply(*tile);
                let past_piece = context.game.past_tile_piece(*before_moves, tile).unwrap().unwrap();

                past_piece.owner != context.current_player
            },
            UnfinishedMoveLegal => {
                // Unwrap safety: The last move is the unfinished move currently being generated.
                let mv = context.game.move_log().moves().last().unwrap();

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
