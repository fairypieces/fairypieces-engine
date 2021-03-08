use std::collections::BTreeSet;
use std::marker::PhantomData;
use lazy_static::lazy_static;
use crate::{GameRules, GameState, Game};
use crate::math::*;
use crate::board::*;

/// A set of pieces whose movesets may refer to each other.
#[derive(Clone, Debug)]
pub struct PieceSet<G: BoardGeometry> {
    definitions: Box<[PieceDefinition<G>]>,
}

impl<G: BoardGeometry> PieceSet<G> {
    /// Validates the pieces and creates a piece set.
    pub fn from(definitions: impl IntoIterator<Item=PieceDefinitionUnvalidated<G>>) -> Result<Self, ()> {
        let mut definitions = definitions.into_iter().collect::<Vec<_>>();

        for definition in &definitions {

            for state in &definition.states {
                state.check_validity(&definition, &definitions)?;
            }
        }

        let definitions = definitions.into_iter().map(PieceDefinitionUnvalidated::assume_validated).collect();

        Ok(Self {
            definitions,
        })
    }
}

/// A piece with determined rotation and chirality.
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Piece<G: BoardGeometry> {
    pub(crate) transformation: AxisPermutation<G>,
    // pub(crate) rotation_index: usize,
    // /// Only effective for chiral piece definitions.
    // pub(crate) flip: bool,
    // /// The index of the `PieceDefinition` within the `PieceSet`.
    /// The index of the piece definition.
    pub(crate) definition: usize,
    /// The player who owns this piece.
    pub(crate) owner: usize,
    /// `true` if the piece has not been moved since the beginning of the game, `false` otherwise.
    pub(crate) initial: bool,
    pub(crate) __marker: PhantomData<G>,
}

impl<G: BoardGeometry> Piece<G> {
    fn check_validity(&self, piece: &PieceDefinitionUnvalidated<G>, definitions: &[PieceDefinitionUnvalidated<G>]) -> Result<(), ()> {
        // TODO validate owner (team)?
        if self.definition >= definitions.len() {
            return Err(());
        }

        Ok(())
    }

    pub fn get_definition<'a>(&'_ self, piece_set: &'a PieceSet<G>) -> &'a PieceDefinition<G> {
        &piece_set.definitions[self.definition]
    }

    pub fn clone_moved(&self) -> Self {
        Self {
            transformation: self.transformation.clone(),
            definition: self.definition.clone(),
            owner: self.owner.clone(),
            initial: false,
            __marker: Default::default(),
        }
    }
}

#[derive(Default, Debug, Clone, Hash, PartialEq, Eq)]
pub struct PieceDefinitionUnvalidated<G: BoardGeometry> {
    pub title: String,
    pub states: Vec<StateUnvalidated<G>>,
    pub initial_states: Vec<usize>,
}

impl<G: BoardGeometry> PieceDefinitionUnvalidated<G> {
    pub fn new(title: impl ToString) -> Self {
        Self {
            title: title.to_string(),
            states: Default::default(),
            initial_states: Default::default(),
        }
    }

    pub fn with_initial_state(mut self, state: StateUnvalidated<G>) -> Self {
        self.states.push(state);
        self.initial_states.push(self.states.len() - 1);
        self
    }

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
        }
    }

    fn validate(self, definitions: &[PieceDefinitionUnvalidated<G>]) -> Result<PieceDefinition<G>, ()> {
        self.check_validity(definitions).map(move |()| {
            self.assume_validated()
        })
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct PieceDefinition<G: BoardGeometry> {
    pub(crate) title: String,
    pub(crate) states: Box<[State<G>]>,
    pub(crate) initial_states: Box<[usize]>,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Hash, Clone, Debug)]
pub enum ConditionEnum<G: BoardGeometry> {
    /// A boolean OR operation or "disjunction". Evaluates to `true` if any of the inner conditions
    /// evaluate to `true`.
    Any(BTreeSet<ConditionEnum<G>>),
    /// A boolean AND operation or "conjunction". Evaluates to `true` if all of the inner conditions
    /// evaluate to `true`.
    All(BTreeSet<ConditionEnum<G>>),
    /// Evaluates to `true` if the inner condition evaluates to `false` and vice versa.
    Not(Box<ConditionEnum<G>>),
    /// Evaluates to `true` if the number of moves played during the game is greater than or equal
    /// to the inner value.
    MovesPlayedGreaterThanOrEqual(usize),
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
    /// Evaluates to `true` if a piece is present on the given tile.
    PiecePresent {
        before_moves: usize,
        tile: <G as BoardGeometryExt>::Tile,
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
        definition_index: usize,
    },
    PieceChiralityIs {
        before_moves: usize,
        tile: <G as BoardGeometryExt>::Tile,
        chirality: bool,
    },
    PieceRotationIs {
        before_moves: usize,
        tile: <G as BoardGeometryExt>::Tile,
        rotation_index: usize,
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
}

impl<G: BoardGeometry> ConditionEnum<G> {
    pub fn any(conditions: impl IntoIterator<Item=ConditionEnum<G>>) -> Self {
        ConditionEnum::Any(conditions.into_iter().collect())
    }

    pub fn all(conditions: impl IntoIterator<Item=ConditionEnum<G>>) -> Self {
        ConditionEnum::All(conditions.into_iter().collect())
    }

    pub fn evaluate(&self, game: &Game<G>, isometry: &Isometry<G>) -> bool {
        use ConditionEnum::*;
        match self {
            Any(children) => {
                children.iter().any(|child| child.evaluate(game, isometry))
            },
            All(children) => {
                children.iter().all(|child| child.evaluate(game, isometry))
            },
            Not(child) => !child.evaluate(game, isometry),
            MovesPlayedGreaterThanOrEqual(moves) => {
                game.move_log.moves.len() >= *moves
            },
            TilePresent { before_moves, tile } => {
                if !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*before_moves), game, isometry) {
                    return true;
                }

                let tile = isometry.apply(tile.clone());
                game.past_tile(*before_moves, &tile).is_ok()
            },
            TileTypeIs { before_moves, tile, type_index } => {
                if !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*before_moves), game, isometry)
                    || !Self::evaluate(&TilePresent { before_moves: *before_moves, tile: tile.clone() }, game, isometry) {
                    return true;
                }

                let tile = isometry.apply(tile.clone());
                <G as BoardGeometry>::get_tile_type(tile.clone()) == *type_index
            },
            PiecePresent { before_moves, tile } => {
                if !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*before_moves), game, isometry)
                    || !Self::evaluate(&TilePresent { before_moves: *before_moves, tile: tile.clone() }, game, isometry) {
                    return true;
                }

                let tile = isometry.apply(tile.clone());
                let past_piece = game.past_tile(*before_moves, &tile).unwrap();

                past_piece.is_some()
            },
            PieceInitial { before_moves, tile } => {
                if !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*before_moves), game, isometry)
                    || !Self::evaluate(&TilePresent { before_moves: *before_moves, tile: tile.clone() }, game, isometry)
                    || !Self::evaluate(&PiecePresent { before_moves: *before_moves, tile: tile.clone() }, game, isometry) {
                    return true;
                }

                let tile = isometry.apply(tile.clone());
                let past_piece = game.past_tile(*before_moves, &tile).unwrap().unwrap();

                past_piece.initial
            },
            PieceTypeIs { before_moves, tile, definition_index } => {
                if !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*before_moves), game, isometry)
                    || !Self::evaluate(&TilePresent { before_moves: *before_moves, tile: tile.clone() }, game, isometry)
                    || !Self::evaluate(&PiecePresent { before_moves: *before_moves, tile: tile.clone() }, game, isometry) {
                    return true;
                }

                let tile = isometry.apply(tile.clone());
                let past_piece = game.past_tile(*before_moves, &tile).unwrap().unwrap();

                past_piece.definition == *definition_index
            },
            PieceChiralityIs { before_moves, tile, chirality } => todo!(),
            PieceRotationIs { before_moves, tile, rotation_index } => todo!(),
            PieceControlledByAlly { before_moves, tile } => {
                if !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*before_moves), game, isometry)
                    || !Self::evaluate(&TilePresent { before_moves: *before_moves, tile: tile.clone() }, game, isometry)
                    || !Self::evaluate(&PiecePresent { before_moves: *before_moves, tile: tile.clone() }, game, isometry) {
                    return true;
                }

                let tile = isometry.apply(tile.clone());
                let past_piece = game.past_tile(*before_moves, &tile).unwrap().unwrap();

                past_piece.owner == game.move_log.current_state.currently_playing_player_index
            },
            PieceControlledByEnemy { before_moves, tile } => {
                if !Self::evaluate(&MovesPlayedGreaterThanOrEqual(*before_moves), game, isometry)
                    || !Self::evaluate(&TilePresent { before_moves: *before_moves, tile: tile.clone() }, game, isometry)
                    || !Self::evaluate(&PiecePresent { before_moves: *before_moves, tile: tile.clone() }, game, isometry) {
                    return true;
                }

                let tile = isometry.apply(tile.clone());
                let past_piece = game.past_tile(*before_moves, &tile).unwrap().unwrap();

                past_piece.owner != game.move_log.current_state.currently_playing_player_index
            },
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct StateUnvalidated<G: BoardGeometry> {
    pub(crate) action: Action<G>,
    pub(crate) successor_indices: Vec<usize>,
    pub(crate) is_final: bool,
}

impl<G: BoardGeometry> StateUnvalidated<G> {
    pub fn new(action: Action<G>) -> Self {
        Self {
            action,
            successor_indices: Vec::new(),
            is_final: false,
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
        }
    }

    pub fn validate(self, piece: &PieceDefinitionUnvalidated<G>, definitions: &[PieceDefinitionUnvalidated<G>]) -> Result<State<G>, ()> {
        self.check_validity(piece, definitions).map(move |()| {
            self.assume_validated()
        })
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct State<G: BoardGeometry> {
    pub(crate) action: Action<G>,
    pub(crate) successor_indices: Box<[usize]>,
    pub(crate) is_final: bool,
}

impl<G: BoardGeometry> State<G> {
}

/// The ordering of variants is used for the order of evaluation.
#[derive(PartialOrd, Ord, PartialEq, Eq, Hash, Clone, Debug)]
pub enum ActionEnum<G: BoardGeometry> {
    /// Replace the contents of the target tile with a specific piece or clear the target tile.
    SetTile {
        /// The target tile to change the contents of.
        target: <G as BoardGeometryExt>::Tile,
        /// The index of the piece within the `PieceSet` to place on the tile
        /// or `None` to clear the tile.
        piece: Option<Piece<G>>,
    },
    /// Replace the contents of the target tile with the contents of the source tile.
    CopyTile {
        /// The source tile to copy the contents from.
        source: <G as BoardGeometryExt>::Tile,
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
                    piece.check_validity(piece_definition, definitions)?;
                }
            },
            CopyTile { .. } => (),
        }

        Ok(())
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum Action<G: BoardGeometry> {
    Move {
        /// All of these conditions must be satisfied, otherwise the traversal is terminated.
        condition: ConditionEnum<G>,
        /// All of these actions are performed.
        actions: BTreeSet<ActionEnum<G>>,
        // FIXME: What would `None` represent?
        // FIXME: Change to Isometry?
        /// Any of these moves is performed.
        move_choices: BTreeSet<Option<<G as BoardGeometryExt>::Tile>>,
    },
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
