use std::collections::BTreeSet;
use std::marker::PhantomData;
use lazy_static::lazy_static;
use crate::{Game, GameState};
use crate::math::*;
use crate::board::*;

lazy_static! {
    pub static ref PIECE_SET_INTERNATIONAL: PieceSet<SquareBoardGeometry> = {
        let definitions = vec![
            PieceDefinitionUnvalidated::new("Pawn"),
            PieceDefinitionUnvalidated::new("Rook"),
            PieceDefinitionUnvalidated::new("Knight"),
            PieceDefinitionUnvalidated::new("Bishop"),
            PieceDefinitionUnvalidated::new("Queen"),
            PieceDefinitionUnvalidated::new("King"),
        ];

        PieceSet::from(definitions).unwrap()
    };
}

pub static PIECE_INTERNATIONAL_PAWN: usize   = 0;
pub static PIECE_INTERNATIONAL_ROOK: usize   = 1;
pub static PIECE_INTERNATIONAL_KNIGHT: usize = 2;
pub static PIECE_INTERNATIONAL_BISHOP: usize = 3;
pub static PIECE_INTERNATIONAL_QUEEN: usize  = 4;
pub static PIECE_INTERNATIONAL_KING: usize   = 5;

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
    pub(crate) definition: usize,
    pub(crate) owner: usize,
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
}

#[derive(Default, Debug, Clone, Hash, PartialEq, Eq)]
pub struct PieceDefinitionUnvalidated<G: BoardGeometry> {
    pub title: String,
    pub states: Vec<State<G>>,
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

    pub fn with_initial_state(mut self, state: State<G>) -> Self {
        self.states.push(state);
        self.initial_states.push(self.states.len() - 1);
        self
    }

    pub fn with_state(mut self, state: State<G>) -> Self {
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
        // Ensure initial state order is invariant
        self.initial_states.sort_unstable();

        PieceDefinition {
            title: self.title,
            states: self.states.into_boxed_slice(),
            initial_states: self.initial_states.into_boxed_slice(),
        }
    }

    fn validate(mut self, definitions: &[PieceDefinitionUnvalidated<G>]) -> Result<PieceDefinition<G>, ()> {
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
    /// Evaluates to `true` if a tile with those coordinates is present.
    TilePresent(<G as BoardGeometryExt>::Tile),
    /// Evaluates to `true` if the tile at the given coordinates matches the type.
    TileTypeIs(<G as BoardGeometryExt>::Tile, usize),
    /// Evaluates to `true` if a piece is present on the given tile.
    PiecePresent(<G as BoardGeometryExt>::Tile),
    PieceChiralityIs(<G as BoardGeometryExt>::Tile, bool),
    PieceRotationIs(<G as BoardGeometryExt>::Tile, usize),
    /// Evaluates to `true` if there is no piece on the tile or if the piece is allied to the
    /// current player.
    PieceControlledByAlly(<G as BoardGeometryExt>::Tile),
    /// Evaluates to `true` if there is no piece on the tile or if the piece is an enemy to the
    /// current player.
    PieceControlledByEnemy(<G as BoardGeometryExt>::Tile),
}

impl<G: BoardGeometry> ConditionEnum<G> {
    pub fn evaluate(&self, game: &Game<G>, state: &GameState<G>, isometry: &Isometry<G>) -> bool {
        use ConditionEnum::*;
        match self {
            Any(children) => {
                children.iter().any(|child| child.evaluate(game, state, isometry))
            },
            All(children) => {
                children.iter().all(|child| child.evaluate(game, state, isometry))
            },
            Not(child) => !child.evaluate(game, state, isometry),
            TilePresent(tile) => {
                state.tile(&game.board, tile.clone()).is_some()
            },
            TileTypeIs(tile, ty_index) => {
                let tile = isometry.apply(tile.clone());
                !state.tile(&game.board, tile.clone()).is_some()
                    || <G as BoardGeometry>::get_tile_type(tile.clone()) == *ty_index
            },
            PiecePresent(tile) => {
                let tile = isometry.apply(tile.clone());
                state.tile(&game.board, tile.clone()).map(|tile| tile.get_piece().is_some()).unwrap_or(true)
            },
            PieceChiralityIs(tile, chirality) => todo!(),
            PieceRotationIs(tile, rotation_index) => todo!(),
            PieceControlledByAlly(tile) => {
                let tile = isometry.apply(tile.clone());
                state.tile(&game.board, tile.clone())
                    .and_then(|tile| tile.get_piece().cloned())
                    .map(|piece| piece.owner == state.currently_playing_player_index)
                    .unwrap_or(true)
            },
            PieceControlledByEnemy(tile) => {
                let tile = isometry.apply(tile.clone());
                state.tile(&game.board, tile.clone())
                    .and_then(|tile| tile.get_piece().cloned())
                    .map(|piece| piece.owner != state.currently_playing_player_index)
                    .unwrap_or(true)
            },
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct State<G: BoardGeometry> {
    pub(crate) action: Action<G>,
    pub(crate) successor_indices: Box<[usize]>,
    pub(crate) is_final: bool,
}

impl<G: BoardGeometry> State<G> {
    fn check_validity(&self, piece: &PieceDefinitionUnvalidated<G>, definitions: &[PieceDefinitionUnvalidated<G>]) -> Result<(), ()> {
        for successor_index in &*self.successor_indices {
            if *successor_index >= piece.states.len() {
                return Err(());
            }
        }

        Ok(())
    }
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
        symmetries: Vec<AxisPermutation<G>>,
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
