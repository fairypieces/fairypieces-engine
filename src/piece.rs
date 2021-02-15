use std::collections::BTreeSet;
use std::marker::PhantomData;
use crate::math::*;
use crate::board::*;

/// A set of pieces whose movesets may refer to each other.
pub struct PieceSet<G: BoardGeometry> {
    definitions: Box<[PieceDefinition<G>]>,
}

/// A piece with determined rotation and chirality.
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Piece<G: BoardGeometry> {
    rotation_index: usize,
    /// Only effective for chiral piece definitions.
    flip: bool,
    /// The index of the `PieceDefinition` within the `PieceSet`.
    definition: usize,
    __marker: PhantomData<G>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct PieceDefinition<G: BoardGeometry> {
    states: Box<[State<G>]>,
    initial_states: Box<[usize]>,
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Hash, Clone, Debug)]
pub enum ConditionEnum<G: BoardGeometry> {
    /// A boolean OR operation or "disjunction". Evaluates to `true` if any of the inner conditions
    /// evaluate to `true`.
    Any(Box<[ConditionEnum<G>]>),
    /// A boolean AND operation or "conjunction". Evaluates to `true` if all of the inner conditions
    /// evaluate to `true`.
    All(Box<[ConditionEnum<G>]>),
    /// Evaluates to `true` if the inner condition evaluates to `false` and vice versa.
    Not(Box<ConditionEnum<G>>),
    /// Evaluates to `true` if a tile with those coordinates is present.
    TilePresent(<G as BoardGeometryExt>::Tile),
    /// Evaluates to `true` if the tile at the given coordinates matches the type.
    TileTypeIs(<G as BoardGeometryExt>::Tile, usize),
    /// Evaluates to `true` if a piece is present on the given tile.
    PiecePresent(<G as BoardGeometryExt>::Tile),
    /// Evaluates to `true` if there is no piece on the tile or if the piece is allied to the
    /// current player.
    PieceControlledByAlly(<G as BoardGeometryExt>::Tile),
    /// Evaluates to `true` if there is no piece on the tile or if the piece is an enemy to the
    /// current player.
    PieceControlledByEnemy(<G as BoardGeometryExt>::Tile),
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

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub struct State<G: BoardGeometry> {
    action: Action<G>,
    successor_indices: Box<[usize]>,
    is_final: bool,
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum Action<G: BoardGeometry> {
    Move {
        /// All of these conditions must be satisfied, otherwise the traversal is terminated.
        conditions: BTreeSet<ConditionEnum<G>>,
        /// All of these actions are performed.
        actions: BTreeSet<ActionEnum<G>>,
        /// Any of these moves is performed.
        move_choices: BTreeSet<Option<<G as BoardGeometryExt>::Tile>>,
    },
    Symmetry {
        symmetries: Vec<AxisPermutation<G>>,
    },
}
