use std::fmt::Debug;
use std::collections::BTreeMap;
use crate::*;

/// A move is made up of a normalized state delta and a final tile which is used for selecting this
/// move in UI.
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Move<G: BoardGeometry> {
    pub(crate) delta: ReversibleGameStateDelta<G>,
    pub(crate) final_tile: <G as BoardGeometryExt>::Tile,
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

    pub fn push_affecting_move(&mut self, move_index: usize) {
        self.delta.push_affecting_move(move_index)
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
    pub fn move_type(&self) -> &MoveType {
        &self.move_type
    }

    pub fn forward(&self) -> &GameStateDelta<G> {
        &self.forward
    }

    pub fn backward(&self) -> &GameStateDelta<G> {
        &self.backward
    }

    pub fn set_next_player(&mut self, next_player: PlayerIndex) {
        self.forward.next_player = next_player;
    }

    pub fn push_affecting_move(&mut self, move_index: usize) {
        self.forward.push_affecting_move(move_index);
    }

    pub fn into_forward(self) -> GameStateDelta<G> {
        self.forward
    }

    pub fn into_backward(self) -> GameStateDelta<G> {
        self.backward
    }
}

/// An operation applicable to a [`GameState`], producing an altered [`GameState`].
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct GameStateDelta<G: BoardGeometry> {
    pub(crate) affected_pieces: BTreeMap<<G as BoardGeometryExt>::Tile, Option<Piece<G>>>,
    pub(crate) affected_flags: BTreeMap<<G as BoardGeometryExt>::Tile, BTreeMap<u32, bool>>,
    // TODO: Reconsider whether this is necessary:
    pub(crate) next_player: PlayerIndex,
}

impl<G: BoardGeometry> GameStateDelta<G> {
    pub fn with_next_player(next_player: PlayerIndex) -> Self {
        Self {
            affected_pieces: Default::default(),
            affected_flags: Default::default(),
            next_player,
        }
    }

    pub fn push_affecting_move(&mut self, move_index: usize) {
        for piece in self.affected_pieces.values_mut() {
            if let Some(piece) = piece.as_mut() {
                piece.push_affecting_move(move_index);
            }
        }
    }

    pub fn set_next_player(&mut self, next_player: PlayerIndex) {
        self.next_player = next_player;
    }

    pub fn set(&mut self, tile: <G as BoardGeometryExt>::Tile, mut piece: Option<Piece<G>>) {
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
