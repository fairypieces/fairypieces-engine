use std::ops::Sub;
use std::num::NonZeroUsize;
use std::collections::{HashMap, VecDeque, BTreeSet, BTreeMap, HashSet};
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

    pub fn append(&mut self, mv: Move<G>) {
        self.moves.push(mv.delta.clone());
        replace_with_or_default(&mut self.current_state, move |current_state| current_state.apply(mv.delta.forward));

        // Ensure all appended moves are reversible
        debug_assert!(self.initial_state == self.moves.iter().rev().fold(self.current_state.clone(), |state, mv| state.apply(mv.backward.clone())));
    }
}

#[derive(Clone, Debug)]
pub struct Game<G: BoardGeometry> {
    pub(crate) rules: GameRules<G>,
    pub(crate) move_log: MoveLog<G>,
}

impl<G: BoardGeometry> Game<G> {
    pub fn new(rules: GameRules<G>, initial_state: GameState<G>) -> Self {
        Self {
            rules,
            move_log: MoveLog::new(initial_state),
        }
    }

    pub fn get_available_moves(&self) -> HashSet<Move<G>> {
        let mut result = HashSet::new();
        let current_player = self.move_log.current_state.currently_playing_player_index;

        for (tile, piece) in &self.move_log.current_state.pieces {
            if piece.owner == current_player {
                if let Ok(available_moves) = self.moves_from_tile(*tile) {
                    result.extend(available_moves);
                }
            }
        }

        result
    }

    pub fn get_available_deltas(&self) -> HashSet<ReversibleGameStateDelta<G>> {
        self.get_available_moves().into_iter().map(|mv| mv.delta).collect()
    }

    /// Returns `true` if there exists a valid move with a matching `delta` and `final_tile`.
    pub fn is_move_available(&self, mv: &Move<G>) -> bool {
        self.get_available_moves().contains(mv)
    }

    /// Returns `true` if there exists a valid move with a matching `delta`. Does not check
    /// `Move::final_tile` as `Self::is_move_available()` does.
    pub fn is_delta_available(&self, delta: &ReversibleGameStateDelta<G>) -> bool {
        self.get_available_deltas().contains(delta)
    }

    #[must_use = "The move may not be available."]
    pub fn append(&mut self, mv: Move<G>) -> Result<(), ()> {
        self.is_move_available(&mv).then(|| {
            self.move_log.append(mv);
        }).ok_or(())

        // if self.is_move_available(&mv) {
        //     self.move_log.append(mv);
        //     Ok(())
        // } else {
        //     Err(())
        // }
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

    pub fn moves_from_tile(&self, tile: <G as BoardGeometryExt>::Tile) -> Result<HashSet<Move<G>>, ()> {
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

        let mut valid_moves = BTreeSet::new();
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
                    valid_moves.insert(Move::from(
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

        Ok(valid_moves.into_iter().collect())
    }
}

#[derive(Clone, Debug)]
pub struct GameRules<G: BoardGeometry> {
    pub(crate) board: Board<G>,
    pub(crate) piece_set: PieceSet<G>,
    pub(crate) players: NonZeroUsize,
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

    pub fn delta(&self) -> &ReversibleGameStateDelta<G> {
        &self.delta
    }

    pub fn final_tile(&self) -> &<G as BoardGeometryExt>::Tile {
        &self.final_tile
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ReversibleGameStateDelta<G: BoardGeometry> {
    forward: GameStateDelta<G>,
    backward: GameStateDelta<G>,
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
