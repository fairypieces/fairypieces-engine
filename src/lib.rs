// #![feature(min_const_generics)]
// #![feature(const_generics)]
// #![feature(const_evaluatable_checked)]
// #![feature(const_fn)]
// #![feature(inline_const)]
#![feature(trait_alias)]

use std::ops::Sub;
use std::num::NonZeroUsize;
use std::collections::{HashMap, VecDeque, BTreeSet, BTreeMap};
use replace_with::replace_with_or_default;

use math::*;
use board::*;
use piece::*;

pub mod math;
pub mod board;
pub mod piece;

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

    pub fn append(&mut self, mv: Move<G>) {
        self.move_log.append(mv);
    }

    /// Returns the piece at tile `tile` on the board `before_moves` moves ago.
    pub fn past_tile(&self, before_moves: usize, tile: &<G as BoardGeometryExt>::Tile) -> Result<Option<&Piece<G>>, PastTileError> {
        if before_moves > self.move_log.moves.len() {
            return Err(PastTileError::PrecedingStart);
        }

        if !self.rules.board.tiles.contains(tile) {
            return Err(PastTileError::MissingTile);
        }

        if before_moves > 0 {
            let moves_played_since = &self.move_log.moves[(self.move_log.moves.len() - before_moves)..];

            for mv in moves_played_since {
                if let Some(piece) = mv.backward.affected_pieces.get(tile) {
                    return Ok(piece.as_ref());
                }
            }
        }

        Ok(self.move_log.current_state.pieces.get(tile))
    }

    pub fn moves(&self, tile: <G as BoardGeometryExt>::Tile) -> Result<Box<[Move<G>]>, ()> {
        let tile_ref = self.move_log.current_state.tile(&self.rules.board, tile.clone()).ok_or(())?;
        let piece = tile_ref.get_piece().ok_or(())?.clone();
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
                tile: tile.clone(),
                axis_permutation: Default::default(),
                delta: Default::default(),
                state_index: *initial_state_index,
            });
        }

        // DFS the moves
        while let Some(QueueItem { tile, axis_permutation, mut delta, state_index }) = queue.pop_front() {
            let state = &definition.states[state_index];
            let game_state = self.move_log.current_state.clone().apply(delta.clone());
            let piece = game_state.tile(&self.rules.board, tile.clone()).ok_or(())?.get_piece().ok_or(())?.clone();
            let isometry = Isometry::from(axis_permutation.clone() * piece.transformation.clone()) * Isometry::translation(tile.clone());

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
                                delta.affected_pieces.insert(move_choice.clone(), Some(piece.clone_moved()));
                                delta.affected_pieces.insert(tile.clone(), None);
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

                        (tile.clone(), axis_permutation, delta.clone())
                    }).collect()
                },
            };

            for (tile, _, delta) in &moves {
                if state.is_final {
                    valid_moves.insert(Move::from(
                        &self.move_log.current_state,
                        delta.clone(),
                        tile.clone(),
                    ));
                }
            }

            for successor_state_index in &*state.successor_indices {
                for (tile, axis_permutation, delta) in &moves {
                    queue.push_back(QueueItem {
                        tile: tile.clone(),
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
        board.tiles().contains(&tile).then(move || {
            TileRef {
                tile,
                pieces: &self.pieces,
            }
        })
    }

    pub fn tile_mut(&mut self, board: &Board<G>, tile: <G as BoardGeometryExt>::Tile) -> Option<TileRefMut<'_, G>> {
        board.tiles().contains(&tile).then(move || {
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
        let mut delta: GameStateDelta<G> = Default::default();

        for (key, lhs_value) in &self.pieces {
            if rhs.pieces.get(key).map(|rhs_value| rhs_value != lhs_value).unwrap_or(true) {
                delta.affected_pieces.insert(key.clone(), Some(lhs_value.clone()));
            }
        }

        for (key, rhs_value) in &rhs.pieces {
            if self.pieces.get(key).is_none() {
                delta.affected_pieces.insert(key.clone(), None);
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
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ReversibleGameStateDelta<G: BoardGeometry> {
    forward: GameStateDelta<G>,
    backward: GameStateDelta<G>,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct GameStateDelta<G: BoardGeometry> {
    affected_pieces: BTreeMap<<G as BoardGeometryExt>::Tile, Option<Piece<G>>>,
}

impl<G: BoardGeometry> GameStateDelta<G> {
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
            self.pieces.insert(self.tile.clone(), piece);
        } else {
            self.pieces.remove(&self.tile);
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }

    #[test]
    fn geometries() {
        use super::*;

        // Triangular grid
        {
            let original: IVec3 = [2, -1, 0].into();
            let expected_results: [IVec3; 3] = [
                [ 2, -1,  0].into(),
                [ 0,  2, -1].into(),
                [-1,  0,  2].into(),
            ];

            assert!(TriangularBoardGeometry::is_tile_valid(original.clone()));

            for (index, (rotation, expected_result)) in TriangularBoardGeometry::get_rotations().into_iter().zip(expected_results.iter().cloned()).enumerate() {
                let rotated = rotation.apply(original.clone());

                assert!(TriangularBoardGeometry::is_tile_valid(rotated.clone()));
                assert_eq!(
                    rotated,
                    expected_result,
                    "Applying rotation #{index} on the triangular grid results in an unexpected value",
                    index=index,
                );
            }
        }

        // Square grid
        {
            let original: IVec2 = [2, 1].into();
            let expected_results: [IVec2; 4] = [
                [ 2,  1].into(),
                [-1,  2].into(),
                [-2, -1].into(),
                [ 1, -2].into(),
            ];

            assert!(SquareBoardGeometry::is_tile_valid(original.clone()));

            for (index, (rotation, expected_result)) in SquareBoardGeometry::get_rotations().into_iter().zip(expected_results.iter().cloned()).enumerate() {
                let rotated = rotation.apply(original.clone());

                assert!(SquareBoardGeometry::is_tile_valid(rotated.clone()));
                assert_eq!(
                    rotated,
                    expected_result,
                    "Applying rotation #{index} on the square grid results in an unexpected value",
                    index=index,
                );
            }
        }

        // Hexagonal grid
        {
            let original: IVec3 = [3, -1, -2].into();
            let expected_results: [IVec3; 6] = [
                [ 3, -1, -2].into(),
                [ 1,  2, -3].into(),
                [-2,  3, -1].into(),
                [-3,  1,  2].into(),
                [-1, -2,  3].into(),
                [ 2, -3,  1].into(),
            ];

            assert!(HexagonalBoardGeometry::is_tile_valid(original.clone()));

            for (index, (rotation, expected_result)) in HexagonalBoardGeometry::get_rotations().into_iter().zip(expected_results.iter().cloned()).enumerate() {
                let rotated = rotation.apply(original.clone());

                assert!(HexagonalBoardGeometry::is_tile_valid(rotated.clone()));
                assert_eq!(
                    rotated,
                    expected_result,
                    "Applying rotation #{index} on the triangular grid results in an unexpected value",
                    index=index,
                );
            }
        }
    }

    #[test]
    fn simple() {
        use super::*;

        let mut game_rules = GameRules {
            board: Board {
                tiles: {
                    let mut tiles = BoardTiles::empty();

                    for y in 0..8 {
                        for x in 0..8 {
                            tiles.set([x, y].into(), true);
                        }
                    }

                    tiles
                },
            },
            piece_set: PIECE_SET_INTERNATIONAL.clone(),
            players: NonZeroUsize::new(2).unwrap(),
        };

        let mut game_state = {
            static P: usize = PIECE_INTERNATIONAL_PAWN;
            static R: usize = PIECE_INTERNATIONAL_ROOK;
            static N: usize = PIECE_INTERNATIONAL_KNIGHT;
            static B: usize = PIECE_INTERNATIONAL_BISHOP;
            static Q: usize = PIECE_INTERNATIONAL_QUEEN;
            static K: usize = PIECE_INTERNATIONAL_KING;
            static WHITE_PIECES: [[usize; 8]; 2] = [
                [P, P, P, P, P, P, P, P],
                [R, N, B, Q, K, B, N, R],
            ];

            let mut game_state = GameState::<SquareBoardGeometry>::default();
            let sides: [(usize, Isometry<SquareBoardGeometry>); 2] = [
                (0, Isometry::default()),
                (1, Isometry::from(SquareBoardGeometry::get_reflective_symmetries()[0].clone() * SquareBoardGeometry::get_rotations()[2].clone()) * Isometry::<SquareBoardGeometry>::translation([0, 7].into())),
            ];

            for (player, isometry) in &sides {
                for (y_inv, row) in WHITE_PIECES.iter().enumerate() {
                    let y = 1 - y_inv;

                    for (x, piece) in row.iter().enumerate() {
                        let mut coords: <SquareBoardGeometry as BoardGeometryExt>::Tile = [x as i32, y as i32].into();
                        coords = isometry.apply(coords);

                        // if (x, y) == (3, 1) {
                        //     dbg!(&isometry);
                        //     dbg!(&coords);
                        // }

                        let mut tile = game_state.tile_mut(&game_rules.board, coords).unwrap();

                        tile.set_piece(Some(Piece {
                            initial: true,
                            definition: *piece,
                            owner: *player,
                            transformation: isometry.axis_permutation.clone(),
                            __marker: Default::default(),
                        }));
                    }
                }
            }

            game_state
        };

        let mut game = Game::new(game_rules, game_state);

        print!("Initial state:\n{}", SquareBoardGeometry::print(&game));
        println!();

        // dbg!(game_state.moves(&game, [3, 1].into()).unwrap());

        let moves: Vec<(IVec2, usize)> = vec![
            ([1, 1].into(), 0),
            ([1, 0].into(), 0),
            ([2, 0].into(), 0),
            ([4, 1].into(), 0),
            ([3, 0].into(), 4),
            ([5, 0].into(), 0),
            ([6, 0].into(), 0),
        ];
        // let final_move: IVec2 = [3, 5].into();
        let final_move: IVec2 = [4, 0].into();

        for (tile, move_index) in moves {
            let mv = game.moves(tile).unwrap()[move_index].clone();
            game.append(mv);

            print!("Move #{}:\n{}", move_index, SquareBoardGeometry::print(&game));
        }

        println!();

        for (move_index, mv) in game.moves(final_move).unwrap().into_iter().enumerate() {
            let mut game = game.clone();

            game.append(mv.clone());
            print!("Option #{}:\n{}", move_index, SquareBoardGeometry::print(&game));
        }
    }
}
