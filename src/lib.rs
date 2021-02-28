// #![feature(min_const_generics)]
// #![feature(const_generics)]
// #![feature(const_evaluatable_checked)]
// #![feature(const_fn)]
// #![feature(inline_const)]
#![feature(trait_alias)]

use std::num::NonZeroUsize;
use std::collections::{HashMap, VecDeque};

use math::*;
use board::*;
use piece::*;

pub mod math;
pub mod board;
pub mod piece;

#[derive(Clone, Debug)]
pub struct Game<G: BoardGeometry> {
    pub(crate) board: Board<G>,
    pub(crate) piece_set: PieceSet<G>,
    pub(crate) players: NonZeroUsize,
}

#[derive(Clone, Default, Debug)]
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

    pub fn moves(&self, game: &Game<G>, original_game_state: &GameState<G>, tile: <G as BoardGeometryExt>::Tile) -> Result<Vec<Move<G>>, ()> {
        let tile_ref = self.tile(&game.board, tile.clone()).ok_or(())?;
        let piece = tile_ref.get_piece().ok_or(())?.clone();
        let definition = piece.get_definition(&game.piece_set);

        struct QueueItem<G2: BoardGeometry> {
            tile: <G2 as BoardGeometryExt>::Tile,
            delta: GameStateDelta<G2>,
            state_index: usize,
        }

        // FIXME: should not contain duplicates
        let mut valid_moves = Vec::new();
        let mut queue = VecDeque::<QueueItem<G>>::new();

        for initial_state_index in &*definition.initial_states {
            queue.push_back(QueueItem {
                tile: tile.clone(),
                delta: Default::default(),
                state_index: *initial_state_index,
            });
        }

        // DFS the moves
        while let Some(QueueItem { tile, mut delta, state_index }) = queue.pop_front() {
            let state = &definition.states[state_index];
            let game_state = original_game_state.clone().apply(delta.clone());
            let piece = game_state.tile(&game.board, tile.clone()).ok_or(())?.get_piece().ok_or(())?.clone();
            let isometry = Isometry {
                axis_permutation: piece.transformation.clone(),
                translate: tile.clone(),
            };

            let moves: Vec<(_, _)> = match state.action.clone() {
                Action::Move { condition, actions, move_choices } => {
                    if !condition.evaluate(game, &game_state, &isometry) {
                        continue;
                    }

                    for action in actions {
                        match action {
                            ActionEnum::SetTile { target, piece } => {
                                let target = isometry.apply(target);

                                delta.affected_pieces.insert(target, piece);
                            },
                            ActionEnum::CopyTile { source, target } => {
                                let source = isometry.apply(source);
                                let target = isometry.apply(target);
                                let piece = game_state.tile(&game.board, source).and_then(|tile| tile.get_piece().cloned());

                                delta.affected_pieces.insert(target, piece);
                            },
                        }
                    }

                    move_choices.into_iter().filter_map(|move_choice| {
                        if let Some(move_choice) = move_choice {
                            let move_choice = isometry.apply(move_choice);
                            let mut delta = delta.clone();

                            delta.affected_pieces.insert(move_choice.clone(), Some(piece.clone()));

                            Some((move_choice, delta))
                        } else {
                            None
                        }
                    }).collect()
                },
                Action::Symmetry { symmetries } => {
                    symmetries.into_iter().map(|axis_permutation| {
                        let mut delta = delta.clone();
                        let mut piece = piece.clone();
                        piece.transformation = axis_permutation * piece.transformation;

                        delta.affected_pieces.insert(tile.clone(), Some(piece));

                        (tile.clone(), delta)
                    }).collect()
                },
            };

            for successor_state_index in &*state.successor_indices {
                for (tile, delta) in &moves {
                    queue.push_back(QueueItem {
                        tile: tile.clone(),
                        delta: delta.clone(),
                        state_index: *successor_state_index,
                    });

                    if state.is_final {
                        valid_moves.push(Move {
                            delta: delta.clone(),
                            final_tile: tile.clone(),
                        });
                    }
                }
            }
        }

        Ok(valid_moves)
    }

    pub fn apply(self, delta: GameStateDelta<G>) -> Self {
        delta.apply_to(self)
    }
}

#[derive(Clone, Debug)]
pub struct Move<G: BoardGeometry> {
    delta: GameStateDelta<G>,
    final_tile: <G as BoardGeometryExt>::Tile,
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct GameStateDelta<G: BoardGeometry> {
    affected_pieces: HashMap<<G as BoardGeometryExt>::Tile, Option<Piece<G>>>,
}

impl<G: BoardGeometry> GameStateDelta<G> {
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
    // game: &'a Game<G>,
    pieces: &'a HashMap<<G as BoardGeometryExt>::Tile, Piece<G>>,
}

impl<G: BoardGeometry> TileRef<'_, G> {
    pub fn get_piece(&self) -> Option<&Piece<G>> {
        self.pieces.get(&self.tile)
    }
}

pub struct TileRefMut<'a, G: BoardGeometry> {
    tile: <G as BoardGeometryExt>::Tile,
    // game: &'a mut Game<G>,
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

        let mut game = Game {
            board: Board {
                tiles: {
                    let mut tiles = BoardTiles::empty();

                    for y in 0..8 {
                        for x in 0..8 {
                            tiles.set([x, y].into(), true);
                        }
                    }

                    for y in -8..0 {
                        for x in 0..8 {
                            let v = x * 3 + y * 2;

                            if v % 5 == 0 || v % 7 == 0 {
                                tiles.set([x, y].into(), true);
                            }
                        }
                    }

                    tiles.set([-1, -1].into(), true);
                    tiles.set([-2, 5].into(), true);
                    tiles.set([-3, 4].into(), true);

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

                        let mut tile = game_state.tile_mut(&game.board, coords).unwrap();

                        tile.set_piece(Some(Piece {
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

        print!("{}", SquareBoardGeometry::print(&game, &game_state));
        // dbg!(game_state);
    }
}
