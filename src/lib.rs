// #![feature(min_const_generics)]
// #![feature(const_generics)]
// #![feature(const_evaluatable_checked)]
// #![feature(const_fn)]
// #![feature(inline_const)]
#![feature(btree_retain)]
#![feature(trait_alias)]
#![feature(format_args_capture)]
#![feature(associated_type_bounds)]
#![feature(const_fn_transmute)]

use math::*;
use board::*;
use piece::*;
use game::*;

pub mod math;
pub mod board;
pub mod piece;
pub mod game;
pub mod games;

#[cfg(test)]
mod tests {
    use std::num::NonZeroUsize;

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

            assert!(TriangularBoardGeometry::is_tile_valid(original));

            for (index, (rotation, expected_result)) in TriangularBoardGeometry::get_rotations().into_iter().zip(expected_results.iter().copied()).enumerate() {
                let rotated = rotation.apply(original);

                assert!(TriangularBoardGeometry::is_tile_valid(rotated));
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

            assert!(SquareBoardGeometry::is_tile_valid(original));

            for (index, (rotation, expected_result)) in SquareBoardGeometry::get_rotations().into_iter().zip(expected_results.iter().copied()).enumerate() {
                let rotated = rotation.apply(original);

                assert!(SquareBoardGeometry::is_tile_valid(rotated));
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

            assert!(HexagonalBoardGeometry::is_tile_valid(original));

            for (index, (rotation, expected_result)) in HexagonalBoardGeometry::get_rotations().into_iter().zip(expected_results.iter().copied()).enumerate() {
                let rotated = rotation.apply(original);

                assert!(HexagonalBoardGeometry::is_tile_valid(rotated));
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
    fn international_chess_castling() {
        use super::*;
        use crate::games::international_chess::tiles::*;

        let mut game = crate::games::international_chess::GAME.clone();

        println!("Initial state:\n{}", SquareBoardGeometry::print(&game));

        let moves: Vec<(IVec2, IVec2)> = vec![
            (B2, B4),
            (A7, A6),
            (B1, C3),
            (B7, B6),
            (C1, A3),
            (C7, C6),
            (E2, E4),
            (D7, D6),
            (D1, H5),
            (E7, E6),
            (F1, A6),
            (F7, F6),
            (G1, F3),
            (G7, G6),
        ];
        let final_move = E1;

        for (tile_from, tile_to) in moves {
            let mv = game.available_moves().moves_from(tile_from)
                .find(|mv| mv.final_tile() == &tile_to)
                .unwrap_or_else(|| panic!("Move {} -> {} is not valid.", tile_from, tile_to))
                .clone();

            game.append(mv).unwrap();
            print!("Move {} -> {}:\n{}", tile_from, tile_to, SquareBoardGeometry::print(&game));
        }

        let options: Vec<_> = game.available_moves().moves_from(final_move).collect();

        assert_eq!(options.len(), 5);
        println!("Options ({len}):", len=options.len());

        for (move_index, mv) in options.into_iter().enumerate() {
            let mut game = game.clone();

            game.append(mv.clone()).unwrap();
            print!("Option #{}:\n{}", move_index + 1, SquareBoardGeometry::print(&game));
        }
    }
}
