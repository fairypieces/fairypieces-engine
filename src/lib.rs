// #![feature(adt_const_params)]
// #![feature(generic_const_exprs)]
// #![feature(inline_const)]
#![feature(trait_alias)]
#![feature(associated_type_bounds)]

use board::*;
use game::*;
use math::*;
use piece::*;

pub mod board;
pub mod delta;
pub mod game;
pub mod games;
pub mod math;
pub mod piece;
mod util;
pub mod victory_conditions;

#[cfg(test)]
mod tests {

    #[test]
    fn geometries() {
        use super::*;

        // Triangular grid
        {
            let original: IVec3 = [2, -1, 0].into();
            let expected_results: [IVec3; 3] =
                [[2, -1, 0].into(), [0, 2, -1].into(), [-1, 0, 2].into()];

            assert!(TriangularBoardGeometry::is_tile_valid(original));

            for (index, (rotation, expected_result)) in TriangularBoardGeometry::get_rotations()
                .into_iter()
                .zip(expected_results.iter().copied())
                .enumerate()
            {
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
                [2, 1].into(),
                [-1, 2].into(),
                [-2, -1].into(),
                [1, -2].into(),
            ];

            assert!(SquareBoardGeometry::is_tile_valid(original));

            for (index, (rotation, expected_result)) in SquareBoardGeometry::get_rotations()
                .into_iter()
                .zip(expected_results.iter().copied())
                .enumerate()
            {
                let rotated = rotation.apply(original);

                assert!(SquareBoardGeometry::is_tile_valid(rotated));
                assert_eq!(
                    rotated,
                    expected_result,
                    "Applying rotation #{index} on the square grid results in an unexpected value",
                    index = index,
                );
            }
        }

        // Hexagonal grid
        {
            let original: IVec3 = [3, -1, -2].into();
            let expected_results: [IVec3; 6] = [
                [3, -1, -2].into(),
                [1, 2, -3].into(),
                [-2, 3, -1].into(),
                [-3, 1, 2].into(),
                [-1, -2, 3].into(),
                [2, -3, 1].into(),
            ];

            assert!(HexagonalBoardGeometry::is_tile_valid(original));

            for (index, (rotation, expected_result)) in HexagonalBoardGeometry::get_rotations()
                .into_iter()
                .zip(expected_results.iter().copied())
                .enumerate()
            {
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
            (G7, G6),
            (G1, F3),
            (H7, H6),
        ];
        let final_move = E1;

        for (tile_from, tile_to) in moves {
            let mv = game
                .available_moves()
                .moves_from(tile_from)
                .find(|mv| mv.final_tile() == &tile_to)
                .unwrap_or_else(|| panic!("Move {} -> {} is not valid.", tile_from, tile_to))
                .clone();

            game.append(mv).unwrap();
            print!(
                "Move {} -> {}:\n{}",
                tile_from,
                tile_to,
                SquareBoardGeometry::print(&game)
            );
        }

        let options: Vec<_> = game.available_moves().moves_from(final_move).collect();

        assert_eq!(options.len(), 5);
        println!("Options ({len}):", len = options.len());

        for (move_index, mv) in options.into_iter().enumerate() {
            let mut game = game.clone();

            game.append(mv.clone()).unwrap();
            print!(
                "Option #{}:\n{}",
                move_index + 1,
                SquareBoardGeometry::print(&game)
            );
        }
    }
}
