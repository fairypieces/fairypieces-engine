// #![feature(min_const_generics)]
// #![feature(const_generics)]
// #![feature(const_evaluatable_checked)]
// #![feature(const_fn)]
// #![feature(inline_const)]
#![feature(trait_alias)]

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

        let mut game = crate::games::international_chess::GAME.clone();

        print!("Initial state:\n{}", SquareBoardGeometry::print(&game));
        println!();

        // dbg!(game_state.moves(&game, [3, 1].into()).unwrap());

        let moves: Vec<(IVec2, IVec2)> = vec![
            ([1, 1], [1, 3]),
            ([0, 6], [0, 5]),
            ([1, 0], [2, 2]),
            ([1, 6], [1, 5]),
            ([2, 0], [0, 2]),
            ([2, 6], [2, 5]),
            ([4, 1], [4, 3]),
            ([3, 6], [3, 5]),
            ([3, 0], [7, 4]),
            ([4, 6], [4, 5]),
            ([5, 0], [0, 5]),
            ([5, 6], [5, 5]),
            ([6, 0], [5, 2]),
            ([6, 6], [6, 5]),
        ].into_iter().map(|(from, to)| (from.into(), to.into())).collect();
        let final_move: IVec2 = [4, 0].into();

        for (tile_from, tile_to) in moves {
            // dbg!(game.moves_from_tile(tile_from.clone()).unwrap());
            let mv = game.moves_from_tile(tile_from.clone()).unwrap().into_iter()
                .find(|mv| mv.final_tile() == &tile_to)
                .unwrap_or_else(|| panic!("Move {} -> {} is not valid.", tile_from, tile_to));

            game.append(mv).unwrap();
            print!("Move {} -> {}:\n{}", tile_from, tile_to, SquareBoardGeometry::print(&game));
        }

        println!();

        for (move_index, mv) in game.moves_from_tile(final_move).unwrap().into_iter().enumerate() {
            let mut game = game.clone();

            game.append(mv.clone()).unwrap();
            print!("Option #{}:\n{}", move_index, SquareBoardGeometry::print(&game));
        }
    }
}
