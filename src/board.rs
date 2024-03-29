use crate::math::*;
use crate::{Game, GameEvaluation, GameRules, GameState};
use fxhash::FxHashSet;
use generic_array::typenum::{self, Unsigned};
use generic_array::{arr, ArrayLength, GenericArray};
use lazy_static::lazy_static;
use smallvec::{smallvec, SmallVec};
use std::borrow::Cow;
use std::fmt::Debug;
use std::fmt::Write;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::{Mul, Neg};

/// A board is a grid of tiles (cells) on which the game is played.
#[derive(Debug, Clone, Default)]
pub struct Board<G: BoardGeometry> {
    pub(crate) tiles: BoardTiles<G>,
}

impl<G: BoardGeometry> Board<G> {
    pub fn new(tiles: BoardTiles<G>) -> Self {
        Self { tiles }
    }
}

impl<G: BoardGeometry> Board<G> {
    pub fn tiles(&self) -> &BoardTiles<G> {
        &self.tiles
    }
}

/// `BoardTiles` determines which tiles of the `BoardGeometry` actually make up
/// the playable board.
/// Empty by default.
#[derive(Debug, Clone, Default)]
pub struct BoardTiles<G: BoardGeometry> {
    invert_set: bool,
    tile_set: FxHashSet<<G as BoardGeometryExt>::Tile>,
}

impl<G: BoardGeometry> BoardTiles<G> {
    /// Creates `BoardTiles` with no tiles.
    pub fn empty() -> Self {
        Self {
            invert_set: false,
            tile_set: Default::default(),
        }
    }

    /// Creates `BoardTiles` with an infinite number of tiles (no tiles missing).
    pub fn infinite() -> Self {
        Self {
            invert_set: true,
            tile_set: Default::default(),
        }
    }

    /// Builder function to add or remove a tile.
    pub fn with(mut self, tile: <G as BoardGeometryExt>::Tile, present: bool) -> Self {
        self.set(tile, present);
        self
    }

    /// Add or remove a tile.
    pub fn set(&mut self, tile: <G as BoardGeometryExt>::Tile, present: bool) {
        if self.invert_set ^ present {
            self.tile_set.insert(tile);
        } else {
            self.tile_set.remove(&tile);
        }
    }

    pub fn contains(&self, tile: <G as BoardGeometryExt>::Tile) -> bool {
        self.invert_set ^ self.tile_set.contains(&tile)
    }

    pub fn is_infinite(&self) -> bool {
        self.invert_set
    }

    pub fn is_bounded(&self) -> bool {
        !self.is_infinite()
    }
}

#[derive(Debug)]
pub enum BoardGeometryEnum {
    Triangular(TriangularBoardGeometry),
    Square(SquareBoardGeometry),
    Hexagonal(HexagonalBoardGeometry),
}

pub trait CoordinateDimensions = IVecLength + ArrayLength<(bool, u8)>;
pub trait TileTypes = Unsigned;

/// Defines the shape of the board and its properties, such as the coordinate system and useful
/// transformations.
pub trait BoardGeometry:
    Clone + Copy + Default + PartialEq + Eq + PartialOrd + Ord + Hash + Debug
{
    /// The number of integer coordinates used to describe a tile.
    type CoordinateDimensions: CoordinateDimensions + Debug;

    /// The number of types a tile can be.
    type TileTypes: TileTypes + Debug;

    /// Returns `true`, if the coordinates describe a valid tile; `false` otherwise.
    fn is_tile_valid(tile: IVec<Self::CoordinateDimensions>) -> bool;

    fn get_tile_type(tile: IVec<Self::CoordinateDimensions>) -> usize;

    fn get_reflective_symmetries() -> &'static [AxisPermutation<Self>]
    where
        Self: Sized;

    fn get_rotations() -> SmallVec<[AxisPermutation<Self>; 3]>;

    fn print_state(game_rules: &GameRules<Self>, game_state: &GameState<Self>) -> String;

    fn print<E: GameEvaluation<Self>>(game: &Game<Self, E>) -> String {
        Self::print_state(&game.rules, &game.move_log.current_state)
    }
}

/// Automatically impl'd trait for all types that implement `BoardGeometry`.
pub trait BoardGeometryExt: BoardGeometry {
    // Add bounds if needed?
    type Tile;
}

impl<T: BoardGeometry> BoardGeometryExt for T {
    type Tile = IVec<<Self as BoardGeometry>::CoordinateDimensions>;
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TriangularBoardGeometry;

impl BoardGeometry for TriangularBoardGeometry {
    type CoordinateDimensions = typenum::U3;
    type TileTypes = typenum::U2; // triangles /\ and \/

    fn is_tile_valid(tile: IVec3) -> bool {
        let sum: IVecComponent = tile.into_iter().sum();

        sum == 0 || sum == 1
    }

    fn get_tile_type(tile: IVec<Self::CoordinateDimensions>) -> usize {
        (tile[0] + tile[1] + tile[2]) as usize
    }

    fn get_reflective_symmetries() -> &'static [AxisPermutation<Self>]
    where
        Self: Sized,
    {
        lazy_static! {
            static ref REFLECTIVE_SYMMETRIES: Vec<AxisPermutation<TriangularBoardGeometry>> = vec![
                // /\ Swap X and Y
                AxisPermutation {
                    signed_axes: arr![
                        (bool, u8);
                        (false, 1u8),
                        (false, 0u8),
                        (false, 2u8),
                    ],
                },
            ];
        }

        &REFLECTIVE_SYMMETRIES
    }

    fn get_rotations() -> SmallVec<[AxisPermutation<Self>; 3]> {
        AxisPermutation::cycle_axes().collect()
    }

    fn print_state(_game_rules: &GameRules<Self>, _game_state: &GameState<Self>) -> String {
        //
        // ⣿ Lbl ⣉⣿⣿⣿⣉ Lbl ⣿ Lbl ⣉⣿⣿⣿⣉ Lbl ⣿
        // ⣿ ⣀⣤⠶⠛⠉ ⣿ ⠉⠛⠶⣤⣀ ⣿ ⣀⣤⠶⠛⠉ ⣿ ⠉⠛⠶⣤⣀ ⣿
        // ⣿⣿⣉ Lbl ⣿ Lbl ⣉⣿⣿⣿⣉ Lbl ⣿ Lbl ⣉⣿⣿
        // ⣿ ⠉⠛⠶⣤⣀ ⣿ ⣀⣤⠶⠛⠉ ⣿ ⠉⠛⠶⣤⣀ ⣿ ⣀⣤⠶⠛⠉ ⣿
        // ⣿ Lbl ⣉⣿⣿⣿⣉ Lbl ⣿ Lbl ⣉⣿⣿⣿⣉ Lbl ⣿
        // ⣿ ⣀⣤⠶⠛⠉ ⣿ ⠉⠛⠶⣤⣀ ⣿ ⣀⣤⠶⠛⠉ ⣿ ⠉⠛⠶⣤⣀ ⣿
        // ⣿⣿⣉ Lbl ⣿ Lbl ⣉⣿⣿⣿⣉ Lbl ⣿ Lbl ⣉⣿⣿
        //
        // ⡷⠶⠶⠶⠶⠶⠶⣿⡷⠶⠶⠶⠶⠶⠶⣿⡷⠶⠶⠶⠶⠶⠶⣿⡷⠶⠶⠶
        // ⢻⡄Lbl ⣼⠃⢻⡄Lbl ⣼⠃⢻⡄Lbl ⣼⠃⢻⡄Lb
        //  ⢻⡄  ⣼⠃  ⢻⡄  ⣼⠃  ⢻⡄  ⣼⠃  ⢻⡄
        // l ⢻⡄⣼⠃Lbl ⢻⡄⣼⠃Lbl ⢻⡄⣼⠃Lbl ⢻⡄
        // ⠶⠶⠶⣿⡷⠶⠶⠶⠶⠶⠶⣿⡷⠶⠶⠶⠶⠶⠶⣿⡷⠶⠶⠶⠶⠶⠶⣿
        // l ⣼⠃⢻⡄Lbl ⣼⠃⢻⡄Lbl ⣼⠃⢻⡄Lbl ⣼⠃
        //  ⣼⠃  ⢻⡄  ⣼⠃  ⢻⡄  ⣼⠃  ⢻⡄  ⣼⠃
        // ⣼⠃Lbl ⢻⡄⣼⠃Lbl ⢻⡄⣼⠃Lbl ⢻⡄⣼⠃Lb
        // ⡷⠶⠶⠶⠶⠶⠶⣿⡷⠶⠶⠶⠶⠶⠶⣿⡷⠶⠶⠶⠶⠶⠶⣿⡷⠶⠶⠶
        //
        // ⡷⠶⠶⠶⠶⣿⡷⠶⠶⠶⠶⣿⡷⠶⠶⠶⠶⣿⡷
        // ⢻⡄Lb⣼⠃⢻⡄Lb⣼⠃⢻⡄Lb⣼⠃⢻
        //  ⢻⡄⣼⠃Lb⢻⡄⣼⠃Lb⢻⡄⣼⠃Lb
        // ⠶⠶⣿⡷⠶⠶⠶⠶⣿⡷⠶⠶⠶⠶⣿⡷⠶⠶⠶
        // b⣼⠃⢻⡄Lb⣼⠃⢻⡄Lb⣼⠃⢻⡄Lb
        // ⣼⠃Lb⢻⡄⣼⠃Lb⢻⡄⣼⠃Lb⢻⡄
        // ⡷⠶⠶⠶⠶⣿⡷⠶⠶⠶⠶⣿⡷⠶⠶⠶⠶⣿⡷
        // ⢻⡄Lb⣼⠃⢻⡄Lb⣼⠃⢻⡄Lb⣼⠃⢻
        //  ⢻⡄⣼⠃Lb⢻⡄⣼⠃Lb⢻⡄⣼⠃Lb
        // ⠶⠶⣿⡷⠶⠶⠶⠶⣿⡷⠶⠶⠶⠶⣿⡷⠶⠶⠶
        //
        // ───╳─────╳─────╳─────╳───
        //   ╱ ╲Lbl╱ ╲Lbl╱ ╲Lbl╱ ╲
        //  ╱Lbl╲ ╱Lbl╲ ╱Lbl╲ ╱Lbl╲
        // ╳─────╳─────╳─────╳─────╳
        //  ╲Lbl╱ ╲Lbl╱ ╲Lbl╱ ╲Lbl╱
        //   ╲ ╱Lbl╲ ╱Lbl╲ ╱Lbl╲ ╱
        // ───╳─────╳─────╳─────╳───
        //
        todo!()
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SquareBoardGeometry;

impl BoardGeometry for SquareBoardGeometry {
    type CoordinateDimensions = typenum::U2;
    type TileTypes = typenum::U1;

    fn is_tile_valid(_tile: IVec2) -> bool {
        true
    }

    fn get_tile_type(_tile: IVec<Self::CoordinateDimensions>) -> usize {
        0
    }

    fn get_reflective_symmetries() -> &'static [AxisPermutation<Self>]
    where
        Self: Sized,
    {
        lazy_static! {
            static ref REFLECTIVE_SYMMETRIES: Vec<AxisPermutation<SquareBoardGeometry>> = vec![
                // [ ] Flip X
                AxisPermutation {
                    signed_axes: arr![
                        (bool, u8);
                        (true,  0u8),
                        (false, 1u8),
                    ],
                },
                // [ ] Swap X and Y
                AxisPermutation {
                    signed_axes: arr![
                        (bool, u8);
                        (false, 1u8),
                        (false, 0u8),
                    ],
                },
            ];
        }

        &REFLECTIVE_SYMMETRIES
    }

    fn get_rotations() -> SmallVec<[AxisPermutation<Self>; 3]> {
        smallvec![
            [(false, 0), (false, 1)].into(),
            [(true, 1), (false, 0)].into(),
            [(true, 0), (true, 1)].into(),
            [(false, 1), (true, 0)].into(),
        ]
    }

    fn print_state(game_rules: &GameRules<Self>, game_state: &GameState<Self>) -> String {
        //
        // ⣿⠶⠶⠶⣿⠶⠶⠶⣿⠶⠶⠶⣿⠶⠶⠶⣿
        // ⣿Lbl⣿Lbl⣿Lbl⣿Lbl⣿
        // ⣿⠶⠶⠶⣿⠶⠶⠶⣿⠶⠶⠶⣿⠶⠶⠶⣿
        // ⣿Lbl⣿Lbl⣿Lbl⣿Lbl⣿
        // ⣿⠶⠶⠶⣿⠶⠶⠶⣿⠶⠶⠶⣿⠶⠶⠶⣿
        // ⣿Lbl⣿Lbl⣿Lbl⣿Lbl⣿
        // ⣿⠶⠶⠶⣿⠶⠶⠶⣿⠶⠶⠶⣿⠶⠶⠶⣿
        //
        // ┼───┼───┼───┼───┼
        // │Lbl│Lbl│Lbl│Lbl│
        // ┼───┼───┼───┼───┼
        // │Lbl│Lbl│Lbl│Lbl│
        // ┼───┼───┼───┼───┼
        // │Lbl│Lbl│Lbl│Lbl│
        // ┼───┼───┼───┼───┼
        //
        use colored::*;
        assert!(
            game_rules.players.get() <= 2,
            "Printing a game with more than 2 players is currently not supported."
        );

        let mut result = String::new();
        let mut min: IVec2 = [IVecComponent::MAX; 2].into();
        let mut max: IVec2 = [IVecComponent::MIN; 2].into();

        if game_rules.board.tiles.invert_set {
            const BORDER: usize = 1;

            if game_state.pieces.is_empty() {
                return String::new();
            }

            for tile in game_state.pieces.keys() {
                for (c_tile, c_min, c_max) in itertools::multizip((tile, &mut min, &mut max)) {
                    *c_min = std::cmp::min(*c_min, *c_tile);
                    *c_max = std::cmp::max(*c_max, *c_tile);
                }
            }

            min -= IVec2::from([BORDER as IVecComponent; 2]);
            max += IVec2::from([BORDER as IVecComponent; 2]);

            for tile in &game_rules.board.tiles.tile_set {
                for (c_tile, c_min, c_max) in itertools::multizip((tile, &mut min, &mut max)) {
                    *c_min = std::cmp::min(*c_min, *c_tile);
                    *c_max = std::cmp::max(*c_max, *c_tile);
                }
            }
        } else {
            if game_rules.board.tiles.tile_set.is_empty() {
                return String::new();
            }

            for tile in &game_rules.board.tiles.tile_set {
                for (c_tile, c_min, c_max) in itertools::multizip((tile, &mut min, &mut max)) {
                    *c_min = std::cmp::min(*c_min, *c_tile);
                    *c_max = std::cmp::max(*c_max, *c_tile);
                }
            }
        }

        fn get_border(right: bool, top: bool, left: bool, bottom: bool) -> char {
            match (right as u8) | (bottom as u8) << 1 | (left as u8) << 2 | (top as u8) << 3 {
                0b0000 => ' ',
                0b0001 => '╶',
                0b0010 => '╵',
                0b0011 => '└',
                0b0100 => '╴',
                0b0101 => '─',
                0b0110 => '┘',
                0b0111 => '┴',
                0b1000 => '╷',
                0b1001 => '┌',
                0b1010 => '│',
                0b1011 => '├',
                0b1100 => '┐',
                0b1101 => '┬',
                0b1110 => '┤',
                0b1111 => '┼',
                _ => unreachable!(),
            }
        }

        fn tile_to_border(
            tile: IVec2,
            board: &Board<SquareBoardGeometry>,
            game_state: &GameState<SquareBoardGeometry>,
        ) -> [[char; 3]; 3] {
            let n: SmallVec<[SmallVec<[bool; 3]>; 3]> = (-1..=1)
                .map(|y| {
                    (-1..=1)
                        .map(|x| {
                            let offset = IVec2::from([x, y]);
                            let absolute = tile + offset;
                            game_state.tile(board, absolute).is_some()
                        })
                        .collect()
                })
                .collect();

            fn get_corner(offset: [usize; 2], n: &SmallVec<[SmallVec<[bool; 3]>; 3]>) -> char {
                #[allow(clippy::identity_op)]
                get_border(
                    n[0 + offset[1]][1 + offset[0]] || n[1 + offset[1]][1 + offset[0]],
                    n[0 + offset[1]][0 + offset[0]] || n[0 + offset[1]][1 + offset[0]],
                    n[0 + offset[1]][0 + offset[0]] || n[1 + offset[1]][0 + offset[0]],
                    n[1 + offset[1]][0 + offset[0]] || n[1 + offset[1]][1 + offset[0]],
                )
            }

            [
                [
                    get_corner([0, 0], &n),
                    get_border(n[0][1] || n[1][1], false, n[0][1] || n[1][1], false),
                    get_corner([1, 0], &n),
                ],
                [
                    get_border(false, n[1][0] || n[1][1], false, n[1][0] || n[1][1]),
                    ' ',
                    get_border(false, n[1][1] || n[1][2], false, n[1][1] || n[1][2]),
                ],
                [
                    get_corner([0, 1], &n),
                    get_border(n[1][1] || n[2][1], false, n[1][1] || n[2][1], false),
                    get_corner([1, 1], &n),
                ],
            ]
        }

        const COLORS: [[[u8; 3]; 2]; 2] = [
            [[0x00, 0x00, 0x00], [0xFF, 0xFF, 0xFF]],
            [[0xFF, 0xFF, 0xFF], [0x00, 0x00, 0x00]],
        ];

        for y in (min[1]..=max[1]).rev() {
            let mut row = [String::new(), String::new(), String::new()];

            for x in min[0]..=max[0] {
                let tile: IVec2 = [x, y].into();
                let border = tile_to_border(tile, &game_rules.board, game_state);
                let label = game_state
                    .tile(&game_rules.board, tile)
                    .map(|tile| {
                        if let Some(piece) = tile.get_piece() {
                            let definition = piece.get_definition(&game_rules.piece_set);
                            let title =
                                &definition.title[0..std::cmp::min(3, definition.title.len())];
                            let colors = COLORS[piece.owner as usize];

                            Cow::Owned(
                                title
                                    .truecolor(colors[0][0], colors[0][1], colors[0][2])
                                    .on_truecolor(colors[1][0], colors[1][1], colors[1][2])
                                    .to_string(),
                            )
                        } else {
                            Cow::Borrowed("   ")
                        }
                    })
                    .unwrap_or(Cow::Borrowed(" × "));

                if x == min[0] {
                    row[0].push(border[2][0]);
                    row[1].push(border[1][0]);
                    row[2].push(border[0][0]);
                }

                (0..3).for_each(|_| row[0].push(border[2][1]));
                row[0].push(border[2][2]);

                row[1].push_str(&label);
                row[1].push(border[1][2]);

                (0..3).for_each(|_| row[2].push(border[0][1]));
                row[2].push(border[0][2]);
            }

            writeln!(result, "{}", &row[0]).unwrap();
            writeln!(result, "{}", &row[1]).unwrap();

            if y == min[1] {
                writeln!(result, "{}", &row[2]).unwrap();
            }
        }

        result
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HexagonalBoardGeometry;

impl BoardGeometry for HexagonalBoardGeometry {
    type CoordinateDimensions = typenum::U3;
    type TileTypes = typenum::U1;

    fn is_tile_valid(tile: IVec3) -> bool {
        let sum: IVecComponent = tile.into_iter().sum();

        sum == 0
    }

    fn get_tile_type(_tile: IVec<Self::CoordinateDimensions>) -> usize {
        0
    }

    fn get_reflective_symmetries() -> &'static [AxisPermutation<Self>]
    where
        Self: Sized,
    {
        lazy_static! {
            static ref REFLECTIVE_SYMMETRIES: Vec<AxisPermutation<HexagonalBoardGeometry>> = vec![
                // ⬡  Horizontal flip (Swap X and Y)
                AxisPermutation {
                    signed_axes: arr![
                        (bool, u8);
                        (false, 1u8),
                        (false, 0u8),
                        (false, 2u8),
                    ],
                },
                // ⟨ ⟩ Horizontal flip (After rotating by 30 degrees)
                AxisPermutation {
                    signed_axes: arr![
                        (bool, u8);
                        (true, 2u8),
                        (true, 1u8),
                        (true, 0u8),
                    ],
                },
            ];
        }

        &REFLECTIVE_SYMMETRIES
    }

    fn get_rotations() -> SmallVec<[AxisPermutation<Self>; 3]> {
        let cycles: SmallVec<[AxisPermutation<Self>; 3]> =
            AxisPermutation::<Self>::cycle_axes().collect();

        smallvec![
            cycles[0].clone(),
            -cycles[2].clone(),
            cycles[1].clone(),
            -cycles[0].clone(),
            cycles[2].clone(),
            -cycles[1].clone(),
        ]
    }

    fn print_state(_game_rules: &GameRules<Self>, _game_state: &GameState<Self>) -> String {
        //
        // ⠉⠛⠶⣤⠶⠛⠉⠛⠶⣤⠶⠛⠉⠛⠶⣤⠶⠛⠉⠛⠶⣤⠶⠛⠉
        // bl ⣿ Lbl ⣿ Lbl ⣿ Lbl ⣿ Lb
        // ⣤⠶⠛⠉⠛⠶⣤⠶⠛⠉⠛⠶⣤⠶⠛⠉⠛⠶⣤⠶⠛⠉⠛⠶⣤
        // ⣿ Lbl ⣿ Lbl ⣿ Lbl ⣿ Lbl ⣿
        // ⠉⠛⠶⣤⠶⠛⠉⠛⠶⣤⠶⠛⠉⠛⠶⣤⠶⠛⠉⠛⠶⣤⠶⠛⠉
        // bl ⣿ Lbl ⣿ Lbl ⣿ Lbl ⣿ Lb
        // ⣤⠶⠛⠉⠛⠶⣤⠶⠛⠉⠛⠶⣤⠶⠛⠉⠛⠶⣤⠶⠛⠉⠛⠶⣤
        // ⣿ Lbl ⣿ Lbl ⣿ Lbl ⣿ Lbl ⣿
        // ⠉⠛⠶⣤⠶⠛⠉⠛⠶⣤⠶⠛⠉⠛⠶⣤⠶⠛⠉⠛⠶⣤⠶⠛⠉
        //
        //    ╳─────╳
        //   ╱       ╲
        //  ╳   Lbl   ╳─────╳
        //   ╲       ╱       ╲
        //    ╳─────╳   Lbl   ╳
        //   ╱       ╲       ╱
        //  ╳   Lbl   ╳─────╳
        //   ╲       ╱
        //    ╳─────╳
        //
        // ⢈⡷⠶⠶⢾⡁Lb⢈⡷⠶⠶⢾⡁Lb⢈⡷
        // ⢾⡁Lb⢈⡷⠶⠶⢾⡁Lb⢈⡷⠶⠶⢾⡁
        // ⢈⡷⠶⠶⢾⡁Lb⢈⡷⠶⠶⢾⡁Lb⢈⡷
        // ⢾⡁Lb⢈⡷⠶⠶⢾⡁Lb⢈⡷⠶⠶⢾⡁
        // ⢈⡷⠶⠶⢾⡁Lb⢈⡷⠶⠶⢾⡁Lb⢈⡷
        // ⢾⡁Lb⢈⡷⠶⠶⢾⡁Lb⢈⡷⠶⠶⢾⡁
        // ⢈⡷⠶⠶⢾⡁Lb⢈⡷⠶⠶⢾⡁Lb⢈⡷
        // ⢾⡁Lb⢈⡷⠶⠶⢾⡁Lb⢈⡷⠶⠶⢾⡁
        //
        //  ⢈⡷⠶⠶⠶⢾⡁ Lbl ⢈⡷⠶⠶⠶⢾⡁
        // ⢀⡾⠁   ⠈⢷⡀   ⢀⡾⠁   ⠈⢷⡀
        // ⢾⡁ Lbl ⢈⡷⠶⠶⠶⢾⡁ Lbl ⢈⡷
        // ⠈⢷⡀   ⢀⡾⠁   ⠈⢷⡀   ⢀⡾⠁
        //  ⢈⡷⠶⠶⠶⢾⡁ Lbl ⢈⡷⠶⠶⠶⢾⡁
        // ⢀⡾⠁   ⠈⢷⡀   ⢀⡾⠁   ⠈⢷⡀
        // ⢾⡁ Lbl ⢈⡷⠶⠶⠶⢾⡁ Lbl ⢈⡷
        // ⠈⢷⡀   ⢀⡾⠁   ⠈⢷⡀   ⢀⡾⠁
        //  ⢈⡷⠶⠶⠶⢾⡁ Lbl ⢈⡷⠶⠶⠶⢾⡁
        //
        todo!()
    }
}

pub struct Symmetry<G: BoardGeometry> {
    pub rotational: bool,
    pub reflectional: bool,
    marker: PhantomData<G>,
}

pub trait Transformation<G: BoardGeometry> {
    fn inverse(&self) -> Self;
    fn combine(lhs: &Self, rhs: &Self) -> Self;
    fn apply(&self, tile: <G as BoardGeometryExt>::Tile) -> <G as BoardGeometryExt>::Tile;
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AxisPermutation<G: BoardGeometry> {
    /// An array of signed axes: `(flip sign, axis index)`
    signed_axes: GenericArray<(bool, u8), <G as BoardGeometry>::CoordinateDimensions>,
}

impl<G: BoardGeometry> Default for AxisPermutation<G> {
    fn default() -> Self {
        let mut signed_axes: GenericArray<(bool, u8), <G as BoardGeometry>::CoordinateDimensions> =
            Default::default();

        for (index, &mut (_, ref mut axis)) in signed_axes.iter_mut().enumerate() {
            *axis = index as u8;
        }

        Self { signed_axes }
    }
}

impl<
        G: BoardGeometry,
        A: Into<GenericArray<(bool, u8), <G as BoardGeometry>::CoordinateDimensions>>,
    > From<A> for AxisPermutation<G>
{
    fn from(signed_axes: A) -> Self {
        Self {
            signed_axes: signed_axes.into(),
        }
    }
}

impl<G: BoardGeometry> Transformation<G> for AxisPermutation<G> {
    fn inverse(&self) -> Self {
        let mut result = AxisPermutation::default();

        for (index, (sign, axis)) in self.signed_axes.iter().enumerate() {
            result.signed_axes[*axis as usize] = (*sign, index as u8);
        }

        result
    }

    /// Combine two permutations. Resulting permutation is equivalent to
    /// applying `rhs` followed by `lhs`.
    fn combine(lhs: &Self, rhs: &Self) -> Self {
        AxisPermutation {
            signed_axes: GenericArray::from_exact_iter(lhs.signed_axes.iter().map(
                |(lhs_sign, lhs_axis)| {
                    let (rhs_sign, rhs_axis) = rhs.signed_axes[*lhs_axis as usize];
                    (rhs_sign ^ lhs_sign, rhs_axis)
                },
            ))
            .unwrap(),
        }
    }

    fn apply(&self, tile: <G as BoardGeometryExt>::Tile) -> <G as BoardGeometryExt>::Tile {
        let mut result = tile;

        result
            .iter_mut()
            .zip(self.signed_axes.iter())
            .for_each(|(c, (sign, axis))| {
                *c = tile[*axis as usize];

                if *sign {
                    *c = -*c;
                }
            });

        result
    }
}

impl<G: BoardGeometry> Neg for AxisPermutation<G> {
    type Output = Self;

    fn neg(mut self) -> Self::Output {
        for &mut (ref mut sign, _) in &mut self.signed_axes {
            *sign ^= true;
        }

        self
    }
}

impl<G: BoardGeometry> AxisPermutation<G> {
    /// Returns all unique transformations mapping axes to other axes while preserving their order.
    /// In 3 dimensions, those would be: XYZ, ZXY, YZX.
    pub fn cycle_axes() -> impl Iterator<Item = Self> {
        (0..<G::CoordinateDimensions as Unsigned>::to_u8())
            .rev()
            .map(|rotation| {
                let axes = (0..<G::CoordinateDimensions as Unsigned>::to_u8())
                    .map(|axis| {
                        (axis + rotation + 1) % <G::CoordinateDimensions as Unsigned>::to_u8()
                    })
                    .map(|axis| (false, axis));

                AxisPermutation {
                    signed_axes: GenericArray::from_exact_iter(axes).unwrap(),
                }
            })
    }

    /// Returns the product of `B * A * B^-1`
    pub fn sandwich_by(&self, sandwich: &Self) -> Self {
        sandwich * self * sandwich.inverse()
    }
}

impl<G: BoardGeometry> Mul for AxisPermutation<G> {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        Self::combine(&self, &rhs)
    }
}

impl<G: BoardGeometry> Mul for &'_ AxisPermutation<G> {
    type Output = AxisPermutation<G>;

    fn mul(self, rhs: Self) -> Self::Output {
        AxisPermutation::<G>::combine(self, rhs)
    }
}

#[derive(Default, Debug)]
pub struct Isometry<G: BoardGeometry> {
    pub translate: <G as BoardGeometryExt>::Tile,
    pub axis_permutation: AxisPermutation<G>,
}

impl<G: BoardGeometry> Isometry<G> {
    pub fn translation(translate: <G as BoardGeometryExt>::Tile) -> Self {
        Self {
            translate,
            axis_permutation: Default::default(),
        }
    }
}

impl<G: BoardGeometry> From<AxisPermutation<G>> for Isometry<G> {
    fn from(axis_permutation: AxisPermutation<G>) -> Self {
        Self {
            translate: Default::default(),
            axis_permutation,
        }
    }
}

impl<G: BoardGeometry> Transformation<G> for Isometry<G> {
    fn inverse(&self) -> Self {
        Isometry {
            translate: -self.translate,
            axis_permutation: self.axis_permutation.inverse(),
        }
    }

    fn combine(lhs: &Self, rhs: &Self) -> Self {
        Isometry {
            translate: rhs.translate + rhs.axis_permutation.apply(lhs.translate),
            axis_permutation: &lhs.axis_permutation * &rhs.axis_permutation,
        }
    }

    fn apply(&self, tile: <G as BoardGeometryExt>::Tile) -> <G as BoardGeometryExt>::Tile {
        self.translate + self.axis_permutation.apply(tile)
    }
}

impl<G: BoardGeometry> Mul for Isometry<G> {
    type Output = Isometry<G>;

    fn mul(self, rhs: Self) -> Self::Output {
        Isometry::<G>::combine(&self, &rhs)
    }
}

impl<G: BoardGeometry> Mul for &'_ Isometry<G> {
    type Output = Isometry<G>;

    fn mul(self, rhs: Self) -> Self::Output {
        Isometry::<G>::combine(self, rhs)
    }
}
