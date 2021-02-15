use std::fmt::Debug;
use std::collections::HashSet;
use std::marker::PhantomData;
use std::ops::Mul;
use generic_array::{GenericArray, ArrayLength, arr};
use generic_array::typenum::{self, Unsigned};
use lazy_static::lazy_static;
use crate::math::*;

#[derive(Debug)]
pub struct Board<G: BoardGeometry> {
    tiles: BoardTiles<G>,
}

impl<G: BoardGeometry> Board<G> {
    pub fn tiles(&self) -> &BoardTiles<G> {
        &self.tiles
    }
}

#[derive(Debug)]
pub struct BoardTiles<G: BoardGeometry> {
    invert_set: bool,
    tile_set: HashSet<<G as BoardGeometryExt>::Tile>,
}

impl<G: BoardGeometry> BoardTiles<G> {
    pub fn set(&mut self, tile: <G as BoardGeometryExt>::Tile, present: bool) {
        if self.invert_set ^ present {
            self.tile_set.insert(tile);
        } else {
            self.tile_set.remove(&tile);
        }
    }

    pub fn contains(&self, tile: &<G as BoardGeometryExt>::Tile) -> bool {
        self.invert_set ^ self.tile_set.contains(tile)
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

pub trait BoardGeometry: Default + Debug {

    /// The number of integer coordinates used to describe a tile.
    // const COORDINATE_DIMENSIONS: usize;
    type CoordinateDimensions: CoordinateDimensions + Debug;

    /// The number of types a tile can be.
    // const TILE_TYPES: usize;
    type TileTypes: TileTypes + Debug;

    /// Returns `true`, if the coordinates describe a valid tile; `false` otherwise.
    fn is_tile_valid(tile: IVec<Self::CoordinateDimensions>) -> bool;

    fn get_reflective_symmetries() -> &'static [AxisPermutation<Self>] where Self: Sized;
}

/// Automatically impl'd trait for all types that implement `BoardGeometry`.
pub trait BoardGeometryExt: BoardGeometry {
    type Tile;
}

impl<T: BoardGeometry> BoardGeometryExt for T {
    type Tile = IVec<<Self as BoardGeometry>::CoordinateDimensions>;
}

#[derive(Debug, Default)]
pub struct TriangularBoardGeometry;

impl BoardGeometry for TriangularBoardGeometry {
    type CoordinateDimensions = typenum::U3;
    type TileTypes = typenum::U2;
    // const COORDINATE_DIMENSIONS: usize = 3;
    // const TILE_TYPES: usize = 2; // triangles /\ and \/

    fn is_tile_valid(tile: IVec3) -> bool {
        let sum: i32 = tile.into_iter().sum();

        sum == 0 || sum == 1
    }

    fn get_reflective_symmetries() -> &'static [AxisPermutation<Self>] where 
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

        &*REFLECTIVE_SYMMETRIES
    }
}

#[derive(Debug, Default)]
pub struct SquareBoardGeometry;

impl BoardGeometry for SquareBoardGeometry {
    type CoordinateDimensions = typenum::U2;
    type TileTypes = typenum::U1;
    // const COORDINATE_DIMENSIONS: usize = 2;
    // const TILE_TYPES: usize = 1;

    fn is_tile_valid(_tile: IVec2) -> bool {
        true
    }

    fn get_reflective_symmetries() -> &'static [AxisPermutation<Self>] where 
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

        &*REFLECTIVE_SYMMETRIES
    }
}

#[derive(Debug, Default)]
pub struct HexagonalBoardGeometry;

impl BoardGeometry for HexagonalBoardGeometry {
    type CoordinateDimensions = typenum::U3;
    type TileTypes = typenum::U1;
    // const COORDINATE_DIMENSIONS: usize = 3;
    // const TILE_TYPES: usize = 1;

    fn is_tile_valid(tile: IVec3) -> bool {
        let sum: i32 = tile.into_iter().sum();

        sum == 0
    }

    fn get_reflective_symmetries() -> &'static [AxisPermutation<Self>] where
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

        &*REFLECTIVE_SYMMETRIES
    }
}

pub struct Symmetry<G: BoardGeometry> {
    pub rotational: bool,
    pub reflectional: bool,
    marker: PhantomData<G>,
}

#[derive(Clone, Default, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct AxisPermutation<G: BoardGeometry> {
    /// An array of signed axes: `(flip sign, axis index)`
    signed_axes: GenericArray<(bool, u8), <G as BoardGeometry>::CoordinateDimensions>,
}

impl<G: BoardGeometry> AxisPermutation<G> {
    pub fn inverse(&self) -> Self {
        let mut result = AxisPermutation::default();

        for (index, (sign, axis)) in self.signed_axes.iter().enumerate() {
            result.signed_axes[*axis as usize] = (*sign, index as u8);
        }

        result
    }

    /// Combine two permutations. Resulting permutation is equivalent to
    /// applying `rhs` followed by `lhs`.
    pub fn combine(lhs: &Self, rhs: &Self) -> Self {
        AxisPermutation {
            signed_axes: GenericArray::from_exact_iter(lhs.signed_axes.iter().map(|(lhs_sign, lhs_axis)| {
                let (rhs_sign, rhs_axis) = rhs.signed_axes[*lhs_axis as usize];
                (rhs_sign ^ lhs_sign, rhs_axis)
            })).unwrap(),
        }
    }

    pub fn apply(&self, tile: <G as BoardGeometryExt>::Tile) -> <G as BoardGeometryExt>::Tile {
        let mut result = tile.clone();

        result.iter_mut().zip(self.signed_axes.iter()).for_each(|(c, (sign, axis))| {
            *c = tile[*axis as usize];

            if *sign {
                *c = -*c;
            }
        });

        result
    }

    pub fn rotations() -> impl Iterator<Item=Self> {
        (0..<G::CoordinateDimensions as Unsigned>::to_u8()).map(|rotation| {
            let axes = (0..<G::CoordinateDimensions as Unsigned>::to_u8()).map(|axis| {
                (axis + rotation) % <G::CoordinateDimensions as Unsigned>::to_u8()
            }).map(|axis| (false, axis));

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
