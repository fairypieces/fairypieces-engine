// #![feature(min_const_generics)]
// #![feature(const_generics)]
// #![feature(const_evaluatable_checked)]
// #![feature(const_fn)]
// #![feature(inline_const)]
#![feature(trait_alias)]

use std::num::NonZeroUsize;
use std::collections::HashMap;

use math::*;
use board::*;
use piece::*;

pub mod math;
pub mod board;
pub mod piece;

pub struct Game<G: BoardGeometry> {
    board: Board<G>,
    piece_set: PieceSet<G>,
    players: NonZeroUsize,
}

pub struct GameState<G: BoardGeometry> {
    game: Game<G>,
    pieces: HashMap<<G as BoardGeometryExt>::Tile, Piece<G>>,
}

impl<G: BoardGeometry> GameState<G> {
    pub fn tile(&self, tile: <G as BoardGeometryExt>::Tile) -> Option<TileRef<'_, G>> {
        self.game.board.tiles().contains(&tile).then(move || {
            TileRef {
                tile,
                // game: &self.game,
                pieces: &self.pieces,
            }
        })
    }

    pub fn tile_mut(&mut self, tile: <G as BoardGeometryExt>::Tile) -> Option<TileRefMut<'_, G>> {
        self.game.board.tiles().contains(&tile).then(move || {
            TileRefMut {
                tile,
                // game: &mut self.game,
                pieces: &mut self.pieces,
            }
        })
    }
}

#[derive(Clone)]
pub struct TileRef<'a, G: BoardGeometry> {
    tile: <G as BoardGeometryExt>::Tile,
    // game: &'a Game<G>,
    pieces: &'a HashMap<<G as BoardGeometryExt>::Tile, Piece<G>>,
}

impl<G: BoardGeometry> TileRef<'_, G> {
    pub fn get(&self) -> Option<&Piece<G>> {
        self.pieces.get(&self.tile)
    }
}

pub struct TileRefMut<'a, G: BoardGeometry> {
    tile: <G as BoardGeometryExt>::Tile,
    // game: &'a mut Game<G>,
    pieces: &'a mut HashMap<<G as BoardGeometryExt>::Tile, Piece<G>>,
}

impl<G: BoardGeometry> TileRefMut<'_, G> {
    pub fn get(&self) -> Option<&Piece<G>> {
        self.pieces.get(&self.tile)
    }

    pub fn set(&mut self, piece: Option<Piece<G>>) {
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
}
