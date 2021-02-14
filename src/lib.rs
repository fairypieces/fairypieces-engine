// #![feature(min_const_generics)]
// #![feature(const_generics)]
// #![feature(const_evaluatable_checked)]
// #![feature(const_fn)]
// #![feature(inline_const)]
#![feature(trait_alias)]

use math::*;
use board::*;
use piece::*;

pub mod math;
pub mod board;
pub mod piece;

pub struct Game<G>
where
    G: BoardGeometry,
{
    board: Board<G>,
}

pub struct GameState {

}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
