use std::num::NonZeroUsize;
use lazy_static::lazy_static;
use crate::*;

pub static PLAYER_WHITE: usize = 0;
pub static PLAYER_BLACK: usize = 1;

pub static PIECE_PAWN: usize   = 0;
pub static PIECE_ROOK: usize   = 1;
pub static PIECE_KNIGHT: usize = 2;
pub static PIECE_BISHOP: usize = 3;
pub static PIECE_QUEEN: usize  = 4;
pub static PIECE_KING: usize   = 5;

lazy_static! {
    pub static ref PIECE_SET: PieceSet<SquareBoardGeometry> = {
        let definitions = vec![
            // TODO: Promote when last row reached
            PieceDefinitionUnvalidated::new("Pawn")
                .with_initial_state(StateUnvalidated::new(Action::Symmetry {
                    symmetries: vec![
                        Default::default(),
                        SquareBoardGeometry::get_reflective_symmetries()[0].clone(),
                    ].into_iter().collect(),
                }).with_successors(vec![1, 2, 3, 4]))
                // Advance by 1 tile
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { before_moves: 0, tile: [0, 1].into() },
                        ConditionEnum::Not(Box::new(ConditionEnum::PiecePresent { before_moves: 0, tile: [0, 1].into() })),
                    ]),
                    actions: Default::default(),
                    move_choices: vec![Some([0, 1].into())].into_iter().collect(),
                }).with_final(true))
                // Advance by 2 tiles
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::PieceInitial { before_moves: 0, tile: [0, 0].into() },
                        ConditionEnum::TilePresent { before_moves: 0, tile: [0, 1].into() },
                        ConditionEnum::TilePresent { before_moves: 0, tile: [0, 2].into() },
                        ConditionEnum::Not(Box::new(ConditionEnum::PiecePresent { before_moves: 0, tile: [0, 1].into() })),
                        ConditionEnum::Not(Box::new(ConditionEnum::PiecePresent { before_moves: 0, tile: [0, 2].into() })),
                    ]),
                    actions: Default::default(),
                    move_choices: vec![Some([0, 2].into())].into_iter().collect(),
                }).with_final(true))
                // Take diagonally
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { before_moves: 0, tile: [1, 1].into() },
                        ConditionEnum::PiecePresent { before_moves: 0, tile: [1, 1].into() },
                        ConditionEnum::PieceControlledByEnemy { before_moves: 0, tile: [1, 1].into() },
                    ]),
                    actions: vec![ActionEnum::SetTile {
                        target: [1, 1].into(),
                        piece: None,
                    }].into_iter().collect(),
                    move_choices: vec![Some([1, 1].into())].into_iter().collect(),
                }).with_final(true))
                // En-passant
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        // Check enemy pawn has been advanced by two tiles in the previous move
                        ConditionEnum::MovesPlayedGreaterThanOrEqual(1),
                        // Before previous move:
                        ConditionEnum::TilePresent { before_moves: 1, tile: [1, 2].into() },
                        ConditionEnum::TilePresent { before_moves: 1, tile: [1, 1].into() },
                        ConditionEnum::TilePresent { before_moves: 1, tile: [1, 0].into() },
                        ConditionEnum::PiecePresent { before_moves: 1, tile: [1, 2].into() },
                        ConditionEnum::Not(Box::new(ConditionEnum::PiecePresent { before_moves: 1, tile: [1, 1].into() })),
                        ConditionEnum::Not(Box::new(ConditionEnum::PiecePresent { before_moves: 1, tile: [1, 0].into() })),
                        ConditionEnum::PieceTypeIs { before_moves: 1, tile: [1, 2].into(), definition_index: PIECE_PAWN },
                        ConditionEnum::PieceControlledByEnemy { before_moves: 1, tile: [1, 2].into() },
                        // After previous move:
                        ConditionEnum::TilePresent { before_moves: 0, tile: [1, 2].into() },
                        ConditionEnum::TilePresent { before_moves: 0, tile: [1, 1].into() },
                        ConditionEnum::TilePresent { before_moves: 0, tile: [1, 0].into() },
                        ConditionEnum::Not(Box::new(ConditionEnum::PiecePresent { before_moves: 0, tile: [1, 2].into() })),
                        ConditionEnum::Not(Box::new(ConditionEnum::PiecePresent { before_moves: 0, tile: [1, 1].into() })),
                        ConditionEnum::PiecePresent { before_moves: 0, tile: [1, 0].into() },
                        ConditionEnum::PieceTypeIs { before_moves: 0, tile: [1, 0].into(), definition_index: PIECE_PAWN },
                        ConditionEnum::PieceControlledByEnemy { before_moves: 0, tile: [1, 0].into() },
                    ]),
                    actions: vec![ActionEnum::SetTile {
                        target: [1, 0].into(),
                        piece: None,
                    }].into_iter().collect(),
                    move_choices: vec![Some([1, 1].into())].into_iter().collect(),
                }).with_final(true)),
            PieceDefinitionUnvalidated::new("Rook")
                .with_initial_state(StateUnvalidated::new(Action::Symmetry {
                    symmetries: SquareBoardGeometry::get_rotations().into_iter().collect(),
                }).with_successors(vec![1, 2]))
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { before_moves: 0, tile: [0, 1].into() },
                        ConditionEnum::Not(Box::new(ConditionEnum::PiecePresent { before_moves: 0, tile: [0, 1].into() })),
                    ]),
                    actions: Default::default(),
                    move_choices: vec![Some([0, 1].into())].into_iter().collect(),
                }).with_final(true).with_successors(vec![1, 2]))
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { before_moves: 0, tile: [0, 1].into() },
                        ConditionEnum::PiecePresent { before_moves: 0, tile: [0, 1].into() },
                        ConditionEnum::PieceControlledByEnemy { before_moves: 0, tile: [0, 1].into() },
                    ]),
                    actions: vec![ActionEnum::SetTile {
                        target: [0, 1].into(),
                        piece: None,
                    }].into_iter().collect(),
                    move_choices: vec![Some([0, 1].into())].into_iter().collect(),
                }).with_final(true)),
            PieceDefinitionUnvalidated::new("Knight")
                .with_initial_state(StateUnvalidated::new(Action::Symmetry {
                    symmetries: SquareBoardGeometry::get_rotations().into_iter().flat_map(|rotation| {
                        vec![
                            AxisPermutation::default(),
                            SquareBoardGeometry::get_reflective_symmetries()[1].clone(),
                        ].into_iter().map(move |reflection| {
                            rotation.clone() * reflection
                        })
                    }).collect()
                }).with_successor(1))
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { before_moves: 0, tile: [2, 1].into() },
                        ConditionEnum::PieceControlledByEnemy { before_moves: 0, tile: [2, 1].into() },
                    ]),
                    actions: vec![ActionEnum::SetTile {
                        target: [2, 1].into(),
                        piece: None,
                    }].into_iter().collect(),
                    move_choices: vec![Some([2, 1].into())].into_iter().collect(),
                }).with_final(true)),
            PieceDefinitionUnvalidated::new("Bishop")
                .with_initial_state(StateUnvalidated::new(Action::Symmetry {
                    symmetries: SquareBoardGeometry::get_rotations().into_iter().collect(),
                }).with_successors(vec![1, 2]))
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { before_moves: 0, tile: [1, 1].into() },
                        ConditionEnum::Not(Box::new(ConditionEnum::PiecePresent { before_moves: 0, tile: [1, 1].into() })),
                    ]),
                    actions: Default::default(),
                    move_choices: vec![Some([1, 1].into())].into_iter().collect(),
                }).with_final(true).with_successors(vec![1, 2]))
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { before_moves: 0, tile: [1, 1].into() },
                        ConditionEnum::PiecePresent { before_moves: 0, tile: [1, 1].into() },
                        ConditionEnum::PieceControlledByEnemy { before_moves: 0, tile: [1, 1].into() },
                    ]),
                    actions: vec![ActionEnum::SetTile {
                        target: [1, 1].into(),
                        piece: None,
                    }].into_iter().collect(),
                    move_choices: vec![Some([1, 1].into())].into_iter().collect(),
                }).with_final(true)),
            PieceDefinitionUnvalidated::new("Queen")
                .with_initial_state(StateUnvalidated::new(Action::Symmetry {
                    symmetries: SquareBoardGeometry::get_rotations().into_iter().collect(),
                }).with_successors(vec![1, 2, 3, 4]))
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { before_moves: 0, tile: [0, 1].into() },
                        ConditionEnum::Not(Box::new(ConditionEnum::PiecePresent { before_moves: 0, tile: [0, 1].into() })),
                    ]),
                    actions: Default::default(),
                    move_choices: vec![Some([0, 1].into())].into_iter().collect(),
                }).with_final(true).with_successors(vec![1, 2]))
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { before_moves: 0, tile: [0, 1].into() },
                        ConditionEnum::PiecePresent { before_moves: 0, tile: [0, 1].into() },
                        ConditionEnum::PieceControlledByEnemy { before_moves: 0, tile: [0, 1].into() },
                    ]),
                    actions: vec![ActionEnum::SetTile {
                        target: [0, 1].into(),
                        piece: None,
                    }].into_iter().collect(),
                    move_choices: vec![Some([0, 1].into())].into_iter().collect(),
                }).with_final(true))
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { before_moves: 0, tile: [1, 1].into() },
                        ConditionEnum::Not(Box::new(ConditionEnum::PiecePresent { before_moves: 0, tile: [1, 1].into() })),
                    ]),
                    actions: Default::default(),
                    move_choices: vec![Some([1, 1].into())].into_iter().collect(),
                }).with_final(true).with_successors(vec![3, 4]))
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { before_moves: 0, tile: [1, 1].into() },
                        ConditionEnum::PiecePresent { before_moves: 0, tile: [1, 1].into() },
                        ConditionEnum::PieceControlledByEnemy { before_moves: 0, tile: [1, 1].into() },
                    ]),
                    actions: vec![ActionEnum::SetTile {
                        target: [1, 1].into(),
                        piece: None,
                    }].into_iter().collect(),
                    move_choices: vec![Some([1, 1].into())].into_iter().collect(),
                }).with_final(true)),
            PieceDefinitionUnvalidated::new("King")
                .with_initial_state(StateUnvalidated::new(Action::Symmetry {
                    symmetries: SquareBoardGeometry::get_rotations().into_iter().collect(),
                }).with_successors(vec![1, 2]))
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { before_moves: 0, tile: [0, 1].into() },
                        ConditionEnum::PieceControlledByEnemy { before_moves: 0, tile: [0, 1].into() },
                    ]),
                    actions: vec![ActionEnum::SetTile {
                        target: [0, 1].into(),
                        piece: None,
                    }].into_iter().collect(),
                    move_choices: vec![Some([0, 1].into())].into_iter().collect(),
                }).with_final(true))
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { before_moves: 0, tile: [1, 1].into() },
                        ConditionEnum::PieceControlledByEnemy { before_moves: 0, tile: [1, 1].into() },
                    ]),
                    actions: vec![ActionEnum::SetTile {
                        target: [1, 1].into(),
                        piece: None,
                    }].into_iter().collect(),
                    move_choices: vec![Some([1, 1].into())].into_iter().collect(),
                }).with_final(true))
                // Castles (symmetrical)
                .with_initial_state(StateUnvalidated::new(Action::Symmetry {
                    symmetries: vec![Default::default(), SquareBoardGeometry::get_reflective_symmetries()[0].clone()].into_iter().collect(),
                }).with_successors(vec![4, 5]))
                // King side castle
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { before_moves: 0, tile: [1, 0].into() },
                        ConditionEnum::TilePresent { before_moves: 0, tile: [2, 0].into() },
                        ConditionEnum::TilePresent { before_moves: 0, tile: [3, 0].into() },
                        ConditionEnum::Not(Box::new(ConditionEnum::PiecePresent { before_moves: 0, tile: [1, 0].into() })),
                        ConditionEnum::Not(Box::new(ConditionEnum::PiecePresent { before_moves: 0, tile: [2, 0].into() })),
                        ConditionEnum::PiecePresent { before_moves: 0, tile: [3, 0].into() },
                        ConditionEnum::PieceTypeIs { before_moves: 0, tile: [3, 0].into(), definition_index: PIECE_ROOK },
                        ConditionEnum::PieceControlledByAlly { before_moves: 0, tile: [3, 0].into() },
                        ConditionEnum::PieceInitial { before_moves: 0, tile: [0, 0].into() },
                        ConditionEnum::PieceInitial { before_moves: 0, tile: [3, 0].into() },
                    ]),
                    actions: vec![
                        ActionEnum::CopyTile {
                            source: [3, 0].into(),
                            target: [1, 0].into(),
                        },
                        ActionEnum::SetTile {
                            target: [3, 0].into(),
                            piece: None,
                        },
                    ].into_iter().collect(),
                    move_choices: vec![Some([2, 0].into())].into_iter().collect(),
                }).with_final(true))
                // Queen side castle
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { before_moves: 0, tile: [1, 0].into() },
                        ConditionEnum::TilePresent { before_moves: 0, tile: [2, 0].into() },
                        ConditionEnum::TilePresent { before_moves: 0, tile: [3, 0].into() },
                        ConditionEnum::TilePresent { before_moves: 0, tile: [4, 0].into() },
                        ConditionEnum::Not(Box::new(ConditionEnum::PiecePresent { before_moves: 0, tile: [1, 0].into() })),
                        ConditionEnum::Not(Box::new(ConditionEnum::PiecePresent { before_moves: 0, tile: [2, 0].into() })),
                        ConditionEnum::Not(Box::new(ConditionEnum::PiecePresent { before_moves: 0, tile: [3, 0].into() })),
                        ConditionEnum::PiecePresent { before_moves: 0, tile: [4, 0].into() },
                        ConditionEnum::PieceTypeIs { before_moves: 0, tile: [4, 0].into(), definition_index: PIECE_ROOK },
                        ConditionEnum::PieceControlledByAlly { before_moves: 0, tile: [4, 0].into() },
                        ConditionEnum::PieceInitial { before_moves: 0, tile: [0, 0].into() },
                        ConditionEnum::PieceInitial { before_moves: 0, tile: [4, 0].into() },
                    ]),
                    actions: vec![
                        ActionEnum::CopyTile {
                            source: [4, 0].into(),
                            target: [1, 0].into(),
                        },
                        ActionEnum::SetTile {
                            target: [4, 0].into(),
                            piece: None,
                        },
                    ].into_iter().collect(),
                    move_choices: vec![Some([2, 0].into())].into_iter().collect(),
                }).with_final(true))
        ];

        PieceSet::from(definitions).unwrap()
    };

    static ref GAME_BOARD: Board<SquareBoardGeometry> = Board {
        tiles: {
            let mut tiles = BoardTiles::empty();

            for y in 0..8 {
                for x in 0..8 {
                    tiles.set([x, y].into(), true);
                }
            }

            tiles
        },
    };

    static ref GAME_STATE_INITIAL: GameState<SquareBoardGeometry> = {
        static P: usize = PIECE_PAWN;
        static R: usize = PIECE_ROOK;
        static N: usize = PIECE_KNIGHT;
        static B: usize = PIECE_BISHOP;
        static Q: usize = PIECE_QUEEN;
        static K: usize = PIECE_KING;
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
                    let mut tile = game_state.tile_mut(&GAME_BOARD, coords).unwrap();

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

    static ref GAME_RULES: GameRules<SquareBoardGeometry> = GameRules {
        board: GAME_BOARD.clone(),
        piece_set: PIECE_SET.clone(),
        players: NonZeroUsize::new(2).unwrap(),
        victory_conditions: Box::new(
            PredictiveVictoryConditions::new(
                RoyalVictoryConditions::new(RoyalVictoryType::Absolute, &*GAME_STATE_INITIAL)
                    .with_min_piece_count(PLAYER_WHITE, PIECE_KING, 1)
                    .with_min_piece_count(PLAYER_BLACK, PIECE_KING, 1)
            )
        ),
    };

    pub static ref GAME: Game<SquareBoardGeometry> = Game::new(
        GAME_RULES.clone(),
        GAME_STATE_INITIAL.clone(),
    );
}

pub mod tiles {
    use crate::math::IVec2;

    const fn ivec2(x: i32, y: i32) -> IVec2 {
        unsafe {
            std::mem::transmute::<[i32; 2], IVec2>([x, y])
        }
    }

    pub const A1: IVec2 = ivec2(0, 0);
    pub const A2: IVec2 = ivec2(0, 1);
    pub const A3: IVec2 = ivec2(0, 2);
    pub const A4: IVec2 = ivec2(0, 3);
    pub const A5: IVec2 = ivec2(0, 4);
    pub const A6: IVec2 = ivec2(0, 5);
    pub const A7: IVec2 = ivec2(0, 6);
    pub const A8: IVec2 = ivec2(0, 7);

    pub const B1: IVec2 = ivec2(1, 0);
    pub const B2: IVec2 = ivec2(1, 1);
    pub const B3: IVec2 = ivec2(1, 2);
    pub const B4: IVec2 = ivec2(1, 3);
    pub const B5: IVec2 = ivec2(1, 4);
    pub const B6: IVec2 = ivec2(1, 5);
    pub const B7: IVec2 = ivec2(1, 6);
    pub const B8: IVec2 = ivec2(1, 7);

    pub const C1: IVec2 = ivec2(2, 0);
    pub const C2: IVec2 = ivec2(2, 1);
    pub const C3: IVec2 = ivec2(2, 2);
    pub const C4: IVec2 = ivec2(2, 3);
    pub const C5: IVec2 = ivec2(2, 4);
    pub const C6: IVec2 = ivec2(2, 5);
    pub const C7: IVec2 = ivec2(2, 6);
    pub const C8: IVec2 = ivec2(2, 7);

    pub const D1: IVec2 = ivec2(3, 0);
    pub const D2: IVec2 = ivec2(3, 1);
    pub const D3: IVec2 = ivec2(3, 2);
    pub const D4: IVec2 = ivec2(3, 3);
    pub const D5: IVec2 = ivec2(3, 4);
    pub const D6: IVec2 = ivec2(3, 5);
    pub const D7: IVec2 = ivec2(3, 6);
    pub const D8: IVec2 = ivec2(3, 7);

    pub const E1: IVec2 = ivec2(4, 0);
    pub const E2: IVec2 = ivec2(4, 1);
    pub const E3: IVec2 = ivec2(4, 2);
    pub const E4: IVec2 = ivec2(4, 3);
    pub const E5: IVec2 = ivec2(4, 4);
    pub const E6: IVec2 = ivec2(4, 5);
    pub const E7: IVec2 = ivec2(4, 6);
    pub const E8: IVec2 = ivec2(4, 7);

    pub const F1: IVec2 = ivec2(5, 0);
    pub const F2: IVec2 = ivec2(5, 1);
    pub const F3: IVec2 = ivec2(5, 2);
    pub const F4: IVec2 = ivec2(5, 3);
    pub const F5: IVec2 = ivec2(5, 4);
    pub const F6: IVec2 = ivec2(5, 5);
    pub const F7: IVec2 = ivec2(5, 6);
    pub const F8: IVec2 = ivec2(5, 7);

    pub const G1: IVec2 = ivec2(6, 0);
    pub const G2: IVec2 = ivec2(6, 1);
    pub const G3: IVec2 = ivec2(6, 2);
    pub const G4: IVec2 = ivec2(6, 3);
    pub const G5: IVec2 = ivec2(6, 4);
    pub const G6: IVec2 = ivec2(6, 5);
    pub const G7: IVec2 = ivec2(6, 6);
    pub const G8: IVec2 = ivec2(6, 7);

    pub const H1: IVec2 = ivec2(7, 0);
    pub const H2: IVec2 = ivec2(7, 1);
    pub const H3: IVec2 = ivec2(7, 2);
    pub const H4: IVec2 = ivec2(7, 3);
    pub const H5: IVec2 = ivec2(7, 4);
    pub const H6: IVec2 = ivec2(7, 5);
    pub const H7: IVec2 = ivec2(7, 6);
    pub const H8: IVec2 = ivec2(7, 7);
}