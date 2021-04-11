use std::num::NonZeroUsize;
use lazy_static::lazy_static;
use crate::victory_conditions::*;
use crate::*;

// TODO: Consider changing these to enums
pub static PLAYER_WHITE: PlayerIndex = 0;
pub static PLAYER_BLACK: PlayerIndex = 1;

pub static PIECE_PAWN: PieceDefinitionIndex   = 0;
pub static PIECE_ROOK: PieceDefinitionIndex   = 1;
pub static PIECE_KNIGHT: PieceDefinitionIndex = 2;
pub static PIECE_BISHOP: PieceDefinitionIndex = 3;
pub static PIECE_QUEEN: PieceDefinitionIndex  = 4;
pub static PIECE_KING: PieceDefinitionIndex   = 5;

pub static TILE_FLAG_PROMOTION_WHITE: TileFlagIndex = 0;
pub static TILE_FLAG_PROMOTION_BLACK: TileFlagIndex = 1;

pub mod pieces {
    use super::*;

    pub static X: Option<PieceDefinitionIndex> = None;

    pub static P: Option<PieceDefinitionIndex> = Some(PIECE_PAWN);
    pub static R: Option<PieceDefinitionIndex> = Some(PIECE_ROOK);
    pub static N: Option<PieceDefinitionIndex> = Some(PIECE_KNIGHT);
    pub static B: Option<PieceDefinitionIndex> = Some(PIECE_BISHOP);
    pub static Q: Option<PieceDefinitionIndex> = Some(PIECE_QUEEN);
    pub static K: Option<PieceDefinitionIndex> = Some(PIECE_KING);

    pub static __: Option<(PlayerIndex, PieceDefinitionIndex)> = None;

    pub static WP: Option<(PlayerIndex, PieceDefinitionIndex)> = Some((PLAYER_WHITE, PIECE_PAWN));
    pub static WR: Option<(PlayerIndex, PieceDefinitionIndex)> = Some((PLAYER_WHITE, PIECE_ROOK));
    pub static WN: Option<(PlayerIndex, PieceDefinitionIndex)> = Some((PLAYER_WHITE, PIECE_KNIGHT));
    pub static WB: Option<(PlayerIndex, PieceDefinitionIndex)> = Some((PLAYER_WHITE, PIECE_BISHOP));
    pub static WQ: Option<(PlayerIndex, PieceDefinitionIndex)> = Some((PLAYER_WHITE, PIECE_QUEEN));
    pub static WK: Option<(PlayerIndex, PieceDefinitionIndex)> = Some((PLAYER_WHITE, PIECE_KING));

    pub static BP: Option<(PlayerIndex, PieceDefinitionIndex)> = Some((PLAYER_BLACK, PIECE_PAWN));
    pub static BR: Option<(PlayerIndex, PieceDefinitionIndex)> = Some((PLAYER_BLACK, PIECE_ROOK));
    pub static BN: Option<(PlayerIndex, PieceDefinitionIndex)> = Some((PLAYER_BLACK, PIECE_KNIGHT));
    pub static BB: Option<(PlayerIndex, PieceDefinitionIndex)> = Some((PLAYER_BLACK, PIECE_BISHOP));
    pub static BQ: Option<(PlayerIndex, PieceDefinitionIndex)> = Some((PLAYER_BLACK, PIECE_QUEEN));
    pub static BK: Option<(PlayerIndex, PieceDefinitionIndex)> = Some((PLAYER_BLACK, PIECE_KING));
}

pub fn create_initial_state(pieces: [[Option<(PlayerIndex, PieceDefinitionIndex)>; 8]; 8]) -> GameState<SquareBoardGeometry> {
    let mut game_state = GameState::<SquareBoardGeometry>::default();
    let player_to_axis_permutation = [
        AxisPermutation::default(),
        SquareBoardGeometry::get_reflective_symmetries()[0].clone() * SquareBoardGeometry::get_rotations()[2].clone(),
    ];

    for (y, tile_flag_promotion) in std::array::IntoIter::new([(0, TILE_FLAG_PROMOTION_BLACK), (7, TILE_FLAG_PROMOTION_WHITE)]) {
        for x in 0..8 {
            let coords = [x, y].into();
            let mut tile = game_state.tile_mut(&GAME_BOARD, coords).unwrap();
            tile.set_flag(tile_flag_promotion, true);
        }
    }

    for (y, pieces_row) in pieces.iter().rev().enumerate() {
        for (x, piece) in pieces_row.iter().enumerate() {
            if let Some((player, piece)) = piece {
                let coords: <SquareBoardGeometry as BoardGeometryExt>::Tile = [x as IVecComponent, y as IVecComponent].into();
                let mut tile = game_state.tile_mut(&GAME_BOARD, coords).unwrap();

                tile.set_piece(Some(Piece {
                    definition: *piece,
                    owner: *player,
                    transformation: player_to_axis_permutation[*player as usize].clone(),
                    affecting_moves: Default::default(),
                }));
            }
        }
    }

    game_state
}

pub fn create_initial_state_symmetrical(white_pieces: &[[Option<PieceDefinitionIndex>; 8]]) -> GameState<SquareBoardGeometry> {
    assert!(white_pieces.len() <= 4);

    let mut game_state = GameState::<SquareBoardGeometry>::default();
    let sides: [(PlayerIndex, TileFlagIndex, Isometry<SquareBoardGeometry>); 2] = [
        (PLAYER_WHITE, TILE_FLAG_PROMOTION_BLACK, Isometry::default()),
        (PLAYER_BLACK, TILE_FLAG_PROMOTION_WHITE, Isometry::from(SquareBoardGeometry::get_reflective_symmetries()[0].clone() * SquareBoardGeometry::get_rotations()[2].clone()) * Isometry::<SquareBoardGeometry>::translation([0, 7].into())),
    ];

    for (player, tile_flag_promotion, isometry) in &sides {
        for (y_inv, pieces_row) in white_pieces.iter().enumerate() {
            let y = 1 - y_inv;
            let promote = y == 0;

            for (x, piece) in pieces_row.iter().enumerate() {
                let mut coords: <SquareBoardGeometry as BoardGeometryExt>::Tile = [x as IVecComponent, y as IVecComponent].into();
                coords = isometry.apply(coords);
                let mut tile = game_state.tile_mut(&GAME_BOARD, coords).unwrap();

                if let Some(piece) = piece {
                    tile.set_piece(Some(Piece {
                        definition: *piece,
                        owner: *player,
                        transformation: isometry.axis_permutation.clone(),
                        affecting_moves: Default::default(),
                    }));
                }

                if promote {
                    tile.set_flag(*tile_flag_promotion, true);
                }
            }
        }
    }

    game_state
}

lazy_static! {
    pub static ref PIECE_SET: PieceSet<SquareBoardGeometry> = {
        let definitions = vec![
            PieceDefinitionUnvalidated::new("Pawn")
                // Pawns' moves are vertically symmetrical
                .with_initial_state(StateUnvalidated::new(Action::Symmetry {
                    symmetries: vec![
                        Default::default(),
                        SquareBoardGeometry::get_reflective_symmetries()[0].clone(),
                    ].into_iter().collect(),
                }).with_successors(vec![1, 2, 3, 4]))
                // Advance by 1 tile
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { tile: [0, 1].into() },
                        ConditionEnum::not(ConditionEnum::PiecePresent { tile: [0, 1].into() }),
                    ]),
                    actions: Default::default(),
                    move_choices: vec![[0, 1].into()].into_iter().collect(),
                }).with_successors(vec![5]))
                // Advance by 2 tiles
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::PieceInitial { tile: [0, 0].into() },
                        ConditionEnum::TilePresent { tile: [0, 1].into() },
                        ConditionEnum::TilePresent { tile: [0, 2].into() },
                        ConditionEnum::not(ConditionEnum::PiecePresent { tile: [0, 1].into() }),
                        ConditionEnum::not(ConditionEnum::PiecePresent { tile: [0, 2].into() }),
                    ]),
                    actions: Default::default(),
                    move_choices: vec![[0, 2].into()].into_iter().collect(),
                }).with_marked(true).with_successors(vec![5]))
                // Capture diagonally
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { tile: [1, 1].into() },
                        ConditionEnum::PiecePresent { tile: [1, 1].into() },
                        ConditionEnum::PieceControlledByEnemy { tile: [1, 1].into() },
                    ]),
                    actions: Default::default(),
                    move_choices: vec![[1, 1].into()].into_iter().collect(),
                }).with_successors(vec![5]))
                // En-passant
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        // Check that at least one move was played before the current move.
                        ConditionEnum::MovesPlayedGreaterThanOrEqual(1),
                        // The preceding move was a pawn move.
                        ConditionEnum::MoveGeneratedByPiece { past_move: 1, piece_definition: PIECE_PAWN },
                        // The preceding move was a pawn's 2-tile advancement move.
                        ConditionEnum::MoveGeneratedByVisitingMarkedState { past_move: 1, state_index: 2 },
                        // There is a tile right of the pawn.
                        ConditionEnum::TilePresent { tile: [1, 0].into() },
                        // There is a piece right of the pawn.
                        ConditionEnum::PiecePresent { tile: [1, 0].into() },
                        // That piece is an enemy piece.
                        ConditionEnum::PieceControlledByEnemy { tile: [1, 0].into() },
                        // That piece is affected by the pawn's 2-tile advancement move, meaning it
                        // was the pawn, because the move does not affect any other piece.
                        //
                        // Note: We do not check whether the piece is of a specific type, because
                        // it may have been promoted, if the board is set up in a different way
                        // than the standard board of international chess.
                        ConditionEnum::PieceAffectedByMove { past_move: 1, tile: [1, 0].into() },
                    ]),
                    actions: vec![ActionEnum::SetTile {
                        target: [1, 0].into(),
                        piece: None,
                    }].into_iter().collect(),
                    move_choices: vec![[1, 1].into()].into_iter().collect(),
                }).with_successors(vec![5]))
                // Finish move unless on last rank, then promotion is mandatory.
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![]),
                    actions: Default::default(),
                    move_choices: vec![[0, 0].into()].into_iter().collect(),
                }).with_successors(vec![
                    6, // no promotion
                    7, // promotion
                ]))
                // If not on last rank, end move.
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::not(ConditionEnum::any(vec![
                        ConditionEnum::all(vec![
                            ConditionEnum::PieceControlledBy {
                                
                                tile: [0, 0].into(),
                                player: PLAYER_WHITE,
                            },
                            ConditionEnum::TileFlagPresent {
                                
                                tile: [0, 0].into(),
                                flag: TILE_FLAG_PROMOTION_WHITE,
                            },
                        ]),
                        ConditionEnum::all(vec![
                            ConditionEnum::PieceControlledBy {
                                
                                tile: [0, 0].into(),
                                player: PLAYER_BLACK,
                            },
                            ConditionEnum::TileFlagPresent {
                                
                                tile: [0, 0].into(),
                                flag: TILE_FLAG_PROMOTION_BLACK,
                            },
                        ]),
                    ])),
                    actions: Default::default(),
                    move_choices: vec![[0, 0].into()].into_iter().collect(),
                }).with_final(true))
                // If on last rank, require promotion.
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::any(vec![
                        ConditionEnum::all(vec![
                            ConditionEnum::PieceControlledBy {
                                
                                tile: [0, 0].into(),
                                player: PLAYER_WHITE,
                            },
                            ConditionEnum::TileFlagPresent {
                                
                                tile: [0, 0].into(),
                                flag: TILE_FLAG_PROMOTION_WHITE,
                            },
                        ]),
                        ConditionEnum::all(vec![
                            ConditionEnum::PieceControlledBy {
                                
                                tile: [0, 0].into(),
                                player: PLAYER_BLACK,
                            },
                            ConditionEnum::TileFlagPresent {
                                
                                tile: [0, 0].into(),
                                flag: TILE_FLAG_PROMOTION_BLACK,
                            },
                        ]),
                    ]),
                    actions: Default::default(),
                    move_choices: vec![[0, 0].into()].into_iter().collect(),
                }).with_successors(vec![
                    8, // rook
                    9, // knight
                    10, // bishop
                    11, // queen
                ]))
                // Promotion to rook
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![]),
                    actions: vec![
                        ActionEnum::SetTile {
                            target: [0, 0].into(),
                            piece: Some(PIECE_ROOK),
                        },
                    ].into_iter().collect(),
                    move_choices: vec![[0, 0].into()].into_iter().collect(),
                }).with_final(true))
                // Promotion to knight
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![]),
                    actions: vec![
                        ActionEnum::SetTile {
                            target: [0, 0].into(),
                            piece: Some(PIECE_KNIGHT),
                        },
                    ].into_iter().collect(),
                    move_choices: vec![[0, 0].into()].into_iter().collect(),
                }).with_final(true))
                // Promotion to bishop
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![]),
                    actions: vec![
                        ActionEnum::SetTile {
                            target: [0, 0].into(),
                            piece: Some(PIECE_BISHOP),
                        },
                    ].into_iter().collect(),
                    move_choices: vec![[0, 0].into()].into_iter().collect(),
                }).with_final(true))
                // Promotion to queen
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![]),
                    actions: vec![
                        ActionEnum::SetTile {
                            target: [0, 0].into(),
                            piece: Some(PIECE_QUEEN),
                        },
                    ].into_iter().collect(),
                    move_choices: vec![[0, 0].into()].into_iter().collect(),
                }).with_final(true)),
            PieceDefinitionUnvalidated::new("Rook")
                // Rooks' moves are rotationally symmetrical
                .with_initial_state(StateUnvalidated::new(Action::Symmetry {
                    symmetries: SquareBoardGeometry::get_rotations().into_iter().collect(),
                }).with_successors(vec![1, 2]))
                // Move to the right 0 or more times
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { tile: [0, 1].into() },
                        ConditionEnum::not(ConditionEnum::PiecePresent { tile: [0, 1].into() }),
                    ]),
                    actions: Default::default(),
                    move_choices: vec![[0, 1].into()].into_iter().collect(),
                }).with_final(true).with_successors(vec![1, 2]))
                // Capture a move to the right
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { tile: [0, 1].into() },
                        ConditionEnum::PiecePresent { tile: [0, 1].into() },
                        ConditionEnum::PieceControlledByEnemy { tile: [0, 1].into() },
                    ]),
                    actions: Default::default(),
                    move_choices: vec![[0, 1].into()].into_iter().collect(),
                }).with_final(true)),
            PieceDefinitionUnvalidated::new("Knight")
                // Knights' moves are both rotationally and reflectively symmetrical
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
                // Move or capture with a knight
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { tile: [2, 1].into() },
                        ConditionEnum::PieceControlledByEnemy { tile: [2, 1].into() },
                    ]),
                    actions: Default::default(),
                    move_choices: vec![[2, 1].into()].into_iter().collect(),
                }).with_final(true)),
            PieceDefinitionUnvalidated::new("Bishop")
                // Bishop' moves are rotationally symmetrical
                .with_initial_state(StateUnvalidated::new(Action::Symmetry {
                    symmetries: SquareBoardGeometry::get_rotations().into_iter().collect(),
                }).with_successors(vec![1, 2]))
                // Move to the top right 0 or more times
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { tile: [1, 1].into() },
                        ConditionEnum::not(ConditionEnum::PiecePresent { tile: [1, 1].into() }),
                    ]),
                    actions: Default::default(),
                    move_choices: vec![[1, 1].into()].into_iter().collect(),
                }).with_final(true).with_successors(vec![1, 2]))
                // Capture a move to the top right
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { tile: [1, 1].into() },
                        ConditionEnum::PiecePresent { tile: [1, 1].into() },
                        ConditionEnum::PieceControlledByEnemy { tile: [1, 1].into() },
                    ]),
                    actions: Default::default(),
                    move_choices: vec![[1, 1].into()].into_iter().collect(),
                }).with_final(true)),
            PieceDefinitionUnvalidated::new("Queen")
                // Queens' moves are rotationally symmetrical
                .with_initial_state(StateUnvalidated::new(Action::Symmetry {
                    symmetries: SquareBoardGeometry::get_rotations().into_iter().collect(),
                }).with_successors(vec![1, 2, 3, 4]))
                // Move upwards 0 or more times
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { tile: [0, 1].into() },
                        ConditionEnum::not(ConditionEnum::PiecePresent { tile: [0, 1].into() }),
                    ]),
                    actions: Default::default(),
                    move_choices: vec![[0, 1].into()].into_iter().collect(),
                }).with_final(true).with_successors(vec![1, 2]))
                // Capture upwards
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { tile: [0, 1].into() },
                        ConditionEnum::PiecePresent { tile: [0, 1].into() },
                        ConditionEnum::PieceControlledByEnemy { tile: [0, 1].into() },
                    ]),
                    actions: Default::default(),
                    move_choices: vec![[0, 1].into()].into_iter().collect(),
                }).with_final(true))
                // Move to the top right 0 or more times
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { tile: [1, 1].into() },
                        ConditionEnum::not(ConditionEnum::PiecePresent { tile: [1, 1].into() }),
                    ]),
                    actions: Default::default(),
                    move_choices: vec![[1, 1].into()].into_iter().collect(),
                }).with_final(true).with_successors(vec![3, 4]))
                // Capture to the top right
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { tile: [1, 1].into() },
                        ConditionEnum::PiecePresent { tile: [1, 1].into() },
                        ConditionEnum::PieceControlledByEnemy { tile: [1, 1].into() },
                    ]),
                    actions: Default::default(),
                    move_choices: vec![[1, 1].into()].into_iter().collect(),
                }).with_final(true)),
            PieceDefinitionUnvalidated::new("King")
                // Kings' moves are rotationally symmetrical
                .with_initial_state(StateUnvalidated::new(Action::Symmetry {
                    symmetries: SquareBoardGeometry::get_rotations().into_iter().collect(),
                }).with_successors(vec![1, 2]))
                // Move or capture to the right
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { tile: [0, 1].into() },
                        ConditionEnum::PieceControlledByEnemy { tile: [0, 1].into() },
                    ]),
                    actions: Default::default(),
                    move_choices: vec![[0, 1].into()].into_iter().collect(),
                }).with_final(true))
                // Move or capture to the top right
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        ConditionEnum::TilePresent { tile: [1, 1].into() },
                        ConditionEnum::PieceControlledByEnemy { tile: [1, 1].into() },
                    ]),
                    actions: Default::default(),
                    move_choices: vec![[1, 1].into()].into_iter().collect(),
                }).with_final(true))
                // Castles (symmetrical)
                //
                // Requirements for castling, as defined by FIDE:
                // * The castling must be kingside or queenside.
                //     - Kingside is handled by state #4, queenside by state #7.
                // * Neither the king nor the chosen rook has previously moved.
                //     - Ensured using `PieceInitial` in states #4 and #7.
                // * There are no pieces between the king and the chosen rook.
                //     - Ensured using `TilePresent` and `PiecePresent` conditions in state #4 and #7.
                // * The king is not currently in check and the king does not pass
                //   through a square that is attacked by an enemy piece.
                //     - Ensured by moving the king by one tile per state and checking that the
                //       unfinished move is legal using `UnfinishedMoveLegal`.
                // * The king does not end up in check. (True of any legal move.)
                .with_initial_state(StateUnvalidated::new(Action::Symmetry {
                    symmetries: vec![Default::default(), SquareBoardGeometry::get_reflective_symmetries()[0].clone()].into_iter().collect(),
                }).with_successors(vec![4, 7]))
                // King side castle
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        // Ensure tiles are present up to the rook
                        ConditionEnum::TilePresent { tile: [1, 0].into() },
                        ConditionEnum::TilePresent { tile: [2, 0].into() },
                        ConditionEnum::TilePresent { tile: [3, 0].into() },
                        // Ensure no pieces are inbetween the king and the rook
                        ConditionEnum::not(ConditionEnum::PiecePresent { tile: [1, 0].into() }),
                        ConditionEnum::not(ConditionEnum::PiecePresent { tile: [2, 0].into() }),
                        // Ensure a piece is on the H file
                        ConditionEnum::PiecePresent { tile: [3, 0].into() },
                        // Ensure that piece is a rook
                        ConditionEnum::PieceTypeIs { tile: [3, 0].into(), definition_index: PIECE_ROOK },
                        // Ensure that piece is allied
                        ConditionEnum::PieceControlledByAlly { tile: [3, 0].into() },
                        // Ensure both the rook and the king have not been moved or affected by a
                        // move prior to castling
                        ConditionEnum::PieceInitial { tile: [0, 0].into() },
                        ConditionEnum::PieceInitial { tile: [3, 0].into() },
                        // Ensure the king is not currently in check
                        ConditionEnum::UnfinishedMoveLegal,
                    ]),
                    actions: Default::default(),
                    move_choices: vec![[1, 0].into()].into_iter().collect(),
                }).with_successors(vec![5]))
                // The king was moved one tile towards the king-side rook
                .with_state(StateUnvalidated::new(Action::Move {
                    // Ensure the king is not currently in check
                    condition: ConditionEnum::UnfinishedMoveLegal,
                    actions: Default::default(),
                    move_choices: vec![[1, 0].into()].into_iter().collect(),
                }).with_successors(vec![6]))
                // The king was moved two tiles towards the king-side rook, into its final position
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::always(),
                    actions: vec![
                        ActionEnum::CopyTile {
                            source: [1, 0].into(),
                            target: [-1, 0].into(),
                        },
                        ActionEnum::SetTile {
                            target: [1, 0].into(),
                            piece: None,
                        },
                    ].into_iter().collect(),
                    move_choices: vec![[0, 0].into()].into_iter().collect(),
                }).with_final(true))
                // Queen side castle
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::all(vec![
                        // Ensure tiles are present up to the rook
                        ConditionEnum::TilePresent { tile: [1, 0].into() },
                        ConditionEnum::TilePresent { tile: [2, 0].into() },
                        ConditionEnum::TilePresent { tile: [3, 0].into() },
                        ConditionEnum::TilePresent { tile: [4, 0].into() },
                        // Ensure no pieces are inbetween the king and the rook
                        ConditionEnum::not(ConditionEnum::PiecePresent { tile: [1, 0].into() }),
                        ConditionEnum::not(ConditionEnum::PiecePresent { tile: [2, 0].into() }),
                        ConditionEnum::not(ConditionEnum::PiecePresent { tile: [3, 0].into() }),
                        // Ensure a piece is on the A file
                        ConditionEnum::PiecePresent { tile: [4, 0].into() },
                        // Ensure that piece is a rook
                        ConditionEnum::PieceTypeIs { tile: [4, 0].into(), definition_index: PIECE_ROOK },
                        // Ensure that piece is allied
                        ConditionEnum::PieceControlledByAlly { tile: [4, 0].into() },
                        // Ensure both the rook and the king have not been moved or affected by a
                        // move prior to castling
                        ConditionEnum::PieceInitial { tile: [0, 0].into() },
                        ConditionEnum::PieceInitial { tile: [4, 0].into() },
                        // Ensure the king is not currently in check
                        ConditionEnum::UnfinishedMoveLegal,
                    ]),
                    actions: Default::default(),
                    move_choices: vec![[1, 0].into()].into_iter().collect(),
                }).with_successors(vec![8]))
                // The king was moved one tile towards the queen-side rook
                .with_state(StateUnvalidated::new(Action::Move {
                    // Ensure the king is not currently in check
                    condition: ConditionEnum::UnfinishedMoveLegal,
                    actions: Default::default(),
                    move_choices: vec![[1, 0].into()].into_iter().collect(),
                }).with_successors(vec![9]))
                // The king was moved two tiles towards the queen-side rook, into its final position
                .with_state(StateUnvalidated::new(Action::Move {
                    condition: ConditionEnum::always(),
                    actions: vec![
                        ActionEnum::CopyTile {
                            source: [2, 0].into(),
                            target: [-1, 0].into(),
                        },
                        ActionEnum::SetTile {
                            target: [2, 0].into(),
                            piece: None,
                        },
                    ].into_iter().collect(),
                    move_choices: vec![[0, 0].into()].into_iter().collect(),
                }).with_final(true))
        ];

        PieceSet::from(definitions).unwrap()
    };

    static ref GAME_BOARD: Board<SquareBoardGeometry> = Board::new({
        let mut tiles = BoardTiles::empty();

        for y in 0..8 {
            for x in 0..8 {
                tiles.set([x, y].into(), true);
            }
        }

        tiles
    });

    static ref GAME_STATE_INITIAL: GameState<SquareBoardGeometry> = {
        use pieces::*;
        create_initial_state_symmetrical(&[
            [P, P, P, P, P, P, P, P],
            [R, N, B, Q, K, B, N, R],
        ])
    };

    static ref GAME_RULES: GameRules<SquareBoardGeometry> = GameRules {
        board: GAME_BOARD.clone(),
        piece_set: PIECE_SET.clone(),
        players: NonZeroUsize::new(2).unwrap(),
        victory_conditions: Box::new(
            PredictiveVictoryCondition::new(
                StalemateEvaluation::Draw,
                RoyalVictoryCondition::new(RoyalVictoryType::Absolute, &*GAME_STATE_INITIAL)
                    .with_min_piece_count(PLAYER_WHITE, PIECE_KING, 1)
                    .with_min_piece_count(PLAYER_BLACK, PIECE_KING, 1),
            )
        ),
    };

    pub static ref GAME: Game<SquareBoardGeometry> = Game::new(
        GAME_RULES.clone(),
        GAME_STATE_INITIAL.clone(),
    );
}

pub mod tiles {
    use crate::math::{IVec2, IVecComponent};

    const fn ivec2(x: IVecComponent, y: IVecComponent) -> IVec2 {
        unsafe {
            std::mem::transmute::<[IVecComponent; 2], IVec2>([x, y])
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
