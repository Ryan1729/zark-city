extern crate rand;
extern crate common;

use common::*;
use common::Projection::*;
use common::Turn::*;
use common::Pips::*;
use common::Value::*;
use common::PiecesLeft::*;
use common::Highlighted::*;

use std::default::Default;

use rand::{StdRng, SeedableRng, Rng};

use std::collections::hash_map::Entry::Occupied;

#[cfg(debug_assertions)]
#[no_mangle]
pub fn new_state() -> State {
    println!("debug on");

    let seed: &[_] = &[42];
    let rng: StdRng = SeedableRng::from_seed(seed);

    make_state(rng)
}
#[cfg(not(debug_assertions))]
#[no_mangle]
pub fn new_state() -> State {
    let timestamp = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|dur| dur.as_secs())
        .unwrap_or(42);

    println!("{}", timestamp);
    let seed: &[_] = &[timestamp as usize];
    let rng: StdRng = SeedableRng::from_seed(seed);

    make_state(rng)
}


fn deal(state: &mut State) -> Option<Card> {
    deal_parts(&mut state.deck, &mut state.pile, &mut state.rng)
}

fn deal_parts(deck: &mut Vec<Card>, pile: &mut Vec<Card>, rng: &mut StdRng) -> Option<Card> {
    //reshuffle if we run out of cards.
    if deck.len() == 0 {
        if pile.len() == 0 {
            return None;
        }

        for card in pile.drain(..) {
            deck.push(card);
        }

        rng.shuffle(deck.as_mut_slice());
    };

    deck.pop()
}

fn make_state(mut rng: StdRng) -> State {

    let mut deck = Card::all_values();

    rng.shuffle(deck.as_mut_slice());

    debug_assert!(deck.len() > MAX_PLAYERS * 3, "Not enough cards!");

    let cpu_players_count = rng.gen_range(1, MAX_PLAYERS);

    let mut pile = Vec::new();
    let player_hand;
    let mut cpu_hands;
    {
        let deck_ref = &mut deck;
        let pile_ref = &mut pile;
        let rng_ref = &mut rng;

        player_hand = vec![
            deal_parts(deck_ref, pile_ref, rng_ref).unwrap(),
            deal_parts(deck_ref, pile_ref, rng_ref).unwrap(),
            deal_parts(deck_ref, pile_ref, rng_ref).unwrap(),
        ];

        cpu_hands = Vec::new();

        for _ in 0..cpu_players_count {
            cpu_hands.push(vec![
                deal_parts(deck_ref, pile_ref, rng_ref).unwrap(),
                deal_parts(deck_ref, pile_ref, rng_ref).unwrap(),
                deal_parts(deck_ref, pile_ref, rng_ref).unwrap(),
            ]);
        }
    }

    let mut colour_deck = PieceColour::all_values();

    rng.shuffle(colour_deck.as_mut_slice());

    debug_assert!(
        colour_deck.len() >= MAX_PLAYERS,
        "Not enough piece colours!"
    );

    let player_stash = Stash::full(colour_deck.pop().unwrap());

    let mut cpu_stashes = Vec::new();

    for _ in 0..cpu_players_count {
        cpu_stashes.push(Stash::full(colour_deck.pop().unwrap()));
    }

    let mut state = State {
        rng,
        cam_x: 0.0,
        cam_y: 0.0,
        zoom: f32::powi(1.25, 8),
        board: HashMap::new(),
        mouse_pos: (400.0, 300.0),
        window_wh: (INITIAL_WINDOW_WIDTH as _, INITIAL_WINDOW_HEIGHT as _),
        ui_context: UIContext::new(),
        mouse_held: false,
        turn: Fly,
        deck,
        pile: Vec::new(),
        player_hand,
        cpu_hands,
        player_stash,
        cpu_stashes,
        hud_alpha: 1.0,
        highlighted: PlayerOccupation,
    };

    add_random_board_card(&mut state);

    state
}

enum Action {
    SelectPiece((i8, i8), usize),
    SelectSpace((i8, i8)),
    PageBack((i8, i8)),
    PageForward((i8, i8)),
    SelectCardFromHand(usize),
    NoAction,
}
use Action::*;

const FADE_RATE: f32 = 1.0 / 24.0;
const TRANSLATION_SCALE: f32 = 0.0625;

#[no_mangle]
//returns true if quit requested
pub fn update_and_render(p: &Platform, state: &mut State, events: &mut Vec<Event>) -> bool {
    let mut mouse_pressed = false;
    let mut mouse_released = false;

    let mut right_mouse_pressed = false;
    let mut right_mouse_released = false;

    let mut escape_pressed = false;

    for event in events {
        if cfg!(debug_assertions) {
            match *event {
                Event::MouseMove(_) => {}
                _ => println!("{:?}", *event),
            }
        }

        match *event {
            Event::Quit |
            Event::KeyDown(Keycode::F10) => {
                return true;
            }
            Event::KeyDown(Keycode::Escape) => {
                escape_pressed = true;
            }
            Event::KeyDown(Keycode::Space) => {
                add_random_board_card(state);
            }
            Event::KeyDown(Keycode::R) => {
                if cfg!(debug_assertions) {

                    state.board.clear();
                    add_random_board_card(state);
                }
            }
            Event::KeyDown(Keycode::C) => {
                if cfg!(debug_assertions) {
                    if let Some(card) = deal(state) {
                        state.player_hand.push(card);
                    }
                }
            }
            Event::KeyDown(Keycode::V) => {
                (p.set_verts)(get_vert_vecs());
            }
            Event::KeyDown(Keycode::Up) => {
                state.cam_y += state.zoom * TRANSLATION_SCALE;
            }
            Event::KeyDown(Keycode::Down) => {
                state.cam_y -= state.zoom * TRANSLATION_SCALE;
            }
            Event::KeyDown(Keycode::Right) => {
                state.cam_x += state.zoom * TRANSLATION_SCALE;
            }
            Event::KeyDown(Keycode::Left) => {
                state.cam_x -= state.zoom * TRANSLATION_SCALE;
            }
            Event::KeyDown(Keycode::Num0) => {
                state.cam_x = 0.0;
                state.cam_y = 0.0;
                state.zoom = 1.0;
            }
            Event::KeyDown(Keycode::W) => {
                state.zoom *= 1.25;
            }
            Event::KeyDown(Keycode::S) => {
                state.zoom /= 1.25;
                if state.zoom == 0.0 {
                    state.zoom = std::f32::MIN_POSITIVE / TRANSLATION_SCALE;
                }
            }
            Event::MouseMove((x, y)) => {
                state.mouse_pos = (x as f32, y as f32);
            }
            Event::LeftMouseDown => {
                mouse_pressed = true;
            }
            Event::LeftMouseUp => {
                mouse_released = true;
            }
            Event::RightMouseDown => {
                right_mouse_pressed = true;
            }
            Event::RightMouseUp => {
                right_mouse_released = true;
            }
            Event::WindowSize((w, h)) => {
                state.window_wh = (w as f32, h as f32);
                println!("{}", state.window_wh.0 / state.window_wh.1);
            }
            _ => {}
        }
    }

    if mouse_released != mouse_pressed {
        if mouse_released {
            state.mouse_held = false;
        } else {
            state.mouse_held = true;
        }
    }

    let mouse_held = state.mouse_held;

    state.ui_context.frame_init();

    state.hud_alpha += if state.mouse_pos.1 / state.window_wh.1 > 0.675 {
        FADE_RATE
    } else {
        -FADE_RATE
    };

    state.hud_alpha = clamp(state.hud_alpha, 0.0, 1.0);

    let aspect_ratio = state.window_wh.0 / state.window_wh.1;

    let mouse_x = center((state.mouse_pos.0) / state.window_wh.0);
    let mouse_y = -center(((state.mouse_pos.1) / state.window_wh.1));

    let (view, inverse_view) = {
        let near = 0.5;
        let far = 1024.0;

        let scale = state.zoom * near;
        let top = scale;
        let bottom = -top;
        let right = aspect_ratio * scale;
        let left = -right;

        let projection = get_projection(&ProjectionSpec {
            top,
            bottom,
            left,
            right,
            near,
            far,
            projection: Perspective,
            // projection: Orthographic,
        });

        let inverse_projection = get_projection(&ProjectionSpec {
            top,
            bottom,
            left,
            right,
            near,
            far,
            projection: InversePerspective,
            // projection: InverseOrthographic,
        });

        let camera = scale_translation(1.0, state.cam_x, state.cam_y);

        let inverse_camera = scale_translation(1.0, -state.cam_x, -state.cam_y);

        let view = mat4x4_mul(&camera, &projection);
        let inverse_view = mat4x4_mul(&inverse_projection, &inverse_camera);

        (view, inverse_view)
    };

    let (world_mouse_x, world_mouse_y, _, _) =
        mat4x4_vector_mul_divide(&inverse_view, mouse_x, mouse_y, 0.0, 1.0);


    let mut action = NoAction;

    for (grid_coords,
         &Space {
             card,
             ref pieces,
             offset: space_offset,
         }) in state.board.iter()
    {
        let (card_x, card_y, rotated) = get_card_spec(grid_coords);

        let card_matrix = get_card_matrix(&view, (card_x, card_y, rotated));

        let card_id = card_id(*grid_coords);

        let mut card_texture_spec = card.texture_spec();

        let (card_mouse_x, card_mouse_y) = (world_mouse_x - card_x, world_mouse_y - card_y);

        let on_card = if rotated {
            (card_mouse_x).abs() <= 1.0 && (card_mouse_y).abs() <= CARD_RATIO
        } else {
            (card_mouse_x).abs() <= CARD_RATIO && (card_mouse_y).abs() <= 1.0
        };

        let button_outcome = button_logic(
            &mut state.ui_context,
            ButtonState {
                id: card_id,
                pointer_inside: on_card,
                mouse_pressed,
                mouse_released,
                mouse_held,
            },
        );

        if button_outcome.clicked {
            action = SelectSpace(grid_coords.clone());
        } else {
            let highlight = if let MoveSelect(piece_pickup_coords, _, _) = state.turn {
                let valid_targets = get_valid_move_targets(&state.board, piece_pickup_coords);

                valid_targets.contains(&grid_coords)
            } else {
                match state.highlighted {
                    PlayerOccupation => {
                        is_occupied_by(&state.board, grid_coords, state.player_stash.colour)
                    }
                    NoHighlighting => false,
                }
            };

            match (button_outcome.draw_state, highlight) {
                (_, true) | (Hover, _) => {
                    card_texture_spec.5 = -0.5;
                    card_texture_spec.7 = -0.5;
                }
                (Pressed, false) => {
                    card_texture_spec.5 = -0.5;
                    card_texture_spec.6 = -0.5;
                }
                _ => {}
            }

            draw_card(p, card_matrix, card_texture_spec);
        }

        for (i, piece) in pieces.into_iter().enumerate() {
            if i < space_offset as _ || i > (space_offset + PIECES_PER_PAGE - 1) as _ {
                continue;
            }

            let (x, y) = card_relative_piece_coords(i);
            let half_piece_scale = piece_scale(piece);

            debug_assert!(i <= 255);
            let piece_id = piece_id(card_id, i as u8);

            let mut piece_texture_spec = piece_texture_spec(piece);

            let on_piece = on_card &&
                point_in_square_on_card(
                    (card_mouse_x, card_mouse_y),
                    (x, y),
                    half_piece_scale,
                    rotated,
                );


            let button_outcome = match state.turn {
                Move | Grow => {
                    button_logic(
                        &mut state.ui_context,
                        ButtonState {
                            id: piece_id,
                            pointer_inside: on_piece,
                            mouse_pressed,
                            mouse_released,
                            mouse_held,
                        },
                    )
                }
                _ => Default::default(),
            };

            if button_outcome.clicked {
                action = SelectPiece(grid_coords.clone(), i);
            } else {
                match button_outcome.draw_state {
                    Pressed => {
                        piece_texture_spec.5 = 1.5;
                        piece_texture_spec.6 = 1.5;
                        piece_texture_spec.7 = 1.5;
                    }
                    Hover => {
                        piece_texture_spec.5 = -1.5;
                        piece_texture_spec.6 = -1.5;
                        piece_texture_spec.7 = -1.5;
                    }
                    Inactive => {}
                }

                draw_piece(p, card_matrix, piece, x, y, piece_texture_spec);
            };
        }

        if pieces.len() > PIECES_PER_PAGE as _ {
            let scale = 0.125;

            let x_offset_amount = 15.0 / 32.0;
            let y_offset_amount = (ARROW_SIZE + 2.5) / (T_S * scale);

            let forward_x = CARD_RATIO - x_offset_amount;
            let forward_y = -(1.0 - y_offset_amount);

            let forward_arrow_matrix = mat4x4_mul(
                &scale_translation(scale, forward_x, forward_y),
                &card_matrix,
            );

            let mut forward_arrow_texture_spec = (
                420.0 / T_S,
                895.0 / T_S,
                ARROW_SIZE / T_S,
                ARROW_SIZE / T_S,
                0,
                0.0,
                0.0,
                0.0,
                0.0,
            );

            let mut backward_arrow_texture_spec = forward_arrow_texture_spec.clone();

            let on_forward = on_card &&
                point_in_square_on_card(
                    (card_mouse_x, card_mouse_y),
                    (forward_x, forward_y),
                    scale,
                    rotated,
                );

            let forward_button_outcome = button_logic(
                &mut state.ui_context,
                ButtonState {
                    id: arrow_id(card_id, true),
                    pointer_inside: on_forward,
                    mouse_pressed,
                    mouse_released,
                    mouse_held,
                },
            );

            if forward_button_outcome.clicked {
                action = PageForward(*grid_coords);
            } else {
                match forward_button_outcome.draw_state {
                    Pressed => {
                        forward_arrow_texture_spec.5 = 1.5;
                        forward_arrow_texture_spec.6 = 1.5;
                    }
                    Hover => {
                        forward_arrow_texture_spec.5 = -1.5;
                        forward_arrow_texture_spec.7 = -1.5;
                    }
                    Inactive => {}
                }
            };

            (p.draw_textured_poly_with_matrix)(
                forward_arrow_matrix,
                SQUARE_POLY_INDEX,
                forward_arrow_texture_spec,
                0,
            );

            if space_offset > 0 {
                let backward_x = -CARD_RATIO + x_offset_amount;
                let backward_y = forward_y;

                let mut backward_camera_matrix = scale_translation(-scale, backward_x, backward_y);

                backward_camera_matrix[5] *= -1.0;

                let backward_arrow_matrix = mat4x4_mul(&backward_camera_matrix, &card_matrix);

                let on_backward = on_card &&
                    point_in_square_on_card(
                        (card_mouse_x, card_mouse_y),
                        (backward_x, backward_y),
                        scale,
                        rotated,
                    );

                let backward_button_outcome = button_logic(
                    &mut state.ui_context,
                    ButtonState {
                        id: arrow_id(card_id, false),
                        pointer_inside: on_backward,
                        mouse_pressed,
                        mouse_released,
                        mouse_held,
                    },
                );

                if backward_button_outcome.clicked {
                    action = PageBack(*grid_coords);

                } else {
                    match backward_button_outcome.draw_state {
                        Pressed => {
                            backward_arrow_texture_spec.5 = 1.5;
                            backward_arrow_texture_spec.6 = 1.5;
                        }
                        Hover => {
                            backward_arrow_texture_spec.5 = -1.5;
                            backward_arrow_texture_spec.7 = -1.5;
                        }
                        Inactive => {}
                    }
                };

                (p.draw_textured_poly_with_matrix)(
                    backward_arrow_matrix,
                    SQUARE_POLY_INDEX,
                    backward_arrow_texture_spec,
                    0,
                );
            }
        }
    }

    match draw_hud(
        p,
        state,
        aspect_ratio,
        (mouse_x, mouse_y),
        mouse_pressed,
        mouse_released,
        mouse_held,
    ) {
        NoAction => {}
        otherwise => {
            action = otherwise;
        }
    };

    match action {
        PageBack(space_coords) => {
            if let Occupied(mut entry) = state.board.entry(space_coords) {
                let mut space = entry.get_mut();
                space.offset = space.offset.saturating_sub(PIECES_PER_PAGE);
            }
        }
        PageForward(space_coords) => {
            if let Occupied(mut entry) = state.board.entry(space_coords) {
                let mut space = entry.get_mut();
                space.offset = space.offset.saturating_add(PIECES_PER_PAGE);
            }
        }
        _ => {}
    }

    let t = state.turn;

    match state.turn {
        DrawInitialCard => {}
        SelectTurnOption => {}
        DrawThree => {}
        Grow => {
            if let SelectPiece(space_coords, piece_index) = action {
                if grow_if_available(
                    space_coords,
                    piece_index,
                    &mut state.board,
                    &mut state.player_stash,
                )
                {
                    //TODO real target turn
                    state.turn = Grow;
                }
            }
        }
        Spawn => {
            if let SelectSpace(key) = action {
                if spawn_if_possible(&mut state.board, &key, &mut state.player_stash) {
                    //TODO real target turn
                    state.turn = Spawn;
                }
            }
        }
        Build => {
            if let SelectCardFromHand(index) = action {
                let valid_target = if let Some(card) = state.player_hand.get(index) {
                    card.is_number()
                } else {
                    false
                };

                if valid_target {
                    state.turn = BuildSelect(state.player_hand.remove(index), index);
                }
            }
        }
        BuildSelect(held_card, old_index) => {
            let build_targets = get_all_build_targets(&state.board, state.player_stash.colour);

            for grid_coords in build_targets.iter() {
                let card_matrix = get_card_matrix(&view, get_card_spec(grid_coords));

                draw_empty_space(p, card_matrix);
            }

            let close_enough_grid_coords =
                get_close_enough_grid_coords(world_mouse_x, world_mouse_y);

            let in_place = close_enough_grid_coords.is_some();

            let card_spec =
                if in_place && build_targets.contains(&close_enough_grid_coords.unwrap()) {
                    get_card_spec(&close_enough_grid_coords.unwrap())
                } else {
                    (world_mouse_x, world_mouse_y, false)
                };

            let card_matrix = get_card_matrix(&view, card_spec);

            let button_outcome = if in_place {
                button_logic(
                    &mut state.ui_context,
                    ButtonState {
                        id: 500,
                        pointer_inside: true,
                        mouse_pressed,
                        mouse_released,
                        mouse_held,
                    },
                )
            } else {
                Default::default()
            };

            draw_card(p, card_matrix, held_card.texture_spec());

            let target_space_coords = if button_outcome.clicked {
                close_enough_grid_coords
            } else {
                None
            };

            if let Some(key) = target_space_coords {
                if build_targets.contains(&key) {
                    state.board.insert(
                        key,
                        Space {
                            card: held_card,
                            ..Default::default()
                        },
                    );
                    //TODO real target turn
                    state.turn = Build;
                }
            } else if right_mouse_pressed || escape_pressed {
                state.player_hand.insert(old_index, held_card);
                //TODO real target turn
                state.turn = Build;
            }
        }
        Move => {
            if let SelectPiece(space_coords, piece_index) = action {
                if let Occupied(mut entry) = state.board.entry(space_coords) {
                    let mut space = entry.get_mut();
                    if let Some(piece) = space.pieces.take_if_present(piece_index) {
                        state.turn = MoveSelect(space_coords, piece_index, piece);
                    }
                }
            }
        }
        MoveSelect(space_coords, piece_index, piece) => {

            draw_piece(
                p,
                view,
                piece,
                world_mouse_x,
                world_mouse_y,
                piece_texture_spec(piece),
            );


            let close_enough_grid_coords =
                get_close_enough_grid_coords(world_mouse_x, world_mouse_y);

            let button_outcome = if close_enough_grid_coords.is_some() {
                button_logic(
                    &mut state.ui_context,
                    ButtonState {
                        id: 500,
                        pointer_inside: true,
                        mouse_pressed,
                        mouse_released,
                        mouse_held,
                    },
                )
            } else {
                Default::default()
            };

            let target_space_coords = if button_outcome.clicked {
                close_enough_grid_coords
            } else {
                None
            };

            if let Some(key) = target_space_coords {
                let valid_targets = get_valid_move_targets(&state.board, space_coords);

                if valid_targets.contains(&key) {

                    if let Occupied(mut entry) = state.board.entry(key) {
                        let mut space = entry.get_mut();

                        //shpuld this be an insert at 0 so the new piece is clearly visible?
                        space.pieces.push(piece);
                    }

                    //TODO real target turn
                    state.turn = Move;
                }
            } else if right_mouse_pressed || escape_pressed {
                if let Occupied(mut entry) = state.board.entry(space_coords) {
                    let mut space = entry.get_mut();

                    space.pieces.insert(piece_index, piece);
                }
                //TODO real target turn
                state.turn = Move;
            }
        }
        ConvertSlashDemolish => {}
        Fly => {
            if let Some(only_ace_index) = get_only_ace_index(&state.player_hand) {
                state.turn =
                    FlySelectCarpet(state.player_hand.remove(only_ace_index), only_ace_index);
            } else if let SelectCardFromHand(index) = action {
                let valid_target = if let Some(card) = state.player_hand.get(index) {
                    card.value == Ace
                } else {
                    false
                };

                if valid_target {
                    state.turn = FlySelectCarpet(state.player_hand.remove(index), index);
                }
            }
        }
        FlySelectCarpet(ace, old_index) => {
            if let SelectSpace(key) = action {
                if let Some(space) = state.board.remove(&key) {
                    state.turn = FlySelect(key, space, ace, old_index);
                }
            } else if right_mouse_pressed || escape_pressed {
                state.player_hand.insert(old_index, ace);
                //TODO real target turn
                state.turn = Fly;
            }
        }
        FlySelect(old_coords, space, ace, old_index) => {
            let Space {
                card,
                pieces,
                offset: space_offset,
            } = space;

            let adjacent_empty_spaces = {
                let mut spaces = get_all_diagonally_connected_empty_spaces(&state.board);
                spaces.remove(&old_coords);
                spaces
            };

            for grid_coords in adjacent_empty_spaces.iter() {
                let card_matrix = get_card_matrix(&view, get_card_spec(grid_coords));

                draw_empty_space(p, card_matrix);
            }

            let close_enough_grid_coords =
                get_close_enough_grid_coords(world_mouse_x, world_mouse_y);

            let in_place = close_enough_grid_coords.is_some();

            let card_spec =
                if in_place && adjacent_empty_spaces.contains(&close_enough_grid_coords.unwrap()) {
                    get_card_spec(&close_enough_grid_coords.unwrap())
                } else {
                    (world_mouse_x, world_mouse_y, false)
                };

            let card_matrix = get_card_matrix(&view, card_spec);

            let button_outcome = if in_place {
                button_logic(
                    &mut state.ui_context,
                    ButtonState {
                        id: 500,
                        pointer_inside: true,
                        mouse_pressed,
                        mouse_released,
                        mouse_held,
                    },
                )
            } else {
                Default::default()
            };

            draw_card(p, card_matrix, card.texture_spec());

            for (i, piece) in pieces.into_iter().enumerate() {
                if i < space_offset as _ || i > (space_offset + PIECES_PER_PAGE) as _ {
                    continue;
                }

                let (x, y) = card_relative_piece_coords(i);

                draw_piece(p, card_matrix, piece, x, y, piece_texture_spec(piece));
            }

            let target_space_coords = if button_outcome.clicked {
                close_enough_grid_coords
            } else {
                None
            };

            if let Some(key) = target_space_coords {
                if adjacent_empty_spaces.contains(&key) {
                    state.board.insert(key, space);
                    //TODO real target turn
                    state.turn = Fly;
                }
            } else if right_mouse_pressed || escape_pressed {
                state.board.insert(old_coords, space);

                state.player_hand.insert(old_index, ace);

                //TODO real target turn
                state.turn = Fly;
            }
        }
        Hatch => {
            if let SelectCardFromHand(index) = action {
                //this assumes no pyramids have been duplicated
                let no_pieces_on_board = state.player_stash.is_full();

                if no_pieces_on_board {

                    let valid_target = if let Some(card) = state.player_hand.get(index) {
                        card.is_number()
                    } else {
                        false
                    };

                    if valid_target {
                        state.turn = HatchSelect(state.player_hand.remove(index), index);
                    }
                }
            }
        }
        HatchSelect(held_card, old_index) => {
            let build_targets = get_all_build_targets(&state.board, state.player_stash.colour);

            for grid_coords in build_targets.iter() {
                let card_matrix = get_card_matrix(&view, get_card_spec(grid_coords));

                draw_empty_space(p, card_matrix);
            }

            let close_enough_grid_coords =
                get_close_enough_grid_coords(world_mouse_x, world_mouse_y);

            let in_place = close_enough_grid_coords.is_some();

            let card_spec =
                if in_place && build_targets.contains(&close_enough_grid_coords.unwrap()) {
                    get_card_spec(&close_enough_grid_coords.unwrap())
                } else {
                    (world_mouse_x, world_mouse_y, false)
                };

            let card_matrix = get_card_matrix(&view, card_spec);

            let button_outcome = if in_place {
                button_logic(
                    &mut state.ui_context,
                    ButtonState {
                        id: 500,
                        pointer_inside: true,
                        mouse_pressed,
                        mouse_released,
                        mouse_held,
                    },
                )
            } else {
                Default::default()
            };

            draw_card(p, card_matrix, held_card.texture_spec());

            let target_space_coords = if button_outcome.clicked {
                close_enough_grid_coords
            } else {
                None
            };

            debug_assert!(state.player_stash.is_full());

            if let Some(key) = target_space_coords {
                if build_targets.contains(&key) {
                    let mut pieces: SpacePieces = Default::default();

                    if let Some(piece) = state.player_stash.remove(Pips::One) {
                        pieces.push(piece);
                    }

                    state.board.insert(
                        key,
                        Space {
                            card: held_card,
                            pieces,

                            ..Default::default()
                        },
                    );
                    //TODO real target turn
                    state.turn = Hatch;
                }
            } else if right_mouse_pressed || escape_pressed {
                state.player_hand.insert(old_index, held_card);
                //TODO real target turn
                state.turn = Hatch;
            }
        }
        CpuTurn => {}
        Over(piece_colour) => {}
    };

    if cfg!(debug_assertions) {
        if t != state.turn {
            println!("{:?}", state.turn);
        }
    }

    match state.turn {
        BuildSelect(_, _) |
        MoveSelect(_, _, _) |
        FlySelectCarpet(_, _) |
        FlySelect(_, _, _, _) |
        HatchSelect(_, _) => {}
        _ => {
            if escape_pressed {
                return true;
            }
        }
    };

    false
}

fn get_only_ace_index(hand: &Vec<Card>) -> Option<usize> {
    let mut result = None;

    for (i, card) in hand.iter().enumerate() {
        if card.value == Ace {
            if result.is_none() {
                result = Some(i);
            } else {
                return None;
            }
        }
    }

    result
}

fn spawn_if_possible(board: &mut Board, key: &(i8, i8), stash: &mut Stash) -> bool {
    if stash[Pips::One] != NoneLeft && is_occupied_by(board, key, stash.colour) {
        if let Some(mut space) = board.get_mut(key) {
            if let Some(piece) = stash.remove(Pips::One) {
                space.pieces.push(piece);
                return true;
            }
        }
    }

    false
}

fn is_occupied_by(board: &Board, grid_coords: &(i8, i8), colour: PieceColour) -> bool {
    if let Some(space) = board.get(grid_coords) {
        space_occupied_by(space, colour)
    } else {
        false
    }
}

fn space_occupied_by(space: &Space, colour: PieceColour) -> bool {
    space.pieces.any(|piece| piece.colour == colour)
}

fn grow_if_available(
    space_coords: (i8, i8),
    piece_index: usize,
    board: &mut Board,
    stash: &mut Stash,
) -> bool {
    if let Occupied(mut entry) = board.entry(space_coords) {
        let mut space = entry.get_mut();
        if let Some(piece) = space.pieces.get_mut_if_present(piece_index) {
            if piece.colour == stash.colour {
                match piece.pips {
                    Pips::One | Pips::Two => {
                        if let Some(larger_piece) = stash.remove(piece.pips.higher()) {

                            let temp = piece.clone();
                            *piece = larger_piece;
                            stash.add(temp);

                            return true;
                        }
                    }
                    Pips::Three => {
                        //TODO indicate illegal move
                    }
                }
            } else {
                //TODO indicate illegal move
            }
        }
    }

    false
}

const PIECES_PER_PAGE: u8 = 4;

fn point_in_square_on_card(
    (point_x, point_y): (f32, f32),
    (box_center_x, box_center_y): (f32, f32),
    square_size: f32,
    rotated: bool,
) -> bool {
    if rotated {
        //swapping x and y and inverting y is equivalent to rotation by 90 degrees
        //This trick appears to only works with a square
        (-point_y - box_center_x).abs() <= square_size &&
            (point_x - box_center_y).abs() <= square_size
    } else {
        (point_x - box_center_x).abs() <= square_size &&
            (point_y - box_center_y).abs() <= square_size
    }
}

const ARROW_SIZE: f32 = 15.0;

fn scale_translation(scale: f32, x_offest: f32, y_offset: f32) -> [f32; 16] {
    [
        scale,
        0.0,
        0.0,
        0.0,
        0.0,
        scale,
        0.0,
        0.0,
        0.0,
        0.0,
        1.0,
        0.0,
        x_offest,
        y_offset,
        0.0,
        1.0,
    ]
}

fn get_close_enough_grid_coords(world_mouse_x: f32, world_mouse_y: f32) -> Option<(i8, i8)> {
    let closest_grid_coords = from_world_coords((world_mouse_x, world_mouse_y));

    let rotated = card_is_rotated(&closest_grid_coords);

    let (center_x, center_y) = to_world_coords(closest_grid_coords);

    let (x_distance, y_distance) = (
        f32::abs(center_x - world_mouse_x),
        f32::abs(center_y - world_mouse_y),
    );

    let in_bounds = if rotated {
        x_distance < CARD_LONG_RADIUS && y_distance < CARD_SHORT_RADIUS
    } else {
        x_distance < CARD_SHORT_RADIUS && y_distance < CARD_LONG_RADIUS
    };

    if in_bounds {
        Some(closest_grid_coords)
    } else {
        None
    }
}

const CARD_LONG_RADIUS: f32 = 1.0;
const CARD_SHORT_RADIUS: f32 = CARD_RATIO;

type CardSpec = (f32, f32, bool);

fn get_card_spec(grid_coords: &(i8, i8)) -> CardSpec {
    let (card_x, card_y) = to_world_coords(*grid_coords);

    let rotated = card_is_rotated(grid_coords);

    (card_x, card_y, rotated)
}

use std::collections::HashSet;

fn get_valid_move_targets(board: &Board, (x, y): (i8, i8)) -> HashSet<(i8, i8)> {
    let mut result = HashSet::new();

    let offsets = [(1, 0), (0, 1), (-1, 0), (0, -1)];

    for &(dx, dy) in offsets.iter() {
        let new_coords = (x.saturating_add(dx), y.saturating_add(dy));
        if board.contains_key(&new_coords) {
            result.insert(new_coords);
        }
    }

    result
}

fn get_all_diagonally_connected_empty_spaces(board: &Board) -> HashSet<(i8, i8)> {
    let filled_coords = board.keys();

    let offsets = [
        (1, 0),
        (1, 1),
        (0, 1),
        (-1, 1),
        (-1, 0),
        (-1, -1),
        (0, -1),
        (1, -1),
    ];

    let mut result = HashSet::new();

    for &(x, y) in filled_coords {
        for &(dx, dy) in offsets.iter() {
            let new_coords = (x.saturating_add(dx), y.saturating_add(dy));
            if !board.contains_key(&new_coords) {
                result.insert(new_coords);
            }
        }
    }

    result
}

fn get_all_build_targets(board: &Board, colour: PieceColour) -> HashSet<(i8, i8)> {
    let mut result = HashSet::new();

    let occupied_spaces = get_all_spaces_occupied_by(board, colour);

    let offsets = [(1, 0), (0, 1), (-1, 0), (0, -1)];

    for &(x, y) in occupied_spaces.iter() {
        for &(dx, dy) in offsets.iter() {
            let new_coords = (x.saturating_add(dx), y.saturating_add(dy));
            if !board.contains_key(&new_coords) {
                result.insert(new_coords);
            }
        }
    }

    result
}

fn get_all_spaces_occupied_by(board: &Board, colour: PieceColour) -> HashSet<(i8, i8)> {
    board
        .iter()
        .filter_map(|(key, space)| if space_occupied_by(space, colour) {
            Some(*key)
        } else {
            None
        })
        .collect()
}

fn card_is_rotated(grid_coords: &(i8, i8)) -> bool {
    (grid_coords.0 + grid_coords.1) % 2 == 0
}

fn card_relative_piece_coords(index: usize) -> (f32, f32) {
    match index % 4 {
        0 => (0.35, 0.5),
        1 => (-0.35, 0.5),
        2 => (-0.35, -0.5),
        3 => (0.35, -0.5),
        _ => (0.0, 0.0),
    }
}

fn draw_piece(
    p: &Platform,
    card_matrix: [f32; 16],
    piece: Piece,
    x: f32,
    y: f32,
    piece_texture_spec: TextureSpec,
) {
    let scale = piece_scale(piece);

    let piece_matrix = scale_translation(scale, x, y);

    (p.draw_textured_poly_with_matrix)(
        mat4x4_mul(&piece_matrix, &card_matrix),
        SQUARE_POLY_INDEX,
        piece_texture_spec,
        0,
    );
}

fn draw_card(p: &Platform, card_matrix: [f32; 16], texture_spec: TextureSpec) {
    (p.draw_textured_poly_with_matrix)(card_matrix, CARD_POLY_INDEX, texture_spec, 0);
}
fn draw_empty_space(p: &Platform, card_matrix: [f32; 16]) {
    (p.draw_textured_poly_with_matrix)(
        card_matrix,
        CARD_POLY_INDEX,
        (
            4.0 * CARD_TEXTURE_WIDTH,
            4.0 * CARD_TEXTURE_HEIGHT,
            CARD_TEXTURE_WIDTH,
            CARD_TEXTURE_HEIGHT,
            0,
            0.0,
            0.0,
            0.0,
            0.0,
        ),
        0,
    );
}

fn get_card_matrix(view: &[f32; 16], (card_x, card_y, rotated): CardSpec) -> [f32; 16] {
    let angle = if rotated {
        std::f32::consts::FRAC_PI_2
    } else {
        0.0
    };

    let world_matrix = [
        f32::cos(angle),
        -f32::sin(angle),
        0.0,
        0.0,
        f32::sin(angle),
        f32::cos(angle),
        0.0,
        0.0,
        0.0,
        0.0,
        1.0,
        0.0,
        card_x,
        card_y,
        0.0,
        1.0,
    ];
    mat4x4_mul(&world_matrix, view)
}

fn draw_hud(
    p: &Platform,
    state: &mut State,
    aspect_ratio: f32,
    (mouse_x, mouse_y): (f32, f32),
    mouse_pressed: bool,
    mouse_released: bool,
    mouse_held: bool,
) -> Action {
    let mut result = NoAction;

    let layer = 1;

    let near = 0.5;
    let far = 1024.0;

    let scale = 8.0;
    let top = scale;
    let bottom = -top;
    let right = aspect_ratio * scale;
    let left = -right;

    let half_height = scale * 2.0;
    let half_width = half_height * aspect_ratio;

    let projection_spec = ProjectionSpec {
        top,
        bottom,
        left,
        right,
        near,
        far,
        projection: Perspective,
        // projection: Orthographic,
    };

    let hud_view = get_projection(&projection_spec);
    let inverse_hud_view = get_projection(&projection_spec.inverse());

    let (hud_mouse_x, hud_mouse_y, _, _) =
        mat4x4_vector_mul_divide(&inverse_hud_view, mouse_x, mouse_y, 0.0, 1.0);

    let card_scale = 3.0;

    let hand_length = state.player_hand.len();

    let mut card_coords = vec![(0.0, 0.0); hand_length];

    let mut selected_index = None;

    for i in (0..hand_length).rev() {
        let (card_x, card_y) = (-half_width * (13.0 - i as f32) / 16.0, -half_height * 0.75);

        card_coords[i] = (card_x, card_y);

        if selected_index.is_none() {
            let (card_mouse_x, card_mouse_y) = (hud_mouse_x - card_x, hud_mouse_y - card_y);

            let on_card = (card_mouse_x).abs() <= CARD_RATIO * card_scale &&
                (card_mouse_y).abs() <= card_scale;

            if on_card {
                selected_index = Some(i);
            }
        }
    }

    for (i, card) in state.player_hand.iter().enumerate() {
        let (card_x, card_y) = card_coords[i];

        let hand_camera_matrix = [
            card_scale,
            0.0,
            0.0,
            0.0,
            0.0,
            card_scale,
            0.0,
            0.0,
            0.0,
            0.0,
            1.0,
            0.0,
            card_x,
            card_y,
            0.0,
            1.0,
        ];

        let card_matrix = mat4x4_mul(&hand_camera_matrix, &hud_view);

        let pointer_inside = match state.turn {
            Build | Hatch => card.is_number() && selected_index == Some(i),
            Fly => card.value == Ace && selected_index == Some(i),
            _ => false,
        };

        let button_outcome = button_logic(
            &mut state.ui_context,
            ButtonState {
                id: (250 + i) as _,
                pointer_inside,
                mouse_pressed,
                mouse_released,
                mouse_held,
            },
        );

        let mut texture_spec = card.texture_spec();

        if button_outcome.clicked {
            result = SelectCardFromHand(i);
        } else {
            match button_outcome.draw_state {
                Pressed => {
                    texture_spec.5 = -0.5;
                    texture_spec.6 = -0.5;
                }
                Hover => {
                    texture_spec.5 = -0.5;
                    texture_spec.7 = -0.5;
                }
                Inactive => {}
            }
        }


        (p.draw_textured_poly_with_matrix)(card_matrix, CARD_POLY_INDEX, texture_spec, layer);
    }

    let (stash_x, stash_y) = (half_width * 0.75, -half_height * 25.0 / 32.0);

    let stash_camera_matrix = [
        3.0,
        0.0,
        0.0,
        0.0,
        0.0,
        3.0,
        0.0,
        0.0,
        0.0,
        0.0,
        1.0,
        0.0,
        stash_x,
        stash_y,
        0.0,
        1.0,
    ];

    let stash_matrix = mat4x4_mul(&stash_camera_matrix, &hud_view);

    draw_stash(p, stash_matrix, &state.player_stash, layer);

    if false {
        let mouse_camera_matrix = [
            1.0,
            0.0,
            0.0,
            0.0,
            0.0,
            1.0,
            0.0,
            0.0,
            0.0,
            0.0,
            1.0,
            0.0,
            mouse_x * (right - left),
            mouse_y * (top - bottom),
            0.0,
            1.0,
        ];

        let mouse_matrix = mat4x4_mul(&mouse_camera_matrix, &hud_view);
        // let mouse_matrix = mouse_camera_matrix;

        let piece_colour = PieceColour::Blue;

        let colour_offset = f32::from(piece_colour);
        let texture_index = i32::from(piece_colour);

        let piece_texture_spec = (
            3.0 * CARD_TEXTURE_WIDTH,
            4.0 * CARD_TEXTURE_HEIGHT +
                (colour_offset * TOOLTIP_TEXTURE_HEIGHT_OFFSET),
            TOOLTIP_TEXTURE_WIDTH,
            TOOLTIP_TEXTURE_HEIGHT,
            0,
            0.0,
            0.0,
            0.0,
            0.0,
        );

        (p.draw_textured_poly_with_matrix)(mouse_matrix, 2, piece_texture_spec, layer);
    }

    (p.draw_layer)(1, state.hud_alpha);

    result
}

fn draw_stash(p: &Platform, matrix: [f32; 16], stash: &Stash, layer: usize) {
    let pips_vec = vec![Pips::Three, Pips::Two, Pips::One];

    for (pile_index, &pips) in pips_vec.iter().enumerate() {
        let poly_index = match pips {
            Pips::One => 3,
            Pips::Two => 4,
            Pips::Three => 5,
        };

        let piece = Piece {
            colour: stash.colour,
            pips,
        };

        let scale = piece_scale(piece) * 2.0;

        for i in 0..u8::from(stash[pips]) {
            //this includes a 90 degree rotation
            let camera_matrix = [
                0.0,
                scale,
                0.0,
                0.0,
                -scale,
                0.0,
                0.0,
                0.0,
                0.0,
                0.0,
                1.0,
                0.0,
                (pile_index as f32 * 1.5) - (0.625 / scale),
                (i as f32 * scale) + (pile_index as f32 * -15.0 / 64.0),
                0.0,
                1.0,
            ];

            let texture_spec = stash_piece_texture_spec(&piece);

            (p.draw_textured_poly_with_matrix)(
                mat4x4_mul(&camera_matrix, &matrix),
                poly_index,
                texture_spec,
                layer,
            );
        }
    }

}

#[derive(Copy, Clone, Debug)]
struct ButtonState {
    id: UiId,
    pointer_inside: bool,
    mouse_pressed: bool,
    mouse_released: bool,
    mouse_held: bool,
}

#[derive(Copy, Clone, Debug)]
struct ButtonOutcome {
    clicked: bool,
    draw_state: DrawState,
}

impl Default for ButtonOutcome {
    fn default() -> Self {
        ButtonOutcome {
            clicked: false,
            draw_state: Inactive,
        }
    }
}

#[derive(Copy, Clone, Debug)]
enum DrawState {
    Pressed,
    Hover,
    Inactive,
}
use DrawState::*;

///This function handles the logic for a given button and returns wheter it was clicked
///and the state of the button so it can be drawn properly elsewhere
fn button_logic(context: &mut UIContext, button_state: ButtonState) -> ButtonOutcome {
    /// In order for this to work properly `context.frame_init();`
    /// must be called at the start of each frame, before this function is called
    let mut clicked = false;

    let inside = button_state.pointer_inside;
    let id = button_state.id;

    if context.active == id {
        if button_state.mouse_released {
            clicked = context.hot == id && inside;

            context.set_not_active();
        }
    } else if context.hot == id {
        if button_state.mouse_pressed {
            context.set_active(id);
        }
    }

    if inside {
        context.set_next_hot(id);
    }

    let draw_state =
        if context.active == id && (button_state.mouse_held || button_state.mouse_pressed) {
            Pressed
        } else if context.hot == id {
            Hover
        } else {
            Inactive
        };

    ButtonOutcome {
        clicked,
        draw_state,
    }
}

//map [0,1] to [-1,1]
fn center(x: f32) -> f32 {
    x * 2.0 - 1.0
}

//TODO: Is there a way to query for this rather than using this guess?
const MOUSE_POINTER_SIZE: f32 = 16.0;

const LARGEST_PIECE_TEXTURE_SIZE: f32 = 65.0 / T_S;

fn piece_scale(piece: Piece) -> f32 {
    piece_size(piece) * 5.0
}
fn piece_size(piece: Piece) -> f32 {
    match piece.pips {
        Pips::One => 33.0 / T_S,
        Pips::Two => 49.0 / T_S,
        Pips::Three => LARGEST_PIECE_TEXTURE_SIZE,
    }
}

fn piece_texture_spec(piece: Piece) -> TextureSpec {
    let size = piece_size(piece);

    let (x, y) = match piece.pips {
        Pips::One => (114.0 / T_S, 792.0 / T_S),
        Pips::Two => (65.0 / T_S, 776.0 / T_S),
        Pips::Three => (0.0, 760.0 / T_S),
    };


    let colour_offset = LARGEST_PIECE_TEXTURE_SIZE * f32::from(piece.colour);

    (
        x,
        y + colour_offset,
        size,
        size,
        i32::from(piece.colour),
        0.0,
        0.0,
        0.0,
        0.0,
    )
}


fn stash_piece_texture_spec(piece: &Piece) -> TextureSpec {
    let (x, y) = match piece.pips {
        Pips::One => (320.0 / T_S, 792.0 / T_S),
        Pips::Two => (246.0 / T_S, 776.0 / T_S),
        Pips::Three => (148.0 / T_S, 760.0 / T_S),
    };
    let (w, h) = match piece.pips {
        Pips::One => (STASH_ONE_PIP_WIDTH / T_S, STASH_ONE_PIP_HEIGHT / T_S),
        Pips::Two => (STASH_TWO_PIP_WIDTH / T_S, STASH_TWO_PIP_HEIGHT / T_S),
        Pips::Three => (STASH_THREE_PIP_WIDTH / T_S, STASH_THREE_PIP_HEIGHT / T_S),
    };

    let colour_offset = LARGEST_PIECE_TEXTURE_SIZE * f32::from(piece.colour);

    (
        x,
        y + colour_offset,
        w,
        h,
        i32::from(piece.colour),
        0.0,
        0.0,
        0.0,
        0.0,
    )
}

fn to_world_coords((grid_x, grid_y): (i8, i8)) -> (f32, f32) {
    (grid_x as f32 * 2.0, grid_y as f32 * 2.0)
}

fn from_world_coords((world_x, world_y): (f32, f32)) -> (i8, i8) {
    ((world_x / 2.0).round() as i8, (world_y / 2.0).round() as i8)
}

fn add_random_board_card(state: &mut State) {
    state.board.insert(
        (state.rng.gen_range(-10, 10), state.rng.gen_range(-10, 10)),
        state.rng.gen(),
    );
}

const CARD_POLY_INDEX: usize = 0;
const SQUARE_POLY_INDEX: usize = 1;

const CARD_RATIO: f32 = CARD_TEXTURE_PIXEL_WIDTH / CARD_TEXTURE_PIXEL_HEIGHT;
const TOOLTIP_RATIO: f32 = TOOLTIP_TEXTURE_PIXEL_WIDTH / TOOLTIP_TEXTURE_PIXEL_HEIGHT;

const STASH_ONE_PIP_WIDTH: f32 = 49.0;
const STASH_ONE_PIP_HEIGHT: f32 = 33.0;
const STASH_TWO_PIP_WIDTH: f32 = 73.0;
const STASH_TWO_PIP_HEIGHT: f32 = 49.0;
const STASH_THREE_PIP_WIDTH: f32 = 97.0;
const STASH_THREE_PIP_HEIGHT: f32 = 65.0;

const STASH_ONE_PIP_RATIO: f32 = STASH_ONE_PIP_WIDTH / STASH_ONE_PIP_HEIGHT;
const STASH_TWO_PIP_RATIO: f32 = STASH_TWO_PIP_WIDTH / STASH_TWO_PIP_HEIGHT;
const STASH_THREE_PIP_RATIO: f32 = STASH_THREE_PIP_WIDTH / STASH_THREE_PIP_HEIGHT;

//These are the verticies of the polygons which can be drawn.
//The index refers to the index of the inner vector within the outer vecton.
#[cfg_attr(rustfmt, rustfmt_skip)]
#[no_mangle]
pub fn get_vert_vecs() -> Vec<Vec<f32>> {
    vec![
        //Card
        vec![
            -CARD_RATIO, 1.0,
            -CARD_RATIO, -1.0,
            CARD_RATIO, -1.0,
            CARD_RATIO, 1.0,
        ],
        //Square
        vec![
            -1.0, 1.0,
            -1.0, -1.0,
            1.0, -1.0,
            1.0, 1.0,
            ],
        //Tooltip
        vec![
            -TOOLTIP_RATIO, 1.0,
            -TOOLTIP_RATIO, -1.0,
            TOOLTIP_RATIO, -1.0,
            TOOLTIP_RATIO, 1.0,
        ],
        //one pip stash
        vec![
            -STASH_ONE_PIP_RATIO, 1.0,
            -STASH_ONE_PIP_RATIO, -1.0,
            STASH_ONE_PIP_RATIO, -1.0,
            STASH_ONE_PIP_RATIO, 1.0,
        ],
        //two pip stash
        vec![
            -STASH_TWO_PIP_RATIO, 1.0,
            -STASH_TWO_PIP_RATIO, -1.0,
            STASH_TWO_PIP_RATIO, -1.0,
            STASH_TWO_PIP_RATIO, 1.0,
        ],
        //three pip stash
        vec![
            -STASH_THREE_PIP_RATIO, 1.0,
            -STASH_THREE_PIP_RATIO, -1.0,
            STASH_THREE_PIP_RATIO, -1.0,
            STASH_THREE_PIP_RATIO, 1.0,
        ],
    ]
}
fn clamp(current: f32, min: f32, max: f32) -> f32 {
    if current > max {
        max
    } else if current < min {
        min
    } else {
        current
    }
}
