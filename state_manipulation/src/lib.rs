extern crate common;
extern crate rand;

use common::*;
use common::Projection::*;
use common::Turn::*;
use common::Value::*;
use common::PiecesLeft::*;
use common::Highlighted::*;
use common::Participant::*;

use std::default::Default;

use rand::{Rng, SeedableRng, StdRng};

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
        if cfg!(debug_assertions) {
            println!("reshuffle");
        }
        if pile.len() == 0 {
            if cfg!(debug_assertions) {
                println!("========= empty pile");
            }

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

    let state = State {
        rng,
        cam_x: 0.0,
        cam_y: 0.0,
        zoom: f32::powi(1.25, 8),
        board: HashMap::new(),
        mouse_pos: (400.0, 300.0),
        window_wh: (INITIAL_WINDOW_WIDTH as _, INITIAL_WINDOW_HEIGHT as _),
        ui_context: UIContext::new(),
        mouse_held: false,
        turn: DrawUntilNumberCard,
        deck,
        pile: Vec::new(),
        player_hand,
        cpu_hands,
        stashes: Stashes {
            player_stash,
            cpu_stashes,
        },
        hud_alpha: 1.0,
        highlighted: PlayerOccupation,
        message: Default::default(),
    };

    state
}

#[derive(Debug)]
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
    //let mut right_mouse_released = false;

    let mut escape_pressed = false;

    for event in events {
        if cfg!(debug_assertions) {
            match *event {
                Event::MouseMove(_) => {}
                _ => println!("{:?}", *event),
            }
        }

        match *event {
            Event::Quit | Event::KeyDown(Keycode::F10) => {
                return true;
            }
            Event::KeyDown(Keycode::Escape) => {
                escape_pressed = true;
            }
            Event::KeyDown(Keycode::Space) => if cfg!(debug_assertions) {
                add_random_board_card(state);
            },
            Event::KeyDown(Keycode::R) => if cfg!(debug_assertions) {
                state.board.clear();
            },
            Event::KeyDown(Keycode::C) => if cfg!(debug_assertions) {
                if let Some(card) = deal(state) {
                    state.player_hand.push(card);
                }
            },
            Event::KeyDown(Keycode::V) => if cfg!(debug_assertions) {
                (p.set_verts)(get_vert_vecs());
            },
            Event::KeyDown(Keycode::M) => if cfg!(debug_assertions) {
                state.message = Message {
                    text: "Test Message".to_owned(),
                    timeout: 1500,
                }
            },
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
                //right_mouse_released = true;
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

    let mouse_button_state = ButtonState {
        pressed: mouse_pressed,
        released: mouse_released,
        held: state.mouse_held,
    };

    state.ui_context.frame_init();

    state.hud_alpha += if state.mouse_pos.1 / state.window_wh.1 > HUD_LINE {
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

        let inverse_camera = inverse_scale_translation(1.0, state.cam_x, state.cam_y);

        let view = mat4x4_mul(&camera, &projection);
        let inverse_view = mat4x4_mul(&inverse_projection, &inverse_camera);

        (view, inverse_view)
    };

    let (world_mouse_x, world_mouse_y, _, _) =
        mat4x4_vector_mul_divide(&inverse_view, mouse_x, mouse_y, 0.0, 1.0);


    let mut action = NoAction;

    for (
        grid_coords,
        &Space {
            card,
            ref pieces,
            offset: space_offset,
        },
    ) in state.board.iter()
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
            Button {
                id: card_id,
                pointer_inside: on_card,
                state: mouse_button_state,
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
                        is_occupied_by(&state.board, grid_coords, state.stashes.player_stash.colour)
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
                Move | Grow | ConvertSlashDemolish => button_logic(
                    &mut state.ui_context,
                    Button {
                        id: piece_id,
                        pointer_inside: on_piece,
                        state: mouse_button_state,
                    },
                ),
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

            if pieces.len() > (space_offset * PIECES_PER_PAGE) as _ {
                let on_forward = on_card &&
                    point_in_square_on_card(
                        (card_mouse_x, card_mouse_y),
                        (forward_x, forward_y),
                        scale,
                        rotated,
                    );

                let forward_button_outcome = button_logic(
                    &mut state.ui_context,
                    Button {
                        id: arrow_id(card_id, true),
                        pointer_inside: on_forward,
                        state: mouse_button_state,
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
            }

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
                    Button {
                        id: arrow_id(card_id, false),
                        pointer_inside: on_backward,
                        state: mouse_button_state,
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

    if state.turn == SelectTurnOption {
        let top_row = [
            (DrawThree, "Draw"),
            (Grow, "Grow"),
            (Spawn, "Spawn"),
            (Build, "Build"),
        ];

        let top_row_len = top_row.len();

        for i in 0..top_row_len {
            let (target_turn, label) = top_row[i];

            if turn_options_button(
                p,
                &mut state.ui_context,
                label,
                (-0.75 + i as f32 * 0.5, 0.875),
                (600 + i) as _,
                (mouse_x, mouse_y),
                mouse_button_state,
            ) {
                state.turn = target_turn;
            };
        }

        let bottom_row = [
            (Move, "Move"),
            (ConvertSlashDemolish, "Convert/Demolish"),
            (Fly, "Fly"),
            (Hatch, "Hatch"),
        ];

        for i in 0..bottom_row.len() {
            let (target_turn, label) = bottom_row[i];

            if turn_options_button(
                p,
                &mut state.ui_context,
                label,
                (-0.75 + i as f32 * 0.5, 11.0 / 16.0),
                (600 + top_row_len + i) as _,
                (mouse_x, mouse_y),
                mouse_button_state,
            ) {
                if target_turn == ConvertSlashDemolish {
                    state.message = Message {
                        text: "Choose a piece to target.".to_owned(),
                        timeout: WARNING_TIMEOUT,
                    };
                } else if target_turn == Fly {
                    state.message = Message {
                        text: "Choose an Ace from your hand to discard.".to_owned(),
                        timeout: WARNING_TIMEOUT,
                    };
                }

                state.turn = target_turn;
            };
        }
    }



    match draw_hud(
        p,
        state,
        aspect_ratio,
        (mouse_x, mouse_y),
        mouse_button_state,
    ) {
        NoAction => {}
        otherwise => {
            action = otherwise;
        }
    };

    match action {
        PageBack(space_coords) => if let Occupied(mut entry) = state.board.entry(space_coords) {
            let mut space = entry.get_mut();
            space.offset = space.offset.saturating_sub(PIECES_PER_PAGE);
        },
        PageForward(space_coords) => if let Occupied(mut entry) = state.board.entry(space_coords) {
            let mut space = entry.get_mut();
            space.offset = space.offset.saturating_add(PIECES_PER_PAGE);
        },
        _ => {}
    }

    if cfg!(debug_assertions) {
        match action {
            NoAction => {}
            _ => {
                println!("{:?}", action);
            }
        }
    }

    let winners = get_winners(&state.board, &state.stashes);
    if let Some(winner) = winners.0 {
        state.turn = if winners.1 == Some(Player) {
            //player gets top billing
            Over(Player, Some(winner))
        } else {
            Over(winner, winners.1)
        };
    }

    let t = state.turn;

    match state.turn {
        DrawUntilNumberCard => {
            if !state.player_hand.iter().any(|c| c.is_number()) {
                state.turn = RevealHand(Player);
            } else {
                for i in 0..state.cpu_hands.len() {
                    let hand = &state.cpu_hands[i];
                    if !hand.iter().any(|c| c.is_number()) {
                        state.turn = RevealHand(Cpu(i));
                        break;
                    }
                }
            }

            if let DrawUntilNumberCard = state.turn {
                state.turn = WhoStarts;
            }
        }
        RevealHand(participant) => {
            {
                let hand = match participant {
                    Player => &state.player_hand,
                    Cpu(i) => &state.cpu_hands[i],
                };

                let card_scale = 1.0;

                let half_hand_len = (hand.len() / 2) as isize;

                for (i, card) in hand.iter().enumerate() {
                    let (card_x, card_y) = ((i as isize - half_hand_len) as f32 / 2.0, 0.0);

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

                    let card_matrix = mat4x4_mul(&hand_camera_matrix, &view);

                    draw_card(p, card_matrix, card.texture_spec());
                }
            }


            //TODO should the CPU players remember the revealed cards?
            if mouse_button_state.pressed {
                if let Some(card) = deal(state) {
                    state.player_hand.push(card);
                }

                for i in 0..state.cpu_hands.len() {
                    if let Some(card) = deal(state) {
                        state.cpu_hands[i].push(card);
                    }
                }

                state.turn = DrawUntilNumberCard;
            }
        }
        WhoStarts => {
            if let SelectCardFromHand(index) = action {
                let valid_target = if let Some(card) = state.player_hand.get(index) {
                    card.is_number()
                } else {
                    false
                };

                if valid_target {
                    let mut cpu_cards_vec = Vec::new();

                    'hands: for hand in state.cpu_hands.iter_mut() {
                        //TODO is there a reason for this not to be the first one?
                        for i in 0..hand.len() {
                            if hand[i].is_number() {
                                cpu_cards_vec.push(hand.remove(i));
                                continue 'hands;
                            }
                        }
                    }

                    debug_assert!(state.cpu_hands.len() == cpu_cards_vec.len());

                    let mut cpu_cards = [None; MAX_PLAYERS - 1];

                    for i in 0..cpu_cards_vec.len() {
                        cpu_cards[i] = Some(cpu_cards_vec[i]);
                    }

                    let player_card = state.player_hand.remove(index);

                    let first = {
                        let mut pairs = vec![(Player, player_card)];

                        for i in 0..cpu_cards_vec.len() {
                            pairs.push((Cpu(i), cpu_cards_vec[i]));
                        }

                        //reverse it so the player wins ties
                        pairs
                            .iter()
                            .rev()
                            .max_by_key(|&&(_, c)| c.value.number_value())
                            .unwrap()
                            .0
                    };

                    let starter_cards = StarterCards {
                        player_card,
                        cpu_cards,
                        first,
                    };
                    state.turn = FirstRound(starter_cards, first);
                }
            }
        }
        FirstRound(starter_cards, mut current_participant) => {
            loop {
                match current_participant {
                    Player => {
                        state.turn = FirstRoundPlayer(starter_cards);
                        break;
                    }
                    Cpu(i) => {
                        let possible_targets = set_to_vec(first_round_targets(&state.board));
                        //TODO should we try to make a power block? or avoid one?

                        if let Some(key) = state.rng.choose(&possible_targets) {
                            let stash = &mut state.stashes.cpu_stashes[i];
                            let space_and_piece_available =
                                stash[Pips::One] != NoneLeft && state.board.get(key).is_none();

                            debug_assert!(space_and_piece_available);

                            if space_and_piece_available {
                                if let Some(card) = starter_cards.cpu_cards[i] {
                                    let mut space = Space::new(card);

                                    if let Some(piece) = stash.remove(Pips::One) {
                                        space.pieces.push(piece);
                                    }
                                    state.board.insert(*key, space);
                                }
                            }
                        }
                    }
                }

                current_participant =
                    next_participant(cpu_player_count(state), current_participant);

                if current_participant == starter_cards.first {
                    state.turn = CpuTurn(Some(current_participant));
                    break;
                }
            }
        }
        FirstRoundPlayer(starter_cards) => {
            let possible_targets = first_round_targets(&state.board);

            let target_space_coords = place_card(
                p,
                state,
                &view,
                &possible_targets,
                starter_cards.player_card,
                (world_mouse_x, world_mouse_y),
                mouse_button_state,
            );

            if let Some(key) = target_space_coords {
                let cpu_player_count = cpu_player_count(state);
                let stash = &mut state.stashes.player_stash;
                let space_and_piece_available =
                    stash[Pips::One] != NoneLeft && state.board.get(&key).is_none();

                debug_assert!(space_and_piece_available);

                if space_and_piece_available {
                    let mut space = Space::new(starter_cards.player_card);

                    if let Some(piece) = stash.remove(Pips::One) {
                        space.pieces.push(piece);
                    }
                    state.board.insert(key, space);
                }


                let next_participant = next_participant(cpu_player_count, Player);
                if next_participant == starter_cards.first {
                    state.turn = CpuTurn(Some(next_participant));
                } else {
                    state.turn = FirstRound(starter_cards, next_participant);
                }
            }
        }
        DrawInitialCard => {
            //TODO drawing sound effect or other indication?
            if let Some(card) = deal(state) {
                state.player_hand.push(card);
            }
            state.turn = SelectTurnOption;
        }
        SelectTurnOption => {
            state.highlighted = NoHighlighting;
            state.message.timeout = 0;
        }
        DrawThree => {
            //TODO drawing sound effect or other indication?
            if let Some(card) = deal(state) {
                state.player_hand.push(card);
            }
            if let Some(card) = deal(state) {
                state.player_hand.push(card);
            }
            if let Some(card) = deal(state) {
                state.player_hand.push(card);
            }
            state.turn = Discard;
        }
        Grow => if let SelectPiece(space_coords, piece_index) = action {
            match grow_if_available(
                space_coords,
                piece_index,
                &mut state.board,
                &mut state.stashes.player_stash,
            ) {
                Ok(true) => {
                    state.turn = Discard;
                }
                Ok(false) => {}
                Err(message) => {
                    state.message = message;
                }
            }
        } else if right_mouse_pressed || escape_pressed {
            state.turn = SelectTurnOption;
        },
        Spawn => {
            state.highlighted = PlayerOccupation;

            if let SelectSpace(key) = action {
                match spawn_if_possible(&mut state.board, &key, &mut state.stashes.player_stash) {
                    Ok(true) => {
                        state.turn = Discard;
                    }
                    Ok(false) => {}
                    Err(message) => {
                        state.message = message;
                    }
                }
            } else if right_mouse_pressed || escape_pressed {
                state.turn = SelectTurnOption;
            }
        }
        Build => {
            state.highlighted = PlayerOccupation;
            if let SelectCardFromHand(index) = action {
                let valid_target = if let Some(card) = state.player_hand.get(index) {
                    card.is_number()
                } else {
                    false
                };

                if valid_target {
                    state.turn = BuildSelect(state.player_hand.remove(index), index);
                }
            } else if right_mouse_pressed || escape_pressed {
                state.turn = SelectTurnOption;
            }
        }
        BuildSelect(held_card, old_index) => {
            let build_targets =
                get_all_build_targets_set(&state.board, state.stashes.player_stash.colour);

            let target_space_coords = place_card(
                p,
                state,
                &view,
                &build_targets,
                held_card,
                (world_mouse_x, world_mouse_y),
                mouse_button_state,
            );

            if let Some(key) = target_space_coords {
                if build_targets.contains(&key) {
                    state.board.insert(
                        key,
                        Space {
                            card: held_card,
                            ..Default::default()
                        },
                    );

                    state.turn = Discard;
                }
            } else if right_mouse_pressed || escape_pressed {
                state.player_hand.insert(old_index, held_card);

                state.turn = SelectTurnOption;
            } else if build_targets.len() == 0 {
                state.message = Message {
                    text: "You can't Build because you don't occupy a space on the edge!"
                        .to_owned(),
                    timeout: WARNING_TIMEOUT,
                };

                state.player_hand.insert(old_index, held_card);

                state.turn = SelectTurnOption;
            }
        }
        Move => if let SelectPiece(space_coords, piece_index) = action {
            if let Occupied(mut entry) = state.board.entry(space_coords) {
                let space = entry.get_mut();
                if let Some(piece_copy) = space.pieces.get(piece_index) {
                    state.turn = MoveSelect(space_coords, piece_index, piece_copy);
                }
            }
        } else if right_mouse_pressed || escape_pressed {
            state.turn = SelectTurnOption;
        },
        MoveSelect(space_coords, piece_index, piece_copy) => {
            draw_piece(
                p,
                view,
                piece_copy,
                world_mouse_x,
                world_mouse_y,
                piece_texture_spec(piece_copy),
            );


            let close_enough_grid_coords =
                get_close_enough_grid_coords(world_mouse_x, world_mouse_y);

            let button_outcome = if close_enough_grid_coords.is_some() {
                button_logic(
                    &mut state.ui_context,
                    Button {
                        id: 500,
                        pointer_inside: true,
                        state: mouse_button_state,
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
                if perform_move(&mut state.board, space_coords, key, piece_index) {
                    state.turn = Discard;
                }
            } else if right_mouse_pressed || escape_pressed {
                state.turn = SelectTurnOption;
            }
        }
        ConvertSlashDemolish => if let SelectPiece(space_coords, piece_index) = action {
            let valid_targets =
                occupied_by_or_adjacent_spaces(&state.board, state.stashes.player_stash.colour);
            if valid_targets.contains(&space_coords) {
                state.turn =
                    ConvertSlashDemolishDiscard(space_coords, piece_index, None, None, None);
            } else {
                let text =
                    "You must target a piece at most one space away from your pieces.".to_owned();
                state.message = Message {
                    text,
                    timeout: WARNING_TIMEOUT,
                };
            }
        } else if right_mouse_pressed || escape_pressed {
            state.turn = SelectTurnOption;
        },
        ConvertSlashDemolishDiscard(
            space_coords,
            piece_index,
            card_index_1,
            card_index_2,
            card_index_3,
        ) => {
            if let Some(space) = state.board.get(&space_coords) {
                if let Some(piece) = space.pieces.get(piece_index) {
                    let pips_needed = u8::from(piece.pips);

                    let hand = &state.player_hand;

                    let (pips_selected, smallest_card_value) = match (
                        card_index_1.and_then(|i| hand.get(i)),
                        card_index_2.and_then(|i| hand.get(i)),
                        card_index_3.and_then(|i| hand.get(i)),
                    ) {
                        (None, None, None) => (0, 0),
                        (Some(&card), None, None) |
                        (None, Some(&card), None) |
                        (None, None, Some(&card)) => {
                            let v = pip_value(&card);

                            (v, v)
                        }
                        (Some(&card1), Some(&card2), None) |
                        (None, Some(&card1), Some(&card2)) |
                        (Some(&card1), None, Some(&card2)) => {
                            let v1 = pip_value(&card1);
                            let v2 = pip_value(&card2);

                            (v1 + v2, std::cmp::min(v1, v2))
                        }
                        (Some(&card1), Some(&card2), Some(&card3)) => {
                            let v1 = pip_value(&card1);
                            let v2 = pip_value(&card2);
                            let v3 = pip_value(&card3);

                            (v1 + v2 + v3, std::cmp::min(v1, std::cmp::min(v2, v3)))
                        }
                    };
                    state.turn = if pips_selected >= pips_needed &&
                        smallest_card_value > pips_selected - pips_needed
                    {
                        state.message.timeout = 0;
                        ConvertSlashDemolishWhich(
                            space_coords,
                            piece_index,
                            card_index_1,
                            card_index_2,
                            card_index_3,
                            pips_selected - pips_needed,
                        )
                    } else {
                        if pips_selected >= pips_needed &&
                            smallest_card_value <= pips_selected - pips_needed
                        {
                            state.message = Message {
                                text: "You cannot discard redundant face cards to get extra draws"
                                    .to_owned(),
                                timeout: WARNING_TIMEOUT,
                            }
                        }
                        if let SelectCardFromHand(index) = action {
                            match (
                                card_index_1.and_then(|i| hand.get(i)),
                                card_index_2.and_then(|i| hand.get(i)),
                                card_index_3.and_then(|i| hand.get(i)),
                            ) {
                                (Some(_), Some(_), Some(_)) => state.turn,
                                (Some(_), Some(_), _) => ConvertSlashDemolishDiscard(
                                    space_coords,
                                    piece_index,
                                    card_index_1,
                                    card_index_2,
                                    Some(index),
                                ),
                                (Some(_), _, _) => ConvertSlashDemolishDiscard(
                                    space_coords,
                                    piece_index,
                                    card_index_1,
                                    Some(index),
                                    None,
                                ),
                                _ => ConvertSlashDemolishDiscard(
                                    space_coords,
                                    piece_index,
                                    Some(index),
                                    None,
                                    None,
                                ),
                            }
                        } else {
                            state.turn
                        }
                    };
                }
            };

            if right_mouse_pressed || escape_pressed {
                state.turn = match state.turn {
                    ConvertSlashDemolishDiscard(_, _, None, None, None) => ConvertSlashDemolish,
                    _ => ConvertSlashDemolishDiscard(space_coords, piece_index, None, None, None),
                };
            } else {
                state.message = Message {
                    text: "Choose which face card(s) to discard from your hand.".to_owned(),
                    timeout: WARNING_TIMEOUT,
                };
            }
        }
        ConvertSlashDemolishWhich(
            space_coords,
            piece_index,
            card_index_1,
            card_index_2,
            card_index_3,
            cards_owed,
        ) => {
            debug_assert!(cards_owed <= 2);
            let hand = &mut state.player_hand;
            let stashes = &mut state.stashes;
            if let Some(space) = state.board.get_mut(&space_coords) {
                let can_convert = if let Some(piece) = space.pieces.get(piece_index) {
                    let stash = &stashes.player_stash;
                    match piece.pips {
                        Pips::One => stash[Pips::One] != NoneLeft,
                        Pips::Two => stash[Pips::One] != NoneLeft || stash[Pips::Two] != NoneLeft,
                        Pips::Three => {
                            stash[Pips::One] != NoneLeft || stash[Pips::Two] != NoneLeft ||
                                stash[Pips::Three] != NoneLeft
                        }
                    }
                } else {
                    false
                };

                if can_convert &&
                    turn_options_button(
                        p,
                        &mut state.ui_context,
                        "Convert",
                        (-0.25, 0.875),
                        700,
                        (mouse_x, mouse_y),
                        mouse_button_state,
                    ) {
                    state.turn = ConvertSelect(
                        space_coords,
                        piece_index,
                        card_index_1,
                        card_index_2,
                        card_index_3,
                        cards_owed,
                    );
                } else if !can_convert ||
                    turn_options_button(
                        p,
                        &mut state.ui_context,
                        "Demolish",
                        (0.25, 0.875),
                        701,
                        (mouse_x, mouse_y),
                        mouse_button_state,
                    ) {
                    if let Some(piece) = space.pieces.remove(piece_index) {
                        stashes[piece.colour].add(piece);
                    }


                    let pile = &mut state.pile;
                    card_index_1.map(|i| pile.push(hand.remove(i)));
                    card_index_2.map(|i| pile.push(hand.remove(i)));
                    card_index_3.map(|i| pile.push(hand.remove(i)));

                    for _ in 0..cards_owed {
                        if let Some(card) = deal_parts(&mut state.deck, pile, &mut state.rng) {
                            hand.push(card);
                        }
                    }

                    state.turn = Discard;
                };
            };

            if right_mouse_pressed || escape_pressed {
                state.turn = ConvertSlashDemolishWhich(
                    space_coords,
                    piece_index,
                    card_index_1,
                    card_index_2,
                    card_index_3,
                    cards_owed,
                );
            }
        }
        ConvertSelect(
            space_coords,
            piece_index,
            card_index_1,
            card_index_2,
            card_index_3,
            cards_owed,
        ) => if let Some(space) = state.board.get_mut(&space_coords) {
            if let Some(piece) = space.pieces.get_mut(piece_index) {
                let stash_colour = state.stashes.player_stash.colour;

                let (has_one, has_two, has_three) = {
                    let stash = &state.stashes[stash_colour];

                    (
                        stash[Pips::One] != NoneLeft,
                        stash[Pips::Two] != NoneLeft,
                        stash[Pips::Three] != NoneLeft,
                    )
                };

                let mut selected_pips = None;

                if has_one && piece.pips == Pips::One {
                    selected_pips = Some(Pips::One);
                }
                if !has_one && has_two && piece.pips == Pips::Two {
                    selected_pips = Some(Pips::Two);
                }
                if !has_one && !has_two && has_three && piece.pips == Pips::Three {
                    selected_pips = Some(Pips::Three);
                }

                if has_one && piece.pips > Pips::One &&
                    turn_options_button(
                        p,
                        &mut state.ui_context,
                        "Small",
                        (-2.0 / 3.0, 0.875),
                        700,
                        (mouse_x, mouse_y),
                        mouse_button_state,
                    ) {
                    selected_pips = Some(Pips::One);
                }
                if has_two && piece.pips >= Pips::Two &&
                    turn_options_button(
                        p,
                        &mut state.ui_context,
                        "Medium",
                        (0.0, 0.875),
                        701,
                        (mouse_x, mouse_y),
                        mouse_button_state,
                    ) {
                    selected_pips = Some(Pips::Two);
                }
                if has_three && piece.pips >= Pips::Three &&
                    turn_options_button(
                        p,
                        &mut state.ui_context,
                        "Large",
                        (2.0 / 3.0, 0.875),
                        702,
                        (mouse_x, mouse_y),
                        mouse_button_state,
                    ) {
                    selected_pips = Some(Pips::Three);
                }

                if let Some(pips) = selected_pips {
                    if pips <= piece.pips {
                        if let Some(stash_piece) = state.stashes[stash_colour].remove(pips) {
                            state.stashes[piece.colour].add(*piece);
                            *piece = stash_piece;

                            let hand = &mut state.player_hand;

                            let pile = &mut state.pile;
                            card_index_1.map(|i| pile.push(hand.remove(i)));
                            card_index_2.map(|i| pile.push(hand.remove(i)));
                            card_index_3.map(|i| pile.push(hand.remove(i)));

                            for _ in 0..cards_owed {
                                if let Some(card) =
                                    deal_parts(&mut state.deck, pile, &mut state.rng)
                                {
                                    hand.push(card);
                                }
                            }

                            state.turn = Discard;
                        }
                    }
                }
            }
        },
        Fly => {
            state.highlighted = PlayerOccupation;
            if let Some(only_ace_index) = get_only_ace_index(&state.player_hand) {
                state.turn =
                    FlySelectCarpet(state.player_hand.remove(only_ace_index), only_ace_index);
            } else if let SelectCardFromHand(index) = action {
                let valid_target = if let Some(card) = state.player_hand.get(index) {
                    if card.value == Ace {
                        true
                    } else {
                        state.message = Message {
                            text: "That card is not an Ace!".to_owned(),
                            timeout: WARNING_TIMEOUT,
                        };

                        false
                    }
                } else {
                    false
                };

                if valid_target {
                    state.turn = FlySelectCarpet(state.player_hand.remove(index), index);
                }
            } else if right_mouse_pressed || escape_pressed {
                state.turn = SelectTurnOption;
            }
        }
        FlySelectCarpet(ace, old_index) => if let SelectSpace(key) = action {
            if is_space_movable(&state.board, &key) {
                if let Some(space) = state.board.remove(&key) {
                    state.turn = FlySelect(key, space, ace, old_index);
                    state.message.timeout = 0;
                }
            } else {
                state.message = Message {
                    text: "Moving that card leaves a section of cards completely detached!"
                        .to_owned(),
                    timeout: WARNING_TIMEOUT,
                }
            }
        } else if right_mouse_pressed || escape_pressed {
            state.player_hand.insert(old_index, ace);

            state.turn = SelectTurnOption;
        } else {
            state.message = Message {
                text: "Choose which card on the board to move.".to_owned(),
                timeout: WARNING_TIMEOUT,
            };
        },
        FlySelect(old_coords, space, ace, old_index) => {
            let Space {
                card,
                pieces,
                offset: space_offset,
            } = space;

            let fly_from_targets = fly_from_targets(&state.board, &old_coords);

            for grid_coords in fly_from_targets.iter() {
                let card_matrix = get_card_matrix(&view, get_card_spec(grid_coords));

                draw_empty_space(p, card_matrix);
            }

            let close_enough_grid_coords =
                get_close_enough_grid_coords(world_mouse_x, world_mouse_y);

            let in_place = close_enough_grid_coords.is_some();

            let card_spec =
                if in_place && fly_from_targets.contains(&close_enough_grid_coords.unwrap()) {
                    get_card_spec(&close_enough_grid_coords.unwrap())
                } else {
                    (world_mouse_x, world_mouse_y, false)
                };

            let card_matrix = get_card_matrix(&view, card_spec);

            let button_outcome = if in_place {
                button_logic(
                    &mut state.ui_context,
                    Button {
                        id: 500,
                        pointer_inside: true,
                        state: mouse_button_state,
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
                if fly_from_targets.contains(&key) {
                    state.board.insert(key, space);

                    state.pile.push(ace);

                    state.turn = Discard;
                }
            } else if right_mouse_pressed || escape_pressed {
                state.board.insert(old_coords, space);

                state.player_hand.insert(old_index, ace);


                state.turn = SelectTurnOption;
            }
        }
        Hatch => {
            if let SelectCardFromHand(index) = action {
                //this assumes no pyramids have been duplicated
                let no_pieces_on_board = state.stashes.player_stash.is_full();

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
            } else if right_mouse_pressed || escape_pressed {
                state.turn = SelectTurnOption;
            }
        }
        HatchSelect(held_card, old_index) => {
            let hatch_targets = get_all_hatch_targets_set(&state.board);

            let target_space_coords = place_card(
                p,
                state,
                &view,
                &hatch_targets,
                held_card,
                (world_mouse_x, world_mouse_y),
                mouse_button_state,
            );

            debug_assert!(state.stashes.player_stash.is_full());

            if let Some(key) = target_space_coords {
                if hatch_targets.contains(&key) {
                    let mut pieces: SpacePieces = Default::default();

                    if let Some(piece) = state.stashes.player_stash.remove(Pips::One) {
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

                    state.turn = Discard;
                }
            } else if right_mouse_pressed || escape_pressed {
                state.player_hand.insert(old_index, held_card);

                state.turn = SelectTurnOption;
            }
        }
        Discard => {
            state.highlighted = NoHighlighting;
            if state.player_hand.len() > 6 {
                if let SelectCardFromHand(index) = action {
                    state.pile.push(state.player_hand.remove(index));
                } else {
                    state.message = Message {
                        text: "Discard down to six cards".to_owned(),
                        timeout: 1000,
                    };
                }
            } else {
                state.turn = CpuTurn(None);
            };
        }

        CpuTurn(set_participant) => {
            let cpu_player_count = cpu_player_count(state);
            let mut current_participant =
                set_participant.unwrap_or(next_participant(cpu_player_count, Player));


            while current_participant != Player {
                if cfg!(debug_assertions) {
                    println!(
                        "current participant: {:?} ({:?})",
                        current_participant,
                        participant_to_colour(&state.stashes, current_participant)
                    );
                }

                let (hand, colour, stashes) = match current_participant {
                    Player => {
                        debug_assert!(false, "Attempting to take player's turn");
                        (
                            &mut state.player_hand,
                            state.stashes.player_stash.colour,
                            &mut state.stashes,
                        )
                    }
                    Cpu(i) => (
                        &mut state.cpu_hands[i],
                        state.stashes.cpu_stashes[i].colour,
                        &mut state.stashes,
                    ),
                };

                let rng = &mut state.rng;

                //DrawInitialCard
                if let Some(card) = deal_parts(&mut state.deck, &mut state.pile, rng) {
                    hand.push(card);
                }

                let mut possible_plan: Option<Plan> =
                    get_plan(&state.board, stashes, &hand, rng, colour);

                'turn: loop {
                    //TODO better "AI". Evaluate every use of rng in this match statement
                    let turn_option = if let Some(plan) = possible_plan {
                        if cfg!(debug_assertions) {
                            println!("plan is {:?}", plan);
                        }
                        match plan {
                            Plan::Fly(_) | Plan::FlySpecific(_, _) => 6,
                            Plan::ConvertSlashDemolish(_, _) => 5,
                            Plan::Move(_) | Plan::MoveSpecific(_, _) => 4,
                            Plan::Build(_) => 3,
                            Plan::Hatch(_) => 7,
                        }
                    } else {
                        let x = if stashes[colour].is_full() {
                            //can only Draw or Hatch
                            *rng.choose(&vec![0, 7]).unwrap_or(&0)
                        } else {
                            //Don't Fly randomly, and you can't Hatch anyways
                            *rng.choose(&vec![0, 1, 2, 3, 4, 5]).unwrap_or(&0)
                        };
                        if cfg!(debug_assertions) {
                            println!("randomly chose {:?}", x);
                        }

                        x
                    };

                    fn choose_piece(
                        board: &Board,
                        key: &(i8, i8),
                        colour: PieceColour,
                    ) -> Option<((i8, i8), usize)> {
                        if let Some(space) = board.get(key) {
                            let own_pieces = space.pieces.filtered_indicies(|p| p.colour == colour);
                            if let Some(piece_index) = own_pieces.last() {
                                return Some((*key, *piece_index));
                            }
                        }

                        None
                    }

                    match turn_option {
                        1 => {
                            //Grow

                            let chosen_piece = {
                                let occupied_spaces: Vec<_> =
                                    get_all_spaces_occupied_by(&state.board, colour);

                                if let Some(space_coords) = rng.choose(&occupied_spaces) {
                                    choose_piece(&state.board, space_coords, colour)
                                } else {
                                    None
                                }
                            };

                            if let Some((space_coords, piece_index)) = chosen_piece {
                                match grow_if_available(
                                    space_coords,
                                    piece_index,
                                    &mut state.board,
                                    &mut stashes[colour],
                                ) {
                                    Ok(true) => {
                                        break 'turn;
                                    }
                                    _ => {}
                                }
                            }
                        }
                        2 => {
                            //Spawn
                            let occupied_spaces: Vec<_> =
                                get_all_spaces_occupied_by(&state.board, colour);

                            if let Some(space_coords) = rng.choose(&occupied_spaces).map(|&i| i) {
                                match spawn_if_possible(
                                    &mut state.board,
                                    &space_coords,
                                    &mut stashes[colour],
                                ) {
                                    Ok(true) => {
                                        break 'turn;
                                    }
                                    _ => {}
                                }
                            }
                        }
                        3 => {
                            //Build
                            let mut number_cards: Vec<_> = hand.iter()
                                .enumerate()
                                .filter(|&(_, c)| c.is_number())
                                .map(|(i, _)| i)
                                .collect();
                            //the cpu's choices should be a function of the rng
                            number_cards.sort();

                            if let Some(&card_index) = rng.choose(&number_cards) {
                                let build_targets = get_all_build_targets(&state.board, colour);
                                let build_target = if let Some(Plan::Build(target)) = possible_plan
                                {
                                    if build_targets.contains(&target) {
                                        Some(target)
                                    } else {
                                        possible_plan = None;
                                        None
                                    }
                                } else {
                                    rng.choose(&build_targets).cloned()
                                };
                                if let Some(key) = build_target {
                                    state.board.insert(
                                        key,
                                        Space {
                                            card: hand.remove(card_index),
                                            ..Default::default()
                                        },
                                    );


                                    break 'turn;
                                }
                            }
                        }
                        4 => {
                            //Move
                            let chosen_piece = match possible_plan {
                                Some(Plan::Move(target)) | Some(Plan::MoveSpecific(target, _)) => {
                                    let adjacent_keys: Vec<_> = {
                                        let mut adjacent_keys : Vec<_> = FOUR_WAY_OFFSETS
                                        .iter()
                                        .map(|&(x, y)| (x + target.0, y + target.1))
                                        .collect();

                                        rng.shuffle(&mut adjacent_keys);

                                        adjacent_keys
                                    };

                                    let mut result = None;

                                    for key in adjacent_keys.iter() {
                                        let chosen_piece = choose_piece(&state.board, key, colour);
                                        if chosen_piece.is_some() {
                                            result = chosen_piece;
                                            break;
                                        }
                                    }

                                    if result.is_some() {
                                        result
                                    } else {
                                        possible_plan = None;
                                        None
                                    }
                                }
                                _ => {
                                    let occupied_spaces: Vec<_> =
                                        get_all_spaces_occupied_by(&state.board, colour);

                                    if let Some(space_coords) =
                                        rng.choose(&occupied_spaces).map(|&i| i)
                                    {
                                        let possible_piece_index =
                                            state.board.get(&space_coords).and_then(|space| {
                                                let own_pieces = space
                                                    .pieces
                                                    .filtered_indicies(|p| p.colour == colour);
                                                rng.choose(&own_pieces).map(|&i| i)
                                            });

                                        possible_piece_index
                                            .map(|piece_index| (space_coords, piece_index))
                                    } else {
                                        None
                                    }
                                }
                            };

                            if let Some((space_coords, piece_index)) = chosen_piece {
                                let possible_target_space_coords =
                                    if let Some(Plan::Move(target)) = possible_plan {
                                        Some(target)
                                    } else {
                                        let valid_targets = set_to_vec(
                                            get_valid_move_targets(&state.board, space_coords),
                                        );

                                        rng.choose(&valid_targets).map(|&i| i)
                                    };

                                if let Some(target_space_coords) = possible_target_space_coords {
                                    if perform_move(
                                        &mut state.board,
                                        space_coords,
                                        target_space_coords,
                                        piece_index,
                                    ) {
                                        break 'turn;
                                    }
                                }
                            }
                        }
                        5 => {
                            //Convert/Demolish
                            let chosen_enemy_pieces = {
                                let spaces: Vec<(i8, i8)> =
                                    occupied_by_or_adjacent_spaces(&state.board, colour);


                                let mut result: Vec<
                                    ((i8, i8), usize),
                                > = vec![];

                                if let Some(Plan::ConvertSlashDemolish(space_coords, piece_index))
                                    = possible_plan {
                                    if let Some(space) = state.board.get(&space_coords) {
                                        if space.pieces.get(piece_index).is_some() {
                                            result = vec![(space_coords, piece_index)];
                                        }
                                    }
                                }

                                if result.len() == 0 {
                                    let mut pieces = Vec::new();

                                    let mut colours = active_colours(stashes);

                                    colours.sort_by_key(|c| stashes[*c].used_count());

                                    colours.retain(|c| *c != colour);

                                    for &target_colour in colours.pop().iter() {
                                        for key in spaces.iter() {
                                            if let Some(space) = state.board.get(key) {
                                                pieces.extend(
                                                    space
                                                        .pieces
                                                        .filtered_indicies(
                                                            |piece| piece.colour == target_colour,
                                                        )
                                                        .iter()
                                                        .filter_map(|&i| {
                                                            space
                                                                .pieces
                                                                .get(i)
                                                                .map(|p| ((*key, i), p.pips))
                                                        }),
                                                );
                                            }
                                        }
                                    }

                                    let stash = &stashes[colour];
                                    let available_sizes = stash.available_sizes_descending();

                                    type Pair = (((i8, i8), usize), Pips);

                                    let (convertable, not_convertable): (Vec<Pair>, Vec<Pair>) =
                                        pieces.iter().partition(|&&(_, pips)|
                                            available_sizes.contains(&pips)
                                    );

                                    result = convertable
                                        .iter()
                                        .chain(not_convertable.iter())
                                        .map(|pair| pair.0)
                                        .collect();
                                }

                                result
                            };

                            for (space_coords, piece_index) in chosen_enemy_pieces {
                                if let Some(space) = state.board.get_mut(&space_coords) {
                                    if let Some(piece) = space.pieces.get(piece_index).clone() {
                                        let pips_needed = u8::from(piece.pips);

                                        let selections = {
                                            //TODO we might want to save particular cards to make a
                                            // power block etc.
                                            let mut card_choices :Vec<_> = hand.iter()
                                                .enumerate()
                                                .filter(|&(_, c)| !c.is_number())
                                                .collect();

                                            card_choices.sort_by(|&(_, a), &(_, b)| {
                                                pip_value(&a).cmp(&pip_value(&b))
                                            });

                                            let mut selected_indicies = Vec::new();
                                            let mut pips_selected = 0;
                                            let mut smallest_card_value = 255;
                                            while let Some((index, card)) = card_choices.pop() {
                                                selected_indicies.push(index);
                                                pips_selected += pip_value(card);
                                                smallest_card_value = std::cmp::min(
                                                    smallest_card_value,
                                                    pip_value(card),
                                                );

                                                if pips_selected >= pips_needed {
                                                    break;
                                                }
                                            }

                                            if pips_selected >= pips_needed &&
                                                smallest_card_value > pips_selected - pips_needed
                                            {
                                                let can_convert = match piece.pips {
                                                    Pips::One => {
                                                        stashes[colour][Pips::One] != NoneLeft
                                                    }
                                                    Pips::Two => {
                                                        stashes[colour][Pips::One] != NoneLeft ||
                                                            stashes[colour][Pips::Two] != NoneLeft
                                                    }
                                                    Pips::Three => {
                                                        stashes[colour][Pips::One] != NoneLeft ||
                                                            stashes[colour][Pips::Two] !=
                                                                NoneLeft ||
                                                            stashes[colour][Pips::Three] != NoneLeft
                                                    }
                                                };

                                                Some((
                                                    selected_indicies,
                                                    can_convert,
                                                    pips_selected - pips_needed,
                                                ))
                                            } else {
                                                possible_plan = None;
                                                None
                                            }
                                        };

                                        //TODO You very rarely might want to demolish instead of
                                        //convert to save a piece in your stash for a future turn

                                        match selections {
                                            Some((selected_indicies, true, cards_owed)) => {
                                                //Convert
                                                for &pips in stashes[colour]
                                                    .available_sizes_descending()
                                                    .iter()
                                                {
                                                    if pips <= piece.pips {
                                                        if let Some(stash_piece) =
                                                            stashes[colour].remove(pips)
                                                        //culprit
                                                        {
                                                            if let Some(old_piece) =
                                                                space.pieces.get_mut(piece_index)
                                                            {
                                                                stashes[old_piece.colour]
                                                                    .add(*old_piece);

                                                                *old_piece = stash_piece;

                                                                for i in selected_indicies {
                                                                    state.pile.push(hand.remove(i));
                                                                }

                                                                for _ in 0..cards_owed {
                                                                    if let Some(card) = deal_parts(
                                                                        &mut state.deck,
                                                                        &mut state.pile,
                                                                        rng,
                                                                    ) {
                                                                        hand.push(card);
                                                                    }
                                                                }

                                                                if cfg!(debug_assertions) {
                                                                    print!("Convert:");

                                                                    println!(
                                                                        "{:?} to {:?} at {:?}",
                                                                        piece.colour,
                                                                        colour,
                                                                        space_coords
                                                                    );
                                                                }

                                                                break 'turn;
                                                            }
                                                        }
                                                    }
                                                }
                                            }

                                            Some((selected_indicies, false, cards_owed)) => {
                                                //Demolish
                                                if let Some(piece) =
                                                    space.pieces.remove(piece_index)
                                                {
                                                    stashes[piece.colour].add(piece);
                                                }

                                                for i in selected_indicies {
                                                    state.pile.push(hand.remove(i));
                                                }

                                                for _ in 0..cards_owed {
                                                    if let Some(card) = deal_parts(
                                                        &mut state.deck,
                                                        &mut state.pile,
                                                        rng,
                                                    ) {
                                                        hand.push(card);
                                                    }
                                                }

                                                if cfg!(debug_assertions) {
                                                    println!(
                                                        "Demolish:{:?} piece at {:?}",
                                                        piece.colour,
                                                        space_coords
                                                    );
                                                }

                                                break 'turn;
                                            }
                                            _ => {}
                                        };
                                    }
                                };
                            }
                        }
                        6 => {
                            //Fly
                            let ace_indicies: Vec<_> = hand.iter()
                                .enumerate()
                                .filter(|&(_, c)| c.value == Ace)
                                .map(|(i, _)| i)
                                .collect();

                            if ace_indicies.len() == 0 {
                                possible_plan = None;
                                continue 'turn;
                            }

                            let board = &mut state.board;
                            let chosen_space = {
                                match possible_plan {
                                    Some(Plan::Fly(source)) |
                                    Some(Plan::FlySpecific(source, _)) => {
                                        if board.contains_key(&source) {
                                            board.remove(&source).map(|space| (source, space))
                                        } else {
                                            debug_assert!(false, "Bad Fly/FlySpecific Plan!");
                                            possible_plan = None;
                                            None
                                        }
                                    }
                                    _ => {
                                        let spaces = get_all_spaces_occupied_by(board, colour);

                                        rng.choose(&spaces).and_then(
                                            |key| if is_space_movable(board, key) {
                                                board.remove(key).map(|space| (*key, space))
                                            } else {
                                                None
                                            },
                                        )
                                    }
                                }
                            };

                            if let Some((old_coords, space)) = chosen_space {
                                let target_space_coords: Option<
                                    (i8, i8),
                                > = if let Some(Plan::FlySpecific(_, target)) = possible_plan {
                                    if let None = board.get(&target) {
                                        Some(target)
                                    } else {
                                        debug_assert!(false, "Bad Plan::FlySpecific!");
                                        None
                                    }
                                } else {
                                    let mut fly_from_targets =
                                        fly_from_targets(&board, &old_coords);
                                    if let Some(Plan::Fly(_)) = possible_plan {
                                        //TODO check if there is a place that helps the cpu player
                                        fly_from_targets.sort_by_key(|key| {
                                            let adjacent_keys: Vec<_> = {
                                                let mut adjacent_keys: Vec<_> = FOUR_WAY_OFFSETS
                                                    .iter()
                                                    .map(|&(x, y)| (x + key.0, y + key.1))
                                                    .collect();

                                                rng.shuffle(&mut adjacent_keys);

                                                adjacent_keys
                                            };

                                            adjacent_keys
                                                .iter()
                                                .filter(|key| board.get(key).is_some())
                                                .count()
                                        });

                                        fly_from_targets.pop()
                                    } else {
                                        rng.choose(&fly_from_targets).cloned()
                                    }
                                };

                                if let Some(key) = target_space_coords {
                                    if let Some(ace_index) = rng.choose(&ace_indicies) {
                                        state.pile.push(hand.remove(*ace_index));

                                        board.insert(key, space);

                                        break 'turn;
                                    }
                                }
                            }
                        }
                        7 => {
                            //Hatch

                            let stash = &mut stashes[colour];

                            let no_pieces_on_board = stash.is_full();
                            if no_pieces_on_board {
                                let mut number_cards: Vec<_> = hand.iter()
                                    .enumerate()
                                    .filter(|&(_, c)| c.is_number())
                                    .map(|(i, _)| i)
                                    .collect();

                                number_cards.sort();
                                rng.shuffle(&mut number_cards);

                                if number_cards.len() != 0 {
                                    let board = &mut state.board;
                                    let possible_decision = if let Some(Plan::Hatch(target)) =
                                        possible_plan
                                    {
                                        rng.choose(&number_cards)
                                            .map(|&i| i)
                                            .and_then(|i| Some((target, i)))
                                    } else {
                                        let mut hatch_targets: Vec<
                                                (i8, i8),
                                            > = get_all_hatch_targets(board)
                                                .iter()
                                                .cloned()
                                                .collect();

                                        hatch_targets.sort_by_key(|key| {
                                            let adjacent_keys: Vec<_> = FOUR_WAY_OFFSETS
                                                    .iter()
                                                    .map(|&(x, y)| (x + key.0, y + key.1))
                                                    .collect();


                                            other_colour_occupation_count(
                                                board,
                                                &adjacent_keys,
                                                colour,
                                            )
                                        });


                                        if let Some(default_space_coord) =
                                            hatch_targets.last().map(|&key| key)
                                        {
                                            let mut possible_decision = None;

                                            //TODO hatching far away from other cards is useful
                                            //if you can make a power block and everyone else is
                                            //far enough away. This might not happen in practice.

                                            let combined_power_block_count =
                                                combined_power_blocks(&board).iter().count();

                                            while let Some(key) = hatch_targets.pop() {
                                                //TODO We assume (for now) that when hatching we
                                                //will want to avoid making a power block. Making
                                                //a new one as a distraction *may* be useful some
                                                //of the time.
                                                let mut number_cards_copy = number_cards.clone();
                                                let mut board_copy = board.clone();

                                                //completable_power_blocks
                                                number_cards_copy.retain(
                                                    |i| if let Some(card) = hand.get(*i) {
                                                        board_copy.insert(
                                                            key,
                                                            Space {
                                                                card: *card,
                                                                ..Default::default()
                                                            },
                                                        );

                                                        let new_combined_power_block_count =
                                                            combined_power_blocks(&board_copy)
                                                                .iter()
                                                                .count();

                                                        new_combined_power_block_count <=
                                                            combined_power_block_count
                                                    } else {
                                                        false
                                                    },
                                                );

                                                if let Some(card_index) = number_cards_copy.pop() {
                                                    possible_decision = Some((key, card_index));
                                                    break;
                                                }
                                            }

                                            possible_decision.or_else(|| {
                                                if cfg!(debug_assertions) {
                                                    println!("Had to make block with Hatch");
                                                }
                                                rng.choose(&number_cards)
                                                    .map(|&i| (default_space_coord, i))
                                            })
                                        } else {
                                            None
                                        }
                                    };


                                    if let Some((key, card_index)) = possible_decision {
                                        let mut pieces: SpacePieces = Default::default();

                                        if let Some(piece) = stash.remove(Pips::One) {
                                            pieces.push(piece);
                                        }

                                        board.insert(
                                            key,
                                            Space {
                                                card: hand.remove(card_index),
                                                pieces,

                                                ..Default::default()
                                            },
                                        );

                                        if cfg!(debug_assertions) {
                                            println!("Hatch at {:?}", key);
                                        }

                                        break 'turn;
                                    }
                                }
                            }
                        }

                        _ => {
                            //DrawThree
                            if let Some(card) = deal_parts(&mut state.deck, &mut state.pile, rng) {
                                hand.push(card);
                            }
                            if let Some(card) = deal_parts(&mut state.deck, &mut state.pile, rng) {
                                hand.push(card);
                            }
                            if let Some(card) = deal_parts(&mut state.deck, &mut state.pile, rng) {
                                hand.push(card);
                            }

                            break 'turn;
                        }
                    }
                }

                //Discard
                if hand.len() > 6 {
                    //TODO track what power blocks this cpu player is going for and
                    //don't discard the pieces
                    hand.sort_by_key(|card| {
                        match card.value {
                            Two | Ten => 100,
                            Jack => 80,
                            //TODO are theer cases where this
                            //hueristic is clearly wrong?
                            Queen => 70,
                            Ace => 60,
                            King => 0,
                            a if a.is_number() => 90,
                            _ => {
                                debug_assert!(false, "unkown card type found during Cpu discard!");
                                0
                            }
                        }
                    });

                    while hand.len() > 6 {
                        state.pile.push(hand.pop().unwrap());
                    }
                }

                current_participant = next_participant(cpu_player_count, current_participant);
            }

            state.turn = DrawInitialCard;
        }
        Over(winner, possible_second_winner) => {
            fn win_message(participant: Participant) -> String {
                match participant {
                    Player => "You win".to_owned(),
                    Cpu(i) => format!("Cpu {} wins", i),
                }
            }
            let text = format!("{}!", win_message(winner));

            draw_outlined_text(
                p,
                &text,
                (0.0, 0.875),
                1.0,
                36.0,
                [1.0, 1.0, 1.0, 1.0],
                [0.0, 0.0, 0.0, 1.0],
            );

            if let Some(second_winner) = possible_second_winner {
                draw_outlined_text(
                    p,
                    &format!("{} as well!", win_message(second_winner)),
                    (0.0, 0.75),
                    1.0,
                    36.0,
                    [1.0, 1.0, 1.0, 1.0],
                    [0.0, 0.0, 0.0, 1.0],
                );
            }
        }
    };

    //draw the hud here so the layering is correct
    (p.draw_layer)(1, state.hud_alpha);

    //draw the message over top of everything
    if state.message.timeout > 0 {
        let message_location = (0.0, -((HUD_LINE * 2.0) - 1.0) + 0.125);

        let alpha = if state.message.timeout > 512 {
            1.0
        } else {
            state.message.timeout as f32 / 512.0
        };

        let outline_colour = [0.0, 0.0, 0.0, alpha];
        let boosted_alpha = if alpha * 2.0 >= 1.0 { 1.0 } else { alpha * 2.0 };
        let middle_alpha = if boosted_alpha >= alpha {
            boosted_alpha
        } else {
            alpha
        };
        let middle_colour = [1.0, 1.0, 1.0, middle_alpha];

        draw_outlined_text(
            p,
            &state.message.text,
            message_location,
            1.0,
            24.0,
            middle_colour,
            outline_colour,
        );

        state.message.timeout = state.message.timeout.saturating_sub(16);
    }

    if cfg!(debug_assertions) {
        if t != state.turn {
            println!("{:?}", state.turn);
        }
    }

    match state.turn {
        DrawUntilNumberCard |
        FirstRound(_, _) |
        DrawInitialCard |
        SelectTurnOption |
        Discard |
        CpuTurn(_) |
        Over(_, _) => if escape_pressed {
            return true;
        },
        _ => {}
    };

    if cfg!(debug_assertions) {
        fn diff<T: Copy + PartialEq>(a: &Vec<T>, b: &Vec<T>) -> Vec<T> {
            let mut diff = Vec::new();

            let (shorter, longer) = if a.len() > b.len() {
                (&b, &a)
            } else {
                (&a, &b)
            };

            for i in 0..longer.len() {
                match (shorter.get(i), longer.get(i)) {
                    (Some(c1), Some(c2)) if c1 == c2 => {}
                    (Some(_), Some(c2)) => {
                        diff.push(*c2);
                    }
                    (Some(c), None) | (None, Some(c)) => {
                        diff.push(*c);
                    }
                    (None, None) => {}
                }
            }

            diff
        }

        ////////////////////
        //    card check  //
        ////////////////////

        let mut all_cards = Vec::new();

        all_cards.extend(state.player_hand.iter().cloned());

        for i in 0..state.cpu_hands.len() {
            let hand = &state.cpu_hands[i];
            assert!(hand.len() <= 6, "Cpu({}) has {} cards!", i, hand.len());

            all_cards.extend(hand.iter().cloned());
        }

        all_cards.extend(state.deck.iter().cloned());
        all_cards.extend(state.pile.iter().cloned());

        let mut over_okay = false;

        match state.turn {
            FirstRound(starter_cards, _) | FirstRoundPlayer(starter_cards) => {
                all_cards.push(starter_cards.player_card.clone());

                for possible_card in starter_cards.cpu_cards.iter() {
                    if let Some(card) = *possible_card {
                        all_cards.push(card);
                    }
                }

                over_okay = true;
            }
            BuildSelect(card, _) | FlySelectCarpet(card, _) | HatchSelect(card, _) => {
                all_cards.push(card);
            }
            FlySelect(_, space, card, _) => {
                all_cards.push(space.card);
                all_cards.push(card);
            }
            _ => {}
        }

        for space in state.board.values() {
            all_cards.push(space.card.clone());
        }

        all_cards.sort();

        let fresh_deck = Card::all_values();

        if over_okay {
            assert!(all_cards.len() >= fresh_deck.len());
        } else {
            assert_eq!(
                all_cards,
                fresh_deck,
                "
                all_cards.len():{:?}
                fresh_deck.len():{:?}
                diff :{:?}",
                all_cards.len(),
                fresh_deck.len(),
                diff(&all_cards, &fresh_deck)
            );
        }

        ////////////////////
        //   piece check  //
        ////////////////////

        let colours = active_colours(&state.stashes);

        let mut all_pieces = Vec::new();

        for &colour in colours.iter() {
            let stash = &state.stashes[colour];

            for pips in Pips::all_values() {
                match stash[pips] {
                    NoneLeft => {}
                    OneLeft => {
                        all_pieces.push(Piece { colour, pips });
                    }
                    TwoLeft => {
                        all_pieces.push(Piece { colour, pips });
                        all_pieces.push(Piece { colour, pips });
                    }
                    ThreeLeft => {
                        all_pieces.push(Piece { colour, pips });
                        all_pieces.push(Piece { colour, pips });
                        all_pieces.push(Piece { colour, pips });
                    }
                }
            }
        }


        for space in state.board.values() {
            for piece in space.pieces.clone().into_iter() {
                all_pieces.push(piece);
            }
        }

        match state.turn {
            //MoveSelect uses a copy so we don't count it.
            FlySelect(_, space, _, _) => for piece in space.pieces.clone().into_iter() {
                all_pieces.push(piece);
            },
            _ => {}
        }

        all_pieces.sort();

        let mut one_of_each = Piece::all_values();

        one_of_each.retain(|p| colours.contains(&p.colour));

        let mut expected = Vec::new();

        for piece in one_of_each {
            expected.push(piece);
            expected.push(piece);
            expected.push(piece);
        }

        assert_eq!(
            all_pieces,
            expected,
            "
            all_pieces.len():{:?}
            expected.len():{:?}
            diff :{:?}",
            all_pieces.len(),
            expected.len(),
            diff(&all_pieces, &expected)
        );
    }


    false
}

fn occupied_by_or_adjacent_spaces(board: &Board, colour: PieceColour) -> Vec<(i8, i8)> {
    let mut set = get_all_spaces_occupied_by_set(board, colour);

    let initial_keys: Vec<(i8, i8)> = set.iter().cloned().collect();

    for key in initial_keys {
        set.extend(
            FOUR_WAY_OFFSETS
                .iter()
                .map(|&(x, y)| (x + key.0, y + key.1)),
        );
    }

    set_to_vec(set)
}

fn perform_move(board: &mut Board, from: (i8, i8), to: (i8, i8), piece_index: usize) -> bool {
    let valid_targets = get_valid_move_targets(&board, from);

    if !valid_targets.contains(&to) {
        return false;
    }

    let possible_piece = if let Occupied(mut source_entry) = board.entry(from) {
        let mut source_space = source_entry.get_mut();
        source_space.pieces.remove(piece_index)
    } else {
        None
    };

    if let Some(piece) = possible_piece {
        if let Occupied(mut target_entry) = board.entry(to) {
            let mut target_space = target_entry.get_mut();

            //shpuld this be an insert at 0
            //so the new piece is clearly visible?
            target_space.pieces.push(piece);

            return true;
        }
    }

    false
}

fn get_winners(board: &Board, stashes: &Stashes) -> (Option<Participant>, Option<Participant>) {
    let power_blocks = get_power_blocks(board);

    // There is a way to have two winners: A player can move
    // a piece from a contesed area to another whihc gives them a power block,
    //while also leaving the contested block to another player.
    //AFAICT that is the maximum possble though.
    let mut winners = (None, None);

    for block in power_blocks {
        if let Some(controller) = single_controller(stashes, board, block) {
            winners = match winners {
                (None, None) => (Some(controller), None),
                (Some(winner), _) | (_, Some(winner)) if winner == controller => winners,
                (Some(_), Some(second_winner)) => {
                    debug_assert!(second_winner == controller, "Three winners?!");
                    winners
                }
                (Some(winner), None) => (Some(winner), Some(controller)),
                (None, Some(mistake)) => (None, Some(mistake)),
            };
        }
    }

    winners
}

//a cheesy way to do an outline
fn draw_outlined_text(
    p: &Platform,
    text: &str,
    location: (f32, f32),
    screen_width_percentage: f32,
    scale: f32,
    middle_colour: [f32; 4],
    outline_colour: [f32; 4],
) {
    //let's just lineraly extrapolate from what worked before!
    let outline_scale = scale * 24.5 / 24.0;
    let outline_offset = 1.0 / (512.0 * 24.0);

    (p.draw_text)(
        text,
        (location.0 + outline_offset, location.1 - outline_offset),
        screen_width_percentage,
        outline_scale,
        outline_colour,
        0,
    );
    (p.draw_text)(
        text,
        (location.0 - outline_offset, location.1 + outline_offset),
        screen_width_percentage,
        outline_scale,
        outline_colour,
        0,
    );
    (p.draw_text)(
        text,
        (location.0 - outline_offset, location.1 - outline_offset),
        screen_width_percentage,
        outline_scale,
        outline_colour,
        0,
    );
    (p.draw_text)(
        text,
        (location.0 + outline_offset, location.1 + outline_offset),
        screen_width_percentage,
        outline_scale,
        outline_colour,
        0,
    );


    (p.draw_text)(
        text,
        location,
        screen_width_percentage,
        scale,
        middle_colour,
        0,
    );
}

fn other_colour_occupation_count(
    board: &Board,
    keys: &Vec<(i8, i8)>,
    colour: PieceColour,
) -> usize {
    keys.iter()
        .filter(|key| {
            board
                .get(key)
                .and_then(|s| controller_colour(&s.pieces).map(|c| c != colour))
                .unwrap_or(false)
        })
        .count()
}

const HUD_LINE: f32 = 0.675;
const WARNING_TIMEOUT: u32 = 2500;

fn place_card(
    p: &Platform,
    state: &mut State,
    view: &[f32; 16],
    targets: &HashSet<(i8, i8)>,
    card: Card,
    (world_mouse_x, world_mouse_y): (f32, f32),
    button_state: ButtonState,
) -> Option<(i8, i8)> {
    for grid_coords in targets.iter() {
        let card_matrix = get_card_matrix(&view, get_card_spec(grid_coords));

        draw_empty_space(p, card_matrix);
    }

    let close_enough_grid_coords = get_close_enough_grid_coords(world_mouse_x, world_mouse_y);

    let in_place = close_enough_grid_coords.is_some();

    let card_spec = if in_place && targets.contains(&close_enough_grid_coords.unwrap()) {
        get_card_spec(&close_enough_grid_coords.unwrap())
    } else {
        (world_mouse_x, world_mouse_y, false)
    };

    let card_matrix = get_card_matrix(&view, card_spec);

    let button_outcome = if in_place {
        button_logic(
            &mut state.ui_context,
            Button {
                id: 500,
                pointer_inside: true,
                state: button_state,
            },
        )
    } else {
        Default::default()
    };

    draw_card(p, card_matrix, card.texture_spec());

    if button_outcome.clicked {
        close_enough_grid_coords
    } else {
        None
    }
}

fn cpu_player_count(state: &State) -> usize {
    cpu_player_count_stashes(&state.stashes)
}
fn cpu_player_count_stashes(stashes: &Stashes) -> usize {
    stashes.cpu_stashes.len()
}

fn next_participant(cpu_player_count: usize, participant: Participant) -> Participant {
    match participant {
        Player => Cpu(0),
        Cpu(i) => if i >= cpu_player_count - 1 {
            Player
        } else {
            Cpu(i + 1)
        },
    }
}

fn pip_value(card: &Card) -> u8 {
    match card.value {
        Ace | Jack /* | Joker*/ => 1,
        Queen => 2,
        King => 3,
        //this value ensures the set of cards will be rejected
        _ => 0,
    }
}

fn turn_options_button(
    p: &Platform,
    context: &mut UIContext,
    label: &str,
    (x, y): (f32, f32),
    id: UiId,
    (mouse_x, mouse_y): (f32, f32),
    state: ButtonState,
) -> bool {
    let mut texture_spec = (
        3.0 * CARD_TEXTURE_WIDTH,
        4.0 * CARD_TEXTURE_HEIGHT + (4.0 * TOOLTIP_TEXTURE_HEIGHT_OFFSET),
        TOOLTIP_TEXTURE_WIDTH,
        TOOLTIP_TEXTURE_HEIGHT,
        0,
        0.0,
        0.0,
        0.0,
        0.0,
    );

    let camera = scale_translation(0.0625, x, y);

    let inverse_camera = inverse_scale_translation(0.0625, x, y);

    let (box_mouse_x, box_mouse_y, _, _) =
        mat4x4_vector_mul(&inverse_camera, mouse_x, mouse_y, 0.0, 1.0);

    let pointer_inside = box_mouse_x.abs() <= TOOLTIP_RATIO && box_mouse_y.abs() <= 1.0;

    let button_outcome = button_logic(
        context,
        Button {
            id,
            pointer_inside,
            state,
        },
    );

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

    (p.draw_textured_poly_with_matrix)(camera, 2, texture_spec, 0);

    let font_scale = if label.len() > 8 { 18.0 } else { 24.0 };

    (p.draw_text)(label, (x, y), 1.0, font_scale, [1.0; 4], 0);

    button_outcome.clicked
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

fn spawn_if_possible(
    board: &mut Board,
    key: &(i8, i8),
    stash: &mut Stash,
) -> Result<bool, Message> {
    if stash[Pips::One] == NoneLeft {
        return Err(Message {
            text: "You are out of small pyramids!".to_owned(),
            timeout: WARNING_TIMEOUT,
        });
    }

    if is_occupied_by(board, key, stash.colour) {
        if let Some(mut space) = board.get_mut(key) {
            if let Some(piece) = stash.remove(Pips::One) {
                space.pieces.push(piece);
                return Ok(true);
            }
        }
    }

    Ok(false)
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
) -> Result<bool, Message> {
    if let Occupied(mut entry) = board.entry(space_coords) {
        let mut space = entry.get_mut();
        if let Some(piece) = space.pieces.get_mut(piece_index) {
            if piece.colour == stash.colour {
                match piece.pips {
                    Pips::One | Pips::Two => {
                        if let Some(larger_piece) = stash.remove(piece.pips.higher()) {
                            let temp = piece.clone();
                            *piece = larger_piece;
                            stash.add(temp);

                            return Ok(true);
                        }
                    }
                    Pips::Three => {
                        return Err(Message {
                            text: "That pyramid is already maximum size!".to_owned(),
                            timeout: WARNING_TIMEOUT,
                        });
                    }
                }
            } else {
                return Err(Message {
                    text: "You cannot grow another player's piece!".to_owned(),
                    timeout: WARNING_TIMEOUT,
                });
            }
        }
    }

    Ok(false)
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

fn inverse_scale_translation(scale: f32, x_offest: f32, y_offset: f32) -> [f32; 16] {
    scale_translation(1.0 / scale, -x_offest / scale, -y_offset / scale)
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

    for &(dx, dy) in FOUR_WAY_OFFSETS.iter() {
        let new_coords = (x.saturating_add(dx), y.saturating_add(dy));
        if board.contains_key(&new_coords) {
            result.insert(new_coords);
        }
    }

    result
}

fn get_all_build_targets(board: &Board, colour: PieceColour) -> Vec<(i8, i8)> {
    set_to_vec(get_all_build_targets_set(board, colour))
}
fn get_all_build_targets_set(board: &Board, colour: PieceColour) -> HashSet<(i8, i8)> {
    let mut result = HashSet::new();

    let occupied_spaces = get_all_spaces_occupied_by_set(board, colour);

    for &(x, y) in occupied_spaces.iter() {
        for &(dx, dy) in FOUR_WAY_OFFSETS.iter() {
            let new_coords = (x.saturating_add(dx), y.saturating_add(dy));
            if !board.contains_key(&new_coords) {
                result.insert(new_coords);
            }
        }
    }

    result
}

fn get_all_hatch_targets(board: &Board) -> Vec<(i8, i8)> {
    set_to_vec(get_all_hatch_targets_set(board))
}
fn get_all_hatch_targets_set(board: &Board) -> HashSet<(i8, i8)> {
    let mut result = HashSet::new();

    for &(x, y) in board.keys() {
        for &(dx, dy) in EIGHT_WAY_OFFSETS.iter() {
            let new_coords = (x.saturating_add(dx), y.saturating_add(dy));
            if !board.contains_key(&new_coords) {
                result.insert(new_coords);
            }
        }
    }

    result
}

fn get_all_spaces_occupied_by(board: &Board, colour: PieceColour) -> Vec<(i8, i8)> {
    set_to_vec(get_all_spaces_occupied_by_set(board, colour))
}

fn set_to_vec(set: HashSet<(i8, i8)>) -> Vec<(i8, i8)> {
    let mut result: Vec<_> = set.iter().cloned().collect();

    //we want the chosen results to be a function only our rng, not the HashSet's internal one
    result.sort();

    result
}

fn get_all_spaces_occupied_by_set(board: &Board, colour: PieceColour) -> HashSet<(i8, i8)> {
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

fn participant_to_colour(stashes: &Stashes, participant: Participant) -> Option<PieceColour> {
    match participant {
        Player => Some(stashes.player_stash.colour),
        Cpu(i) => stashes.cpu_stashes.get(i).map(|s| s.colour),
    }
}

fn colour_to_participant(stashes: &Stashes, colour: PieceColour) -> Option<Participant> {
    if stashes.player_stash.colour == colour {
        Some(Player)
    } else {
        for i in 0..stashes.cpu_stashes.len() {
            if stashes.cpu_stashes[i].colour == colour {
                return Some(Cpu(i));
            }
        }

        None
    }
}

fn single_controller(stashes: &Stashes, board: &Board, block: Block) -> Option<Participant> {
    single_controller_colour(board, block).and_then(|c| colour_to_participant(stashes, c))
}

fn single_controller_colour(board: &Board, block: Block) -> Option<PieceColour> {
    if let Some(pieces_array) = block_to_pieces(board, block) {
        let mut colour_iter = pieces_array.iter().map(controller_colour);

        let possible_colours: [Option<PieceColour>; 3] = [
            colour_iter.next().and_then(id),
            colour_iter.next().and_then(id),
            colour_iter.next().and_then(id),
        ];

        match (
            possible_colours[0],
            possible_colours[1],
            possible_colours[2],
        ) {
            (Some(p1), Some(p2), Some(p3)) if p1 == p2 && p2 == p3 => Some(p1),
            _ => None,
        }
    } else {
        None
    }
}

fn controller_colour(pieces: &SpacePieces) -> Option<PieceColour> {
    let mut possible_colour = None;

    for piece in pieces.into_iter() {
        if let Some(colour) = possible_colour {
            if colour != piece.colour {
                return None;
            }
        } else {
            possible_colour = Some(piece.colour);
        }
    }

    possible_colour
}

#[derive(Clone, Copy, Debug)]
enum Block {
    Horizontal(i8, (i8, i8, i8)),
    Vertical(i8, (i8, i8, i8)),
    UpRight(i8, i8),
    UpLeft(i8, i8),
    DownLeft(i8, i8),
    DownRight(i8, i8),
}
use Block::*;

fn get_power_blocks(board: &Board) -> Vec<Block> {
    let mut result = Vec::new();

    for &(x, y) in board.keys() {
        let horizontal = Horizontal(y, (x - 1, x, x + 1));

        if is_power_block(board, horizontal) {
            result.push(horizontal);
        }

        let vertical = Vertical(x, (y - 1, y, y + 1));

        if is_power_block(board, vertical) {
            result.push(vertical);
        }

        let up_right = UpRight(x, y);

        if is_power_block(board, up_right) {
            result.push(up_right);
        }

        let up_left = UpLeft(x, y);

        if is_power_block(board, up_left) {
            result.push(up_left);
        }

        let down_right = DownRight(x, y);

        if is_power_block(board, down_right) {
            result.push(down_right);
        }

        let down_left = DownLeft(x, y);

        if is_power_block(board, down_left) {
            result.push(down_left);
        }
    }

    result
}

#[derive(Clone, Copy, Debug)]
struct CompletableBlock {
    keys: [(i8, i8); 2],
    completion: Completion,
    completion_key: (i8, i8),
}

#[derive(Clone, Copy, Debug)]
enum Completion {
    Unique(Suit, Value),
    ValueMatch(Value),
    SuitedEnds(Suit, (Value, Value)),
}
use Completion::*;

fn completable_power_block(board: &Board, block: Block) -> Option<CompletableBlock> {
    if let Some((keys, mut cards, completion_key)) = block_to_two_cards_and_a_blank(board, block) {
        cards.sort();

        match (cards[0], cards[1]) {
            (Card { value: v1, .. }, Card { value: v2, .. })
                if v1.is_number() && v2.is_number() && v1 == v2 =>
            {
                Some(CompletableBlock {
                    keys,
                    completion: ValueMatch(v1),
                    completion_key,
                })
            }
            (
                Card {
                    value: v1,
                    suit: s1,
                },
                Card {
                    value: v2,
                    suit: s2,
                },
            ) if v1.is_number() && v2.is_number() && s1 == s2 =>
            {
                let v1_f32 = f32::from(v1);
                let v2_f32 = f32::from(v2);
                if v1_f32 + 1.0 == v2_f32 {
                    match (v1.lower_number(), v2.higher_number()) {
                        (Some(low), Some(high)) => Some(CompletableBlock {
                            keys,
                            completion: SuitedEnds(s1, (low, high)),
                            completion_key,
                        }),
                        (Some(v), _) | (_, Some(v)) => Some(CompletableBlock {
                            keys,
                            completion: Unique(s1, v),
                            completion_key,
                        }),
                        (None, None) => None,
                    }
                } else if v1_f32 + 2.0 == v2_f32 {
                    v1.higher_number().map(|v| {
                        CompletableBlock {
                            keys,
                            completion: Unique(s1, v),
                            completion_key,
                        }
                    })
                } else {
                    None
                }
            }
            _ => None,
        }
    } else {
        None
    }
}


fn block_to_two_cards_and_a_blank(
    board: &Board,
    block: Block,
) -> Option<([(i8, i8); 2], [Card; 2], (i8, i8))> {
    let coords = block_to_coords(block);
    let add_card = |key: (i8, i8)| (key, board.get(&key).map(|s| s.card));

    match (
        add_card(coords[0]),
        add_card(coords[1]),
        add_card(coords[2]),
    ) {
        ((k1, Some(c1)), (k2, Some(c2)), (blank, None)) |
        ((k1, Some(c1)), (blank, None), (k2, Some(c2))) |
        ((blank, None), (k1, Some(c1)), (k2, Some(c2))) => Some(([k1, k2], [c1, c2], blank)),
        _ => None,
    }
}

fn completable_power_blocks(board: &Board) -> Vec<CompletableBlock> {
    let mut result = Vec::new();

    let plain_keys: Vec<_> = board.keys().cloned().collect();

    let mut keys: Vec<(i8, i8)> = Vec::new();

    for key in plain_keys {
        for &(x, y) in FOUR_WAY_OFFSETS.iter() {
            keys.push((x + key.0, y + key.1));
        }
    }

    keys.sort();
    keys.dedup();

    for (x, y) in keys {
        let horizontal = Horizontal(y, (x - 1, x, x + 1));

        if let Some(completable) = completable_power_block(board, horizontal) {
            result.push(completable);
        }

        let vertical = Vertical(x, (y - 1, y, y + 1));

        if let Some(completable) = completable_power_block(board, vertical) {
            result.push(completable);
        }

        let up_right = UpRight(x, y);

        if let Some(completable) = completable_power_block(board, up_right) {
            result.push(completable);
        }

        let up_left = UpLeft(x, y);

        if let Some(completable) = completable_power_block(board, up_left) {
            result.push(completable);
        }

        let down_right = DownRight(x, y);

        if let Some(completable) = completable_power_block(board, down_right) {
            result.push(completable);
        }

        let down_left = DownLeft(x, y);

        if let Some(completable) = completable_power_block(board, down_left) {
            result.push(completable);
        }
    }

    result
}

#[derive(Clone, Copy, Debug)]
enum CombinedBlock {
    Completable(CompletableBlock),
    Complete(Block),
}
use CombinedBlock::*;
fn combined_power_blocks(board: &Board) -> Vec<CombinedBlock> {
    get_power_blocks(board)
        .iter()
        .cloned()
        .map(Complete)
        .chain(
            completable_power_blocks(board)
                .iter()
                .cloned()
                .map(Completable),
        )
        .collect()
}

fn block_to_coords(block: Block) -> [(i8, i8); 3] {
    match block {
        Horizontal(y, (x_minus_1, x, x_plus_1)) => [(x_minus_1, y), (x, y), (x_plus_1, y)],
        Vertical(x, (y_minus_1, y, y_plus_1)) => [(x, y_minus_1), (x, y), (x, y_plus_1)],
        UpRight(x, y) => [(x, y + 1), (x, y), (x + 1, y)],
        UpLeft(x, y) => [(x, y + 1), (x, y), (x - 1, y)],
        DownLeft(x, y) => [(x, y - 1), (x, y), (x - 1, y)],
        DownRight(x, y) => [(x, y - 1), (x, y), (x + 1, y)],
    }
}

/// The identity function.
fn id<T>(x: T) -> T {
    x
}

fn block_to_cards(board: &Board, block: Block) -> Option<[Card; 3]> {
    let coords = block_to_coords(block);
    let mut cards_iter = coords.iter().map(|key| board.get(key).map(|s| s.card));
    let possible_cards: [Option<Card>; 3] = [
        cards_iter.next().and_then(id),
        cards_iter.next().and_then(id),
        cards_iter.next().and_then(id),
    ];

    match (possible_cards[0], possible_cards[1], possible_cards[2]) {
        (Some(c1), Some(c2), Some(c3)) => Some([c1, c2, c3]),
        _ => None,
    }
}

fn block_to_pieces(board: &Board, block: Block) -> Option<[SpacePieces; 3]> {
    let coords = block_to_coords(block);
    let mut coords_iter = coords
        .iter()
        .map(|key| board.get(key).map(|s| (*s).pieces.clone()));

    let possible_pieces: [Option<SpacePieces>; 3] = [
        coords_iter.next().and_then(id),
        coords_iter.next().and_then(id),
        coords_iter.next().and_then(id),
    ];

    match (possible_pieces[0], possible_pieces[1], possible_pieces[2]) {
        (Some(p1), Some(p2), Some(p3)) => Some([p1, p2, p3]),
        _ => None,
    }
}

fn is_power_block(board: &Board, block: Block) -> bool {
    if let Some(mut cards) = block_to_cards(board, block) {
        cards.sort();

        match (cards[0], cards[1], cards[2]) {
            (Card { value: v1, .. }, Card { value: v2, .. }, Card { value: v3, .. })
                if v1.is_number() && v2.is_number() && v3.is_number() && v1 == v2 && v2 == v3 =>
            {
                true
            }
            (
                Card {
                    value: v1,
                    suit: s1,
                },
                Card {
                    value: v2,
                    suit: s2,
                },
                Card {
                    value: v3,
                    suit: s3,
                },
            ) if v1.is_number() && v2.is_number() && v3.is_number() && s1 == s2 && s2 == s3 =>
            {
                f32::from(v1) + 1.0 == f32::from(v2) && f32::from(v2) + 1.0 == f32::from(v3)
            }
            _ => false,
        }
    } else {
        false
    }
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
    mouse_button_state: ButtonState,
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
            Build | Hatch | WhoStarts => card.is_number() && selected_index == Some(i),
            ConvertSlashDemolishDiscard(_, _, _, _, _) => {
                !card.is_number() && selected_index == Some(i)
            }
            Fly => card.value == Ace && selected_index == Some(i),
            Discard => selected_index == Some(i),
            _ => false,
        };

        let button_outcome = button_logic(
            &mut state.ui_context,
            Button {
                id: (250 + i) as _,
                pointer_inside,
                state: mouse_button_state,
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

    draw_stash(p, stash_matrix, &state.stashes.player_stash, layer);

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
struct Button {
    id: UiId,
    pointer_inside: bool,
    state: ButtonState,
}

#[derive(Copy, Clone, Debug)]
struct ButtonState {
    pressed: bool,
    released: bool,
    held: bool,
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
///and the state of the button so it can be drawn properly elsestate of the button so
///it can be drawn properly elsewhere
fn button_logic(context: &mut UIContext, button: Button) -> ButtonOutcome {
    /// In order for this to work properly `context.frame_init();`
    /// must be called at the start of each frame, before this function is called
    let mut clicked = false;

    let inside = button.pointer_inside;
    let id = button.id;

    if context.active == id {
        if button.state.released {
            clicked = context.hot == id && inside;

            context.set_not_active();
        }
    } else if context.hot == id {
        if button.state.pressed {
            context.set_active(id);
        }
    }

    if inside {
        context.set_next_hot(id);
    }

    let draw_state = if context.active == id && (button.state.held || button.state.pressed) {
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

fn first_round_targets(board: &Board) -> HashSet<(i8, i8)> {
    let mut result = HashSet::new();

    if board.len() == 0 {
        result.insert((0, 0));
    } else {
        let full_spaces = board.keys();

        for &(x, y) in full_spaces {
            for &(dx, dy) in FOUR_WAY_OFFSETS.iter() {
                let new_coords = (x.saturating_add(dx), y.saturating_add(dy));
                if !board.contains_key(&new_coords) {
                    result.insert(new_coords);
                }
            }
        }
    }

    result
}

fn active_colours(stashes: &Stashes) -> Vec<PieceColour> {
    let mut colours = Vec::new();

    colours.push(stashes.player_stash.colour);

    for stash in stashes.cpu_stashes.iter() {
        colours.push(stash.colour);
    }

    colours
}
fn other_active_colours(stashes: &Stashes, colour: PieceColour) -> Vec<PieceColour> {
    let mut colours = Vec::new();

    if colour != stashes.player_stash.colour {
        colours.push(stashes.player_stash.colour);
    }

    for stash in stashes.cpu_stashes.iter() {
        if colour != stash.colour {
            colours.push(stash.colour);
        }
    }

    colours
}

fn get_winning_plan(
    board: &Board,
    stashes: &Stashes,
    hand: &Vec<Card>,
    colour: PieceColour,
) -> Option<Plan> {
    //Since the board can have a maximum of 40 entries I'm assuming avaoiding copies isn't
    //worth it, until I see until evidence to the contrary

    let participant = {
        let possible_particpant = colour_to_participant(stashes, colour);

        if let Some(participant) = possible_particpant {
            participant
        } else {
            return None;
        }
    };

    let occupied_spaces = get_all_spaces_occupied_by(board, colour);

    let has_ace = hand.iter().filter(|c| c.value == Ace).count() > 0;

    if has_ace {
        for key in occupied_spaces.iter() {
            let fly_from_targets = fly_from_targets(board, key);

            for target in fly_from_targets {
                let mut board_copy = board.clone();

                if let Some(space) = board_copy.remove(key) {
                    board_copy.insert(target, space);

                    if winners_contains(get_winners(&board_copy, stashes), participant) {
                        return Some(Plan::FlySpecific(*key, target));
                    }
                }
            }
        }
    }

    for key in occupied_spaces.iter() {
        let move_targets = get_valid_move_targets(board, *key);

        for target in move_targets {
            let mut board_copy = board.clone();

            if let Some(piece_index) = board_copy.get(key).and_then(|space| {
                space
                    .pieces
                    .filtered_indicies(|p| p.colour == colour)
                    .last()
                    .map(|&i| i)
            }) {
                perform_move(&mut board_copy, *key, target, piece_index);

                if winners_contains(get_winners(&board_copy, stashes), participant) {
                    return Some(Plan::MoveSpecific(*key, target));
                }
            }
        }
    }

    None
}

fn winners_contains(
    winners: (Option<Participant>, Option<Participant>),
    participant: Participant,
) -> bool {
    match winners {
        (Some(p), _) | (_, Some(p)) if p == participant => true,
        _ => false,
    }
}

#[derive(Copy, Clone, Debug)]
enum Plan {
    Fly((i8, i8)),
    FlySpecific((i8, i8), (i8, i8)),
    ConvertSlashDemolish((i8, i8), usize),
    Move((i8, i8)),
    MoveSpecific((i8, i8), (i8, i8)),
    Build((i8, i8)),
    Hatch((i8, i8)),
}


/// Get all coords in power blocks that contain the given coord.s
/// Overlaps result in duplicates.
fn get_all_power_block_coords(power_blocks: &Vec<Block>, coord: (i8, i8)) -> Vec<(i8, i8)> {
    let mut result = Vec::new();

    for block in power_blocks.iter() {
        let block_coords = block_to_coords(*block);
        if block_coords.contains(&coord) {
            result.extend(block_coords.iter());
        }
    }

    result
}

//sort by occurence (highest occurance first) than deduo
fn overlap_priority_sort_and_dedup(coords: &mut Vec<(i8, i8)>) {
    let mut counts = HashMap::new();


    for coord in coords.iter() {
        if !counts.contains_key(coord) {
            counts.insert(*coord, coords.iter().filter(|&c| c == coord).count());
        }
    }

    coords.sort_by_key(|coord| {
        <usize>::max_value() - counts.get(coord).unwrap_or(&0)
    });
    coords.dedup();
}

fn get_fly_specific(
    board: &Board,
    power_blocks: &Vec<Block>,
    empty_disruption_targets: &Vec<(i8, i8)>,
    adjacent_keys: &Vec<(i8, i8)>,
    occupied_spaces: &Vec<(i8, i8)>,
    target: &(i8, i8),
) -> Option<Plan> {
    //We filter out these spaces so the cpu player doesn't
    //waste an Ace flying somehere that doesn't change the situation
    //TODO check this works on overlapping power blocks
    let undesired_coords: HashSet<(i8, i8)> = power_blocks
        .iter()
        .map(|block| block_to_coords(*block))
        .filter(|coords| coords.iter().any(|key| key == target))
        .flat_map(|coords| coords.to_vec().into_iter())
        .collect();

    let filtered_occupied_spaces = occupied_spaces
        .iter()
        .filter(|coord| !undesired_coords.contains(coord));

    let mut adjacent_empty_spaces: Vec<_> = adjacent_keys
        .iter()
        .filter(|key| board.get(key).is_none())
        .collect();

    //2 for 1 if possible
    adjacent_empty_spaces.sort_by_key(|key| {
        if empty_disruption_targets.contains(key) {
            //keep at the front
            0u8
        } else {
            //move to the end
            255u8
        }
    });

    //Find suggested place to fly from
    for source_coord in filtered_occupied_spaces {
        let possible_targets = fly_from_targets(board, source_coord);
        for target_coord in adjacent_empty_spaces.iter() {
            if possible_targets.contains(target_coord) {
                if flight_does_not_create_power_block(&board, *source_coord, **target_coord) {
                    return Some(Plan::FlySpecific(*source_coord, **target_coord));
                }
            }
        }
    }

    None
}

fn flight_does_not_create_power_block(board: &Board, source: (i8, i8), target: (i8, i8)) -> bool {
    if board.contains_key(&source) {
        let power_block_count = get_power_blocks(board).len();
        let mut board_copy = board.clone();
        if let Some(space) = board_copy.remove(&source) {
            board_copy.insert(target, space);

            get_power_blocks(&board_copy).len() <= power_block_count
        } else {
            true
        }
    } else {
        true
    }
}


fn get_plan(
    board: &Board,
    stashes: &Stashes,
    hand: &Vec<Card>,
    rng: &mut StdRng,
    colour: PieceColour,
) -> Option<Plan> {
    {
        let winning_plan = get_winning_plan(board, stashes, hand, colour);
        if winning_plan.is_some() {
            if cfg!(debug_assertions) {
                println!("winning plan!");
            }
            return winning_plan;
        }
    }
    if cfg!(debug_assertions) {
        println!("No winning plan");
    }


    let other_winning_plans: Vec<Plan> = {
        let other_colours = other_active_colours(stashes, colour);
        let mut other_winning_plans = Vec::new();

        let board_cards: Vec<Card> = board.values().map(|s| s.card.clone()).collect();

        let mut full_hand = Card::all_values();
        full_hand.retain(|c| !(hand.contains(c) || board_cards.contains(c)));

        for current_colour in other_colours {
            if let Some(plan) = get_winning_plan(board, stashes, &full_hand, current_colour) {
                if let Some(participant) = colour_to_participant(stashes, current_colour) {
                    other_winning_plans.push((participant, plan));
                }
            }
        }
        //Prevent whoever will win next fromk winning
        let order_keys = {
            let mut order_keys = HashMap::new();
            if let Some(participant) = colour_to_participant(stashes, colour) {
                order_keys.insert(participant, 0);

                let mut counter: u8 = 1;
                let mut current_participant = participant;
                loop {
                    current_participant =
                        next_participant(cpu_player_count_stashes(stashes), current_participant);

                    order_keys.insert(current_participant, counter);

                    counter += 1;

                    if current_participant == participant || counter == 255 {
                        break;
                    }
                }
            }

            order_keys
        };

        other_winning_plans.sort_by_key(|&(participant, _)| {
            order_keys.get(&participant).map(|&k| k).unwrap_or(255u8)
        });

        other_winning_plans.iter().map(|&(_, plan)| plan).collect()
    };

    let power_blocks = get_power_blocks(board);

    let mut completable_power_blocks = completable_power_blocks(board);

    completable_power_blocks.retain(|completable| {
        let has_card_to_complete: bool = match completable.completion {
            Unique(suit, value) => hand.contains(&Card { suit, value }),
            ValueMatch(v) => hand.iter().any(|c| v == c.value),
            SuitedEnds(suit, (v1, v2)) => {
                hand.contains(&Card { suit, value: v1 }) || hand.contains(&Card { suit, value: v2 })
            }
        };

        //If we have a card to complete the block, (probably), no one else can complete it
        !has_card_to_complete
    });

    if cfg!(debug_assertions) {
        if other_winning_plans.len() > 0 {
            println!(
                "Found {} other player winning plan(s)",
                other_winning_plans.len()
            );
        }
    }

    let (disruption_targets, empty_disruption_targets) = if other_winning_plans.len() > 0 {
        //limit disruption to winning prevention
        let mut disruption_targets = Vec::new();
        let mut empty_disruption_targets = Vec::new();


        for plan in other_winning_plans {
            match plan {
                Plan::Fly(from) => {
                    disruption_targets.push(from);
                }
                Plan::FlySpecific(from, to) => {
                    let mut board_copy = board.clone();

                    if let Some(moved) = board_copy.remove(&from) {
                        board_copy.insert(to, moved);

                        let mut power_block_coords =
                            get_all_power_block_coords(&get_power_blocks(&board_copy), to);
                        power_block_coords.retain(|k| *k != to);

                        disruption_targets.append(&mut power_block_coords);
                    }
                    empty_disruption_targets.push(to);
                }
                Plan::ConvertSlashDemolish(to, _) => {
                    disruption_targets.append(&mut get_all_power_block_coords(&power_blocks, to));
                }
                Plan::Move(_from) => {
                    //no point in disrupting where they were
                    //TODO is this ever emitted as a winning plan?
                }
                Plan::MoveSpecific(_from, to) => {
                    disruption_targets.append(&mut get_all_power_block_coords(&power_blocks, to));
                }
                Plan::Build(to) => {
                    empty_disruption_targets.push(to);
                }
                Plan::Hatch(to) => {
                    empty_disruption_targets.push(to);
                }
            }
        }

        overlap_priority_sort_and_dedup(&mut disruption_targets);

        overlap_priority_sort_and_dedup(&mut empty_disruption_targets);

        (disruption_targets, empty_disruption_targets)
    } else {
        let mut power_block_targets: Vec<(i8, i8)> = power_blocks
            .iter()
            .cloned()
            .flat_map(|block| block_to_coords(block).to_vec().into_iter())
            .collect();

        let other_colour_occupation_count_sorter = |key: &(i8, i8)| {
            let mut power_block_keys = Vec::new();

            for block in power_blocks.iter() {
                let coords = block_to_coords(*block);
                if coords.contains(key) {
                    power_block_keys.extend(coords.iter());
                }
            }

            for block in completable_power_blocks.iter() {
                if block.keys.contains(key) {
                    power_block_keys.extend(block.keys.iter());
                }
            }

            //how many of those spaces are occupied by other colours
            let count = other_colour_occupation_count(board, &power_block_keys, colour);


            //put the highest count keys in front
            <usize>::max_value() - count
        };

        overlap_priority_sort_and_dedup(&mut power_block_targets);
        power_block_targets.sort_by_key(&other_colour_occupation_count_sorter);

        let mut completable_power_block_targets: Vec<(i8, i8)> = completable_power_blocks
            .iter()
            .flat_map(|completable| completable.keys.iter().cloned())
            .collect();

        overlap_priority_sort_and_dedup(&mut completable_power_block_targets);
        completable_power_block_targets.sort_by_key(other_colour_occupation_count_sorter);

        let disruption_targets: Vec<(i8, i8)> = power_block_targets
            .into_iter()
            .chain(completable_power_block_targets)
            .collect();

        let empty_disruption_targets: Vec<(i8, i8)> = completable_power_blocks
            .iter()
            .filter(|completable| {
                let mut contoller_colours = completable.keys.iter().filter_map(|key| {
                    board
                        .get(key)
                        .and_then(|space| controller_colour(&space.pieces))
                });

                contoller_colours.all(|c| c != colour)
            })
            .map(|completable| completable.completion_key)
            .collect();

        (disruption_targets, empty_disruption_targets)
    };

    if cfg!(debug_assertions) {
        println!(
            "disruption_targets {:?}, empty_disruption_targets: {:?}",
            disruption_targets,
            empty_disruption_targets
        );
    }

    //TODO more comprhensive other player winning prevention check here
    //  * see if other players have a winning move
    //  * if so, look for a move that prevents it

    let mut occupied_spaces = get_all_spaces_occupied_by(board, colour);

    //2 for 1 if possible
    occupied_spaces.sort_by_key(|key| {
        if disruption_targets.contains(key) {
            //keep at the front
            0u8
        } else {
            //move to the end
            255u8
        }
    });


    let has_ace = hand.iter().filter(|c| c.value == Ace).count() > 0;
    println!("hand : {:?}, has_ace {}", hand, has_ace);
    let has_number_card = hand.iter().filter(|c| c.is_number()).count() > 0;

    let build_targets = get_all_build_targets_set(board, colour);

    let buildable_empty_disruption_targets: HashSet<(i8, i8)> = build_targets
        .iter()
        .filter(|key| empty_disruption_targets.contains(key))
        .cloned()
        .collect();

    println!(
        "buildable_empty_disruption_targets: {:?}",
        buildable_empty_disruption_targets
    );

    if has_number_card {
        for target in empty_disruption_targets.iter() {
            if build_targets.contains(target) {
                return Some(Plan::Build(*target));
            }
        }
    }


    for &target in disruption_targets.iter() {
        if let Some(space) = board.get(&target) {
            //It's contested enough if it won't be taken for at least one round.
            let contested_enough = {
                let counts = PieceColour::all_values()
                    .into_iter()
                    .map(|c| space.pieces.filtered_indicies(|p| p.colour == c).len());

                counts.filter(|&n| n >= 2).count() >= 2
            };

            if contested_enough {
                continue;
            }
        } else {
            continue;
        }
        println!("target {:?}", target);
        let occupys_target = is_occupied_by(&board, &target, colour);

        if occupys_target && has_ace {
            return Some(Plan::Fly(target));
        } else {
            let adjacent_keys: Vec<_> = {
                let mut adjacent_keys: Vec<_> = FOUR_WAY_OFFSETS
                    .iter()
                    .map(|&(x, y)| (x + target.0, y + target.1))
                    .collect();

                rng.shuffle(&mut adjacent_keys);

                adjacent_keys
            };

            let adjacent_to_target = adjacent_keys
                .iter()
                .any(|key| is_occupied_by(&board, key, colour));


            if adjacent_to_target || occupys_target {
                let pip_budget: Option<u8> =
                    hand.iter()
                        .fold(None, |acc, card| match (acc, pip_value(&card)) {
                            (Some(prev), pip_value) => Some(prev.saturating_add(pip_value)),
                            (None, pip_value) if pip_value == 0 => None,
                            (None, pip_value) => Some(pip_value),
                        });

                if let Some(pip_max) = pip_budget {
                    let possible_target_piece = board.get(&target).and_then(|space| {
                        let mut other_player_pieces = space.pieces.filtered_indicies(|p| {
                            p.colour != colour && u8::from(p.pips) <= pip_max
                            //Don't give your opponent the ability to Hatch
                            //if there is a completable_power_block
                            && (
                                stashes[p.colour].used_count() < STASH_MAX - 1
                                || completable_power_blocks.len() == 0
                            )
                        });

                        other_player_pieces.sort_by_key(|&i| {
                            space
                                .pieces
                                .get(i)
                                .map(|p| stashes[p.colour].used_count())
                                .unwrap_or(0)
                        });

                        other_player_pieces.pop()
                    });
                    if let Some(target_piece) = possible_target_piece {
                        return Some(Plan::ConvertSlashDemolish(target, target_piece));
                    }
                }
            };

            if adjacent_to_target && !occupys_target {
                if has_number_card {
                    let target_blocks = completable_power_blocks
                        .iter()
                        .filter(|b| b.keys.contains(&target));

                    for block in target_blocks {
                        for coord in block.keys.iter() {
                            if buildable_empty_disruption_targets.contains(coord) {
                                return Some(Plan::Build(*coord));
                            }
                        }
                    }
                }

                return Some(Plan::Move(target));
            } else if stashes[colour].is_full() {
                let possible_plan = adjacent_keys
                    .iter()
                    .find(|key| !board.contains_key(key))
                    .map(|target_blank| Plan::Hatch(*target_blank));
                if possible_plan.is_some() {
                    return possible_plan;
                }
            }
        }
    }

    if has_ace {
        for &target in disruption_targets.iter() {
            //Try to fly next to the target

            let adjacent_keys: Vec<_> = {
                let mut adjacent_keys: Vec<_> = FOUR_WAY_OFFSETS
                    .iter()
                    .map(|&(x, y)| (x + target.0, y + target.1))
                    .collect();

                rng.shuffle(&mut adjacent_keys);

                adjacent_keys
            };

            if let Some(f_s) = get_fly_specific(
                board,
                &power_blocks,
                &empty_disruption_targets,
                &adjacent_keys,
                &occupied_spaces,
                &target,
            ) {
                return Some(f_s);
            }
        }
    }

    if has_ace {
        let occupied_disruption_targets: Vec<_> = disruption_targets
            .iter()
            .filter(|key| occupied_spaces.contains(key))
            .cloned()
            .collect();

        for target in empty_disruption_targets.iter() {
            for source in occupied_disruption_targets.iter() {
                if flight_does_not_create_power_block(&board, *source, *target) {
                    return Some(Plan::FlySpecific(*source, *target));
                }
            }
        }

        for target in empty_disruption_targets.iter() {
            for source in occupied_spaces.iter() {
                if flight_does_not_create_power_block(&board, *source, *target) {
                    return Some(Plan::FlySpecific(*source, *target));
                }
            }
        }
    }

    for &target in disruption_targets.iter() {
        let adjacent_filled_keys: Vec<_> = {
            let mut adjacent_filled_keys: Vec<_> = FOUR_WAY_OFFSETS
                .iter()
                .map(|&(x, y)| (x + target.0, y + target.1))
                .filter(|key| board.get(key).is_some())
                .collect();

            rng.shuffle(&mut adjacent_filled_keys);

            adjacent_filled_keys
        };

        //TODO should this only happen on completable power blocks?
        //Walking there when you can't do anything about it seems pointless

        for adjacent in adjacent_filled_keys.iter() {
            for &(x, y) in FOUR_WAY_OFFSETS.iter() {
                if is_occupied_by(board, &(x + adjacent.0, y + adjacent.1), colour) {
                    return Some(Plan::Move(*adjacent));
                }
            }
        }
    }

    None
}

#[cfg(test)]
#[macro_use]
extern crate quickcheck;

#[cfg(test)]
#[macro_use]
extern crate lazy_static;

#[cfg(test)]
mod plan_tests {
    use ::*;
    use common::PieceColour::*;
    use common::Suit::*;

    fn green_vertical_almost_power_block_board() -> Board {
        let mut board = HashMap::new();

        {
            let mut pieces: SpacePieces = Default::default();
            pieces.insert(
                0,
                Piece {
                    colour: Green,
                    pips: Pips::One,
                },
            );
            pieces.insert(
                1,
                Piece {
                    colour: Green,
                    pips: Pips::One,
                },
            );

            board.insert(
                (0, 0),
                Space {
                    card: Card {
                        suit: Clubs,
                        value: Two,
                    },
                    pieces,
                    offset: 0,
                },
            );
        }
        {
            let mut pieces: SpacePieces = Default::default();
            pieces.insert(
                0,
                Piece {
                    colour: Green,
                    pips: Pips::One,
                },
            );

            board.insert(
                (0, 1),
                Space {
                    card: Card {
                        suit: Spades,
                        value: Two,
                    },
                    pieces,
                    offset: 0,
                },
            );
        }

        board
    }
    fn green_vertical_power_block_board() -> Board {
        let mut board = green_vertical_almost_power_block_board();

        {
            board.insert(
                (0, -1),
                Space {
                    card: Card {
                        suit: Diamonds,
                        value: Two,
                    },
                    ..Default::default()
                },
            );
        }

        board
    }

    fn add_space(board: &mut Board, key: (i8, i8), suit: Suit, value: Value) {
        board.insert(
            key,
            Space {
                card: Card { suit, value },
                ..Default::default()
            },
        );
    }

    fn add_piece(board: &mut Board, key: (i8, i8), colour: PieceColour, pips: Pips) {
        board
            .get_mut(&key)
            .map(|s| s.pieces.push(Piece { colour, pips }));
    }

    #[cfg_attr(rustfmt, rustfmt_skip)]
    quickcheck! {
        fn move_in_and_hope(seed: usize) -> bool {
            let seed_slice: &[_] = &[seed];
            let mut rng: StdRng = SeedableRng::from_seed(seed_slice);

            let mut board = green_vertical_power_block_board();

            {
                let mut pieces: SpacePieces = Default::default();
                pieces.insert(0, Piece {
                    colour: Red,
                    pips: Pips::One,
                });

                board.insert((-1,0), Space {
                    card: Card {
                        suit: Hearts,
                        value: Ten,
                    },
                        pieces,
                        offset: 0,
                });
            }

            let player_stash = Stash {
                colour: Green,
                one_pip: NoneLeft,
                two_pip: ThreeLeft,
                three_pip: ThreeLeft,
            };

            let red_stash = Stash {
                colour: Red,
                one_pip: TwoLeft,
                two_pip: ThreeLeft,
                three_pip: ThreeLeft,
            };

            let stashes = Stashes {
                player_stash,
                cpu_stashes: vec![red_stash],
            };

            let hand = vec![];

            let plan = get_plan(&board, &stashes, &hand, &mut rng, Red);

            if let Some(Plan::Move((0,0))) = plan {
                true
            } else {
                println!("plan was {:?}", plan);
                false
            }
        }

        fn fly_towards(seed: usize) -> bool {
            let seed_slice: &[_] = &[seed];
            let mut rng: StdRng = SeedableRng::from_seed(seed_slice);

            let mut board = green_vertical_power_block_board();

            {
                let mut pieces: SpacePieces = Default::default();
                pieces.insert(0, Piece {
                    colour: Red,
                    pips: Pips::One,
                });

                board.insert((-1,2), Space {
                    card: Card {
                        suit: Hearts,
                        value: Ten,
                    },
                        pieces,
                        offset: 0,
                });
            }

            let player_stash = Stash {
                colour: Green,
                one_pip: NoneLeft,
                two_pip: ThreeLeft,
                three_pip: ThreeLeft,
            };

            let red_stash = Stash {
                colour: Red,
                one_pip: TwoLeft,
                two_pip: ThreeLeft,
                three_pip: ThreeLeft,
            };

            let stashes = Stashes {
                player_stash,
                cpu_stashes: vec![red_stash],
            };

            let hand = vec![Card {value: Ace, suit:Spades}];

            let plan = get_plan(&board, &stashes, &hand, &mut rng, Red);

            if let Some(Plan::FlySpecific((-1,2),_)) = plan {
                true
            } else {
                println!("plan was {:?}", plan);
                false
            }
        }

        fn move_in_and_hope_with_number_card_in_hand(seed: usize) -> bool {
            let seed_slice: &[_] = &[seed];
            let mut rng: StdRng = SeedableRng::from_seed(seed_slice);

            let mut board = green_vertical_power_block_board();

            {
                let mut pieces: SpacePieces = Default::default();
                pieces.insert(0, Piece {
                    colour: Red,
                    pips: Pips::One,
                });

                board.insert((-1,0), Space {
                    card: Card {
                        suit: Hearts,
                        value: Ten,
                    },
                        pieces,
                        offset: 0,
                });
            }

            let player_stash = Stash {
                colour: Green,
                one_pip: NoneLeft,
                two_pip: ThreeLeft,
                three_pip: ThreeLeft,
            };

            let red_stash = Stash {
                colour: Red,
                one_pip: TwoLeft,
                two_pip: ThreeLeft,
                three_pip: ThreeLeft,
            };

            let stashes = Stashes {
                player_stash,
                cpu_stashes: vec![red_stash],
            };

            let hand = vec![Card{suit: Diamonds, value:Four}];

            let plan = get_plan(&board, &stashes, &hand, &mut rng, Red);

            if let Some(Plan::Move((0,0))) = plan {
                true
            } else {
                println!("plan was {:?}", plan);
                false
            }
        }

        fn fly_towards_with_number_card_in_hand(seed: usize) -> bool {
            let seed_slice: &[_] = &[seed];
            let mut rng: StdRng = SeedableRng::from_seed(seed_slice);

            let mut board = green_vertical_power_block_board();

            {
                let mut pieces: SpacePieces = Default::default();
                pieces.insert(0, Piece {
                    colour: Red,
                    pips: Pips::One,
                });

                board.insert((-1,2), Space {
                    card: Card {
                        suit: Hearts,
                        value: Ten,
                    },
                        pieces,
                        offset: 0,
                });
            }

            let player_stash = Stash {
                colour: Green,
                one_pip: NoneLeft,
                two_pip: ThreeLeft,
                three_pip: ThreeLeft,
            };

            let red_stash = Stash {
                colour: Red,
                one_pip: TwoLeft,
                two_pip: ThreeLeft,
                three_pip: ThreeLeft,
            };

            let stashes = Stashes {
                player_stash,
                cpu_stashes: vec![red_stash],
            };

            let hand = vec![Card {value: Ace, suit:Spades}, Card{suit: Diamonds, value:Four}];

            let plan = get_plan(&board, &stashes, &hand, &mut rng, Red);

            if let Some(Plan::FlySpecific((-1,2),_)) = plan {
                true
            } else {
                println!("plan was {:?}", plan);
                false
            }
        }

        fn move_towards_almost_power_block(seed: usize) -> bool {
            let seed_slice: &[_] = &[seed];
            let mut rng: StdRng = SeedableRng::from_seed(seed_slice);

            let mut board = green_vertical_almost_power_block_board();

            {
                let mut pieces: SpacePieces = Default::default();
                pieces.insert(0, Piece {
                    colour: Red,
                    pips: Pips::One,
                });

                board.insert((-2,0), Space {
                    card: Card {
                        suit: Hearts,
                        value: Ten,
                    },
                        pieces,
                        offset: 0,
                });
            }
            {
                board.insert((-1,0), Space {
                    card: Card {
                        suit: Hearts,
                        value: Five,
                    },
                    ..Default::default()
                });
            }

            let player_stash = Stash {
                colour: Green,
                one_pip: OneLeft,
                two_pip: ThreeLeft,
                three_pip: ThreeLeft,
            };

            let red_stash = Stash {
                colour: Red,
                one_pip: TwoLeft,
                two_pip: ThreeLeft,
                three_pip: ThreeLeft,
            };

            let stashes = Stashes {
                player_stash,
                cpu_stashes: vec![red_stash],
            };

            let hand = vec![];

            let plan = get_plan(&board, &stashes, &hand, &mut rng, Red);

            if let Some(Plan::Move((-1,0))) = plan {
                true
            } else {
                println!("plan was {:?}", plan);
                false
            }
        }

        fn move_towards_diagonal_almost_power_block(seed: usize) -> bool {
            let seed_slice: &[_] = &[seed];
            let mut rng: StdRng = SeedableRng::from_seed(seed_slice);

            let mut board = HashMap::new();

            {
                let mut pieces: SpacePieces = Default::default();
                pieces.insert(
                    0,
                    Piece {
                        colour: Green,
                        pips: Pips::One,
                    },
                );
                pieces.insert(
                    1,
                    Piece {
                        colour: Green,
                        pips: Pips::One,
                    },
                );

                board.insert(
                    (1, 1),
                    Space {
                        card: Card {
                            suit: Clubs,
                            value: Two,
                        },
                        pieces,
                        offset: 0,
                    },
                );
            }
            {
                let pieces: SpacePieces = Default::default();

                board.insert(
                    (0, 0),
                    Space {
                        card: Card {
                            suit: Spades,
                            value: Two,
                        },
                        pieces,
                        offset: 0,
                    },
                );
            }

            {
                let mut pieces: SpacePieces = Default::default();
                pieces.insert(0, Piece {
                    colour: Red,
                    pips: Pips::One,
                });

                board.insert((-1,0), Space {
                    card: Card {
                        suit: Hearts,
                        value: Ten,
                    },
                        pieces,
                        offset: 0,
                });
            }

            let player_stash = Stash {
                colour: Green,
                one_pip: OneLeft,
                two_pip: ThreeLeft,
                three_pip: ThreeLeft,
            };

            let red_stash = Stash {
                colour: Red,
                one_pip: TwoLeft,
                two_pip: ThreeLeft,
                three_pip: ThreeLeft,
            };

            let stashes = Stashes {
                player_stash,
                cpu_stashes: vec![red_stash],
            };

            let hand = vec![];

            let plan = get_plan(&board, &stashes, &hand, &mut rng, Red);

            if let Some(Plan::Move((0,0))) = plan {
                true
            } else {
                println!("plan was {:?}", plan);
                false
            }
        }

        fn build_to_block(seed: usize) -> bool {
            let seed_slice: &[_] = &[seed];
            let mut rng: StdRng = SeedableRng::from_seed(seed_slice);

            let mut board = HashMap::new();

            {
                let mut pieces: SpacePieces = Default::default();
                pieces.insert(
                    0,
                    Piece {
                        colour: Green,
                        pips: Pips::One,
                    },
                );
                pieces.insert(
                    1,
                    Piece {
                        colour: Green,
                        pips: Pips::One,
                    },
                );

                board.insert(
                    (0, 1),
                    Space {
                        card: Card {
                            suit: Clubs,
                            value: Two,
                        },
                        pieces,
                        offset: 0,
                    },
                );
            }
            {
                board.insert(
                    (-1, 0),
                    Space {
                        card: Card {
                            suit: Spades,
                            value: Two,
                        },
                        .. Default::default()
                    },
                );
            }

            {
                board.insert(
                    (-1, 1),
                    Space {
                        card: Card {
                            suit: Spades,
                            value: Ten,
                        },
                        .. Default::default()
                    },
                );
            }

            {
                let mut pieces: SpacePieces = Default::default();
                pieces.insert(0, Piece {
                    colour: Red,
                    pips: Pips::One,
                });

                board.insert((0,-1), Space {
                    card: Card {
                        suit: Hearts,
                        value: Four,
                    },
                        pieces,
                        offset: 0,
                });
            }
            {
                let mut pieces: SpacePieces = Default::default();
                pieces.insert(0, Piece {
                    colour: Red,
                    pips: Pips::One,
                });

                board.insert((0,-2), Space {
                    card: Card {
                        suit: Spades,
                        value: Five,
                    },
                        pieces,
                        offset: 0,
                });
            }

            let player_stash = Stash {
                colour: Green,
                one_pip: OneLeft,
                two_pip: ThreeLeft,
                three_pip: ThreeLeft,
            };

            let red_stash = Stash {
                colour: Red,
                one_pip: OneLeft,
                two_pip: ThreeLeft,
                three_pip: ThreeLeft,
            };

            let stashes = Stashes {
                player_stash,
                cpu_stashes: vec![red_stash],
            };

            let hand = vec![Card {
                suit: Hearts,
                value: Eight,
            }];

            let plan = get_plan(&board, &stashes, &hand, &mut rng, Red);

            if let Some(Plan::Build((0,0))) = plan {
                true
            } else {
                println!("plan was {:?}", plan);
                false
            }
        }

        fn build_to_block_other_player_power_block(seed: usize) -> bool {
            let seed_slice: &[_] = &[seed];
            let mut rng: StdRng = SeedableRng::from_seed(seed_slice);

            let mut board = HashMap::new();

            {
                let mut pieces: SpacePieces = Default::default();
                pieces.insert(
                    0,
                    Piece {
                        colour: Green,
                        pips: Pips::One,
                    },
                );
                pieces.insert(
                    1,
                    Piece {
                        colour: Green,
                        pips: Pips::One,
                    },
                );

                board.insert(
                    (0, 1),
                    Space {
                        card: Card {
                            suit: Clubs,
                            value: Two,
                        },
                        pieces,
                        offset: 0,
                    },
                );
            }
            {
                board.insert(
                    (-1, 0),
                    Space {
                        card: Card {
                            suit: Spades,
                            value: Two,
                        },
                        .. Default::default()
                    },
                );
            }

            {
                board.insert(
                    (-1, 1),
                    Space {
                        card: Card {
                            suit: Spades,
                            value: Ten,
                        },
                        .. Default::default()
                    },
                );
            }

            {
                let mut pieces: SpacePieces = Default::default();
                pieces.insert(0, Piece {
                    colour: Red,
                    pips: Pips::One,
                });

                board.insert((0,-1), Space {
                    card: Card {
                        suit: Spades,
                        value: Four,
                    },
                        pieces,
                        offset: 0,
                });
            }
            {
                let mut pieces: SpacePieces = Default::default();
                pieces.insert(0, Piece {
                    colour: Red,
                    pips: Pips::One,
                });

                board.insert((0,-2), Space {
                    card: Card {
                        suit: Spades,
                        value: Five,
                    },
                        pieces,
                        offset: 0,
                });
            }

            let player_stash = Stash {
                colour: Green,
                one_pip: OneLeft,
                two_pip: ThreeLeft,
                three_pip: ThreeLeft,
            };

            let red_stash = Stash {
                colour: Red,
                one_pip: OneLeft,
                two_pip: ThreeLeft,
                three_pip: ThreeLeft,
            };

            let stashes = Stashes {
                player_stash,
                cpu_stashes: vec![red_stash],
            };

            let hand = vec![Card {
                suit: Hearts,
                value: Eight,
            }];

            let plan = get_plan(&board, &stashes, &hand, &mut rng, Red);

            if let Some(Plan::Build((0,0))) = plan {
                true
            } else {
                println!("plan was {:?}", plan);
                false
            }
        }

        fn take_winning_step(seed: usize) -> bool {
            let seed_slice: &[_] = &[seed];
            let mut rng: StdRng = SeedableRng::from_seed(seed_slice);

            let board = green_vertical_power_block_board();

            let green_stash = Stash {
                colour: Green,
                one_pip: NoneLeft,
                two_pip: ThreeLeft,
                three_pip: ThreeLeft,
            };

            let stashes = Stashes {
                player_stash: Stash::full(Red),
                cpu_stashes: vec![green_stash],
            };

            let hand = vec![];

            let plan = get_plan(&board, &stashes, &hand, &mut rng, Green);

            match plan {
                Some(Plan::Move((0,-1)))|Some(Plan::MoveSpecific((0,0), (0,-1))) => {
                    true
                },
                _ => {
                    println!("plan was {:?}", plan);
                    false
                }
            }
        }

        fn take_winning_step_when_other_options_exist(seed: usize) -> bool {
            let seed_slice: &[_] = &[seed];
            let mut rng: StdRng = SeedableRng::from_seed(seed_slice);

            let mut board = green_vertical_power_block_board();

            {
                board.insert(
                    (-1, 0),
                    Space {
                        card: Card {
                            suit: Spades,
                            value: Eight,
                        },
                        .. Default::default()
                    },
                );
            }

            let green_stash = Stash {
                colour: Green,
                one_pip: NoneLeft,
                two_pip: ThreeLeft,
                three_pip: ThreeLeft,
            };

            let stashes = Stashes {
                player_stash: Stash::full(Red),
                cpu_stashes: vec![green_stash],
            };

            let hand = vec![];

            let plan = get_plan(&board, &stashes, &hand, &mut rng, Green);

            match plan {
                Some(Plan::Move((0,-1)))|Some(Plan::MoveSpecific((0,0), (0,-1))) => {
                    true
                },
                _ => {
                    println!("plan was {:?}", plan);
                    false
                }
            }
        }

        fn move_to_delay_even_when_another_power_block_exists(seed: usize) -> bool {
            let seed_slice: &[_] = &[seed];
            let mut rng: StdRng = SeedableRng::from_seed(seed_slice);

            let mut board = green_vertical_power_block_board();

            {
                let mut pieces: SpacePieces = Default::default();
                pieces.insert(0, Piece {
                    colour: Red,
                    pips: Pips::One,
                });

                board.insert((0,-2), Space {
                    card: Card {
                        suit: Hearts,
                        value: Ten,
                    },
                        pieces,
                        offset: 0,
                });
            }
            {
                board.insert(
                    (0, -3),
                    Space {
                        card: Card {
                            suit: Spades,
                            value: Eight,
                        },
                        .. Default::default()
                    },
                );
            }

            //alternate power block
            {
                let mut pieces: SpacePieces = Default::default();
                pieces.insert(0, Piece {
                    colour: Green,
                    pips: Pips::Two,
                });
                pieces.insert(1, Piece {
                    colour: Red,
                    pips: Pips::One,
                });

                board.insert((0,-4), Space {
                    card: Card {
                        suit: Diamonds,
                        value: Ten,
                    },
                        pieces,
                        offset: 0,
                });
            }
            {
                let mut pieces: SpacePieces = Default::default();
                pieces.insert(0, Piece {
                    colour: Green,
                    pips: Pips::Two,
                });
                pieces.insert(1, Piece {
                    colour: Red,
                    pips: Pips::Two,
                });

                board.insert((0,-5), Space {
                    card: Card {
                        suit: Diamonds,
                        value: Nine,
                    },
                        pieces,
                        offset: 0,
                });
            }
            {
                board.insert((0,-6), Space {
                    card: Card {
                        suit: Diamonds,
                        value: Eight,
                    },
                    ..Default::default()
                });
            }

            let player_stash = Stash {
                colour: Green,
                one_pip: NoneLeft,
                two_pip: OneLeft,
                three_pip: ThreeLeft,
            };

            let red_stash = Stash {
                colour: Red,
                one_pip: OneLeft,
                two_pip: OneLeft,
                three_pip: ThreeLeft,
            };

            let stashes = Stashes {
                player_stash,
                cpu_stashes: vec![red_stash],
            };

            let hand = vec![];

            let plan = get_plan(&board, &stashes, &hand, &mut rng, Green);

            match plan {
                Some(Plan::Move((0,-1)))|Some(Plan::MoveSpecific((0,0), (0,-1))) => {
                    true
                },
                _ => {
                    println!("plan was {:?}", plan);
                    false
                }
            }
        }

        fn build_rather_than_move_if_other_player_would_win(seed: usize) -> bool {
            let seed_slice: &[_] = &[seed];
            let mut rng: StdRng = SeedableRng::from_seed(seed_slice);

            let mut board = HashMap::new();

            add_space(&mut board, (0,0), Hearts, Two);
            add_piece(&mut board, (0,0), Green, Pips::Two);

            add_space(&mut board, (0,1), Spades, Two);
            add_piece(&mut board, (0,1), Green, Pips::One);

            add_space(&mut board, (1,1), Clubs, Two);
            add_piece(&mut board, (1,1), Red, Pips::One);

            add_space(&mut board, (1,0), Spades, Three);
            add_piece(&mut board, (1,0), Green, Pips::One);

            add_space(&mut board, (2,0), Clubs, Three);
            add_piece(&mut board, (2,0), Green, Pips::One);

            let player_stash = Stash {
                colour: Green,
                one_pip: NoneLeft,
                two_pip: OneLeft,
                three_pip: ThreeLeft,
            };

            let red_stash = Stash {
                colour: Red,
                one_pip: OneLeft,
                two_pip: OneLeft,
                three_pip: ThreeLeft,
            };

            let stashes = Stashes {
                player_stash,
                cpu_stashes: vec![red_stash],
            };

            let hand = vec![Card{suit: Diamonds, value:Four}];

            let plan = get_plan(&board, &stashes, &hand, &mut rng, Red);

            match plan {
                Some(Plan::Build((2,1))) => {
                    true
                },
                _ => {
                    println!("plan was {:?}", plan);
                    false
                }
            }
        }

        fn move_or_c_slash_d_if_other_player_would_win_otherwise(seed: usize) -> bool {
            let seed_slice: &[_] = &[seed];
            let mut rng: StdRng = SeedableRng::from_seed(seed_slice);

            let mut board = HashMap::new();

            add_space(&mut board, (0,0), Hearts, Two);

            add_space(&mut board, (1,0), Clubs, Two);
            add_piece(&mut board, (1,0), Green, Pips::One);
            add_piece(&mut board, (1,0), Green, Pips::One);

            add_space(&mut board, (1,-1), Spades, Two);
            add_piece(&mut board, (1,-1), Green, Pips::One);

            add_space(&mut board, (2,-1), Clubs, Three);
            add_piece(&mut board, (2,-1), Red, Pips::One);

            let player_stash = Stash {
                colour: Green,
                one_pip: NoneLeft,
                two_pip: OneLeft,
                three_pip: ThreeLeft,
            };

            let red_stash = Stash {
                colour: Red,
                one_pip: OneLeft,
                two_pip: OneLeft,
                three_pip: ThreeLeft,
            };

            let stashes = Stashes {
                player_stash,
                cpu_stashes: vec![red_stash],
            };

            let hand = vec![Card{suit: Diamonds, value:Ace}];

            let plan = get_plan(&board, &stashes, &hand, &mut rng, Red);

            match plan {
                Some(Plan::Move((1,-1)))
                | Some(Plan::MoveSpecific((2,-1),(1,-1)))
                | Some(Plan::ConvertSlashDemolish((1,-1), _)) => {
                    true
                },
                _ => {
                    println!("plan was {:?}", plan);
                    false
                }
            }
        }
        fn dont_fly_away_giving_another_player_the_win(seed: usize) -> bool {
            let seed_slice: &[_] = &[seed];
            let mut rng: StdRng = SeedableRng::from_seed(seed_slice);

            let mut board = HashMap::new();

            add_space(&mut board, (0,0), Hearts, Two);
            add_piece(&mut board, (0,0), Green, Pips::One);

            add_space(&mut board, (0,-1), Clubs, Two);
            add_piece(&mut board, (0,-1), Green, Pips::One);

            add_space(&mut board, (1,0), Diamonds, Three);
            add_piece(&mut board, (1,0), Green, Pips::One);

            add_space(&mut board, (1,1), Spades, Two);
            add_piece(&mut board, (1,1), Green, Pips::One);

            add_space(&mut board, (0,1), Clubs, Five);

            add_space(&mut board, (0,2), Clubs, Five);

            add_space(&mut board, (0,-2), Hearts, Seven);
            add_piece(&mut board, (0,-2), Red, Pips::One);


            let player_stash = Stash {
                colour: Green,
                one_pip: NoneLeft,
                two_pip: ThreeLeft,
                three_pip: ThreeLeft,
            };

            let red_stash = Stash {
                colour: Red,
                one_pip: OneLeft,
                two_pip: ThreeLeft,
                three_pip: ThreeLeft,
            };

            let stashes = Stashes {
                player_stash,
                cpu_stashes: vec![red_stash],
            };

            let hand = vec![Card{suit: Diamonds, value:Ace}];

            let plan = get_plan(&board, &stashes, &hand, &mut rng, Red);

            match plan {
                Some(Plan::Fly(_))
                | Some(Plan::FlySpecific(_,_))
                 => {
                     println!("plan was {:?}", plan);
                    false
                },
                _ => {
                    true
                }
            }
        }

        fn prefer_moving_off_of_non_power_block(seed: usize) -> bool {
            let seed_slice: &[_] = &[seed];
            let mut rng: StdRng = SeedableRng::from_seed(seed_slice);

            let mut board = HashMap::new();

            add_space(&mut board, (0,1), Spades, Eight);
            add_piece(&mut board, (0,1), Red, Pips::One);

            add_space(&mut board, (0,0), Hearts, Two);
            add_piece(&mut board, (0,0), Green, Pips::Two);

            add_space(&mut board, (0,-1), Spades, Two);
            add_piece(&mut board, (0,-1), Green, Pips::One);
            add_piece(&mut board, (0,-1), Green, Pips::One);

            add_space(&mut board, (1,-1), Clubs, Two);

            add_space(&mut board, (1,0), Spades, Three);
            add_piece(&mut board, (1,0), Green, Pips::One);
            add_piece(&mut board, (1,0), Red, Pips::One);

            add_space(&mut board, (2,0), Clubs, Three);

            add_space(&mut board, (3,0), Clubs, Three);

            let player_stash = Stash {
                colour: Green,
                one_pip: NoneLeft,
                two_pip: OneLeft,
                three_pip: ThreeLeft,
            };

            let red_stash = Stash {
                colour: Red,
                one_pip: OneLeft,
                two_pip: OneLeft,
                three_pip: ThreeLeft,
            };

            let stashes = Stashes {
                player_stash,
                cpu_stashes: vec![red_stash],
            };

            let hand = vec![Card{suit: Diamonds, value:Four}];

            let plan = get_plan(&board, &stashes, &hand, &mut rng, Red);

            match plan {
                Some(Plan::Move((0,0)))|Some(Plan::MoveSpecific(_, (0,0))) => {
                    true
                },
                _ => {
                    println!("plan was {:?}", plan);
                    false
                }
            }
        }

        fn do_not_fly_a_power_block_over_to_someone_else(seed: usize) -> bool {
            let seed_slice: &[_] = &[seed];
            let mut rng: StdRng = SeedableRng::from_seed(seed_slice);

            let mut board = HashMap::new();

            add_space(&mut board, (0,-1), Spades, Two);
            add_piece(&mut board, (0,-1), Green, Pips::One);
            add_piece(&mut board, (0,-1), Green, Pips::One);

            add_space(&mut board, (1,0), Clubs, Two);
            add_piece(&mut board, (1,0), Green, Pips::Two);

            add_space(&mut board, (1,-1), Spades, Eight);

            add_space(&mut board, (2,0), Spades, Three);
            add_piece(&mut board, (2,0), Green, Pips::One);
            add_piece(&mut board, (2,0), Red, Pips::One);

            add_space(&mut board, (3,0), Hearts, Two);
            add_piece(&mut board, (3,0), Red, Pips::One);

            let player_stash = Stash {
                colour: Green,
                one_pip: NoneLeft,
                two_pip: OneLeft,
                three_pip: ThreeLeft,
            };

            let red_stash = Stash {
                colour: Red,
                one_pip: OneLeft,
                two_pip: OneLeft,
                three_pip: ThreeLeft,
            };

            let stashes = Stashes {
                player_stash,
                cpu_stashes: vec![red_stash],
            };

            let hand = vec![Card{suit: Diamonds, value:Ace}];

            let plan = get_plan(&board, &stashes, &hand, &mut rng, Red);

            match plan {
                Some(Plan::FlySpecific(_, (0,0))) => {
                    false
                },
                _ => {
                    true
                }
            }
        }

        fn dont_move_from_blocking_two_blocks_to_blocking_one_of_them(seed: usize) -> bool {
            let seed_slice: &[_] = &[seed];
            let mut rng: StdRng = SeedableRng::from_seed(seed_slice);

            let mut board = HashMap::new();

            add_space(&mut board, (0,0), Clubs, Two);
            add_piece(&mut board, (0,0), Green, Pips::One);

            add_space(&mut board, (1,0), Clubs, Four);
            add_piece(&mut board, (1,0), Green, Pips::One);
            add_piece(&mut board, (1,0), Red, Pips::One);

            add_space(&mut board, (1,-1), Clubs, Three);
            add_piece(&mut board, (1,-1), Green, Pips::One);

            add_space(&mut board, (1,1), Spades, Four);

            add_space(&mut board, (0,1), Diamonds, Four);

            let player_stash = Stash {
                colour: Green,
                one_pip: NoneLeft,
                two_pip: ThreeLeft,
                three_pip: ThreeLeft,
            };

            let red_stash = Stash {
                colour: Red,
                one_pip: TwoLeft,
                two_pip: ThreeLeft,
                three_pip: ThreeLeft,
            };

            let stashes = Stashes {
                player_stash,
                cpu_stashes: vec![red_stash],
            };

            let hand = vec![Card{suit: Diamonds, value:Eight}];

            let plan = get_plan(&board, &stashes, &hand, &mut rng, Red);

            match plan {
                Some(Plan::Move(_)) => {
                    println!("plan was {:?}", plan);
                    false
                },
                _ => {
                    true
                }
            }
        }
    }


    //I thought that the repeated construction of the table was slow.
    //Doesn't seem to be the case though.
        #[cfg_attr(rustfmt, rustfmt_skip)]
    lazy_static! {
            static ref BREAK_UP_BOARD: HashMap<(i8,i8), Space> = {
                let mut  board = HashMap::new();
                // //Non-compleatable power block
                add_space(&mut board, (0, 0), Diamonds, Six);
                add_piece(&mut board, (0,0), Red, Pips::One);

                add_space(&mut board, (0, 1), Diamonds, Five);
                add_piece(&mut board, (0,1), Red, Pips::Two);

                add_space(&mut board, (-1, 1), Hearts, Eight);

                add_space(&mut board, (-1, 0), Spades, Four);

                add_space(&mut board, (-1, 2), Hearts, Four);
                add_piece(&mut board, (-1, 2), Green, Pips::One);

                add_space(&mut board, (0, 2), Spades, Two);

                add_space(&mut board, (0, -1), Hearts, Seven);

                add_space(&mut board, (1, 0), Clubs, Nine);

                //completable power block
                add_space(&mut board, (1, 1), Clubs, Four);
                add_piece(&mut board, (1, 1), Red, Pips::One);

                add_space(&mut board, (1, 2), Diamonds, Four);
                add_piece(&mut board, (1, 2), Red, Pips::One);

                //disruptable block
                add_space(&mut board, (-1, -1), Clubs, Ten);
                add_piece(&mut board, (-1, -1), Green, Pips::Three);

                add_space(&mut board, (0, -2), Spades, Ten);
                add_piece(&mut board, (0, -2), Green, Pips::Three);

                board
            };
        }


    #[test]
    fn test_break_up_two_blocks_when_possible() {
        //this test is slow
        //TODO find out why (is `lazy_static` being weird?)
        //and make sure it isn't affecting the actual game.
        //An easy thing to try would be a faster hasher
        quickcheck::QuickCheck::new()
            .tests(10)
            .quickcheck(break_up_two_blocks_when_possible as fn(usize) -> bool)
    }

    fn break_up_two_blocks_when_possible(seed: usize) -> bool {
        let seed_slice: &[_] = &[seed];
        let mut rng: StdRng = SeedableRng::from_seed(seed_slice);

        let player_stash = Stash {
            colour: Green,
            one_pip: TwoLeft,
            two_pip: ThreeLeft,
            three_pip: OneLeft,
        };

        let red_stash = Stash {
            colour: Red,
            one_pip: NoneLeft,
            two_pip: TwoLeft,
            three_pip: ThreeLeft,
        };

        let stashes = Stashes {
            player_stash,
            cpu_stashes: vec![red_stash],
        };

        let hand = vec![
            Card {
                value: Ace,
                suit: Spades,
            },
        ];

        let plan = get_plan(&BREAK_UP_BOARD, &stashes, &hand, &mut rng, Red);

        match plan {
            Some(Plan::Fly((1, 1))) |
            Some(Plan::FlySpecific((1, 1), (-1, -2))) |
            Some(Plan::Fly((1, 2))) |
            Some(Plan::FlySpecific((1, 2), (-1, -2))) => true,
            _ => {
                println!("plan was {:?}", plan);
                false
            }
        }
    }
}
