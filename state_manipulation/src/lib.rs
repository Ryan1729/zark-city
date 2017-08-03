extern crate rand;
extern crate common;

use common::*;
use common::Projection::*;
use common::Turn::*;

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

    let mut pile = Vec::new();
    let player_hand;
    let mut cpu_hands;
    {
        let deck_ref = &mut deck;
        let pile_ref = &mut pile;
        let rng_ref = &mut rng;

        //WE assume there are enough cards in the deck at the start

        player_hand = vec![
            deal_parts(deck_ref, pile_ref, rng_ref).unwrap(),
            deal_parts(deck_ref, pile_ref, rng_ref).unwrap(),
            deal_parts(deck_ref, pile_ref, rng_ref).unwrap(),
        ];

        let cpu_players_count = rng_ref.gen_range(1, 5);
        cpu_hands = Vec::new();

        for _ in 0..cpu_players_count {
            cpu_hands.push(vec![
                deal_parts(deck_ref, pile_ref, rng_ref).unwrap(),
                deal_parts(deck_ref, pile_ref, rng_ref).unwrap(),
                deal_parts(deck_ref, pile_ref, rng_ref).unwrap(),
            ]);
        }
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
        turn: Move,
        deck,
        pile: Vec::new(),
        player_hand,
        cpu_hands,
        hud_alpha: 1.0,
    };

    add_random_board_card(&mut state);

    state
}

enum Action {
    GrabPiece((i8, i8), usize),
    GrabSpace((i8, i8)),
    NoAction,
}
use Action::*;

const FADE_RATE: f32 = 1.0 / 24.0;

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
                state.board.clear();
                add_random_board_card(state);
            }
            Event::KeyDown(Keycode::V) => {
                (p.set_verts)(get_vert_vecs());
            }
            Event::KeyDown(Keycode::Up) => {
                state.cam_y += 0.0625;
            }
            Event::KeyDown(Keycode::Down) => {
                state.cam_y -= 0.0625;
            }
            Event::KeyDown(Keycode::Right) => {
                state.cam_x += 0.0625;
            }
            Event::KeyDown(Keycode::Left) => {
                state.cam_x -= 0.0625;
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

    state.ui_context.frame_init();

    state.hud_alpha += if state.mouse_pos.1 / state.window_wh.1 > 0.7 {
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

        let camera = [
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
            state.cam_x,
            state.cam_y,
            0.0,
            1.0,
        ];

        let inverse_camera = [
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
            -state.cam_x,
            -state.cam_y,
            0.0,
            1.0,
        ];

        let view = mat4x4_mul(&camera, &projection);
        let inverse_view = mat4x4_mul(&inverse_projection, &inverse_camera);

        (view, inverse_view)
    };

    let (world_mouse_x, world_mouse_y, _, _) =
        mat4x4_vector_mul_divide(&inverse_view, mouse_x, mouse_y, 0.0, 1.0);


    let mut action = NoAction;

    for (grid_coords, &Space { card, ref pieces }) in state.board.iter() {
        let (card_x, card_y, rotated) = get_card_spec(grid_coords);

        let card_matrix = get_card_matrix(&view, (card_x, card_y, rotated));

        let card_id = card_id(card_x as _, card_y as _);

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
            },
        );

        if button_outcome.clicked {
            action = GrabSpace(grid_coords.clone());
        } else {
            let highlight = if let MoveSelect(piece_pickup_coords, _, _) = state.turn {
                let orthogonally_adjacent_filled_spaces =
                    get_orthogonally_adjacent_filled_spaces(&state.board, piece_pickup_coords);

                orthogonally_adjacent_filled_spaces.contains(&grid_coords)
            } else {
                false
            };

            match (button_outcome.draw_state, highlight) {
                (_, true) | (Hover, false) => {
                    card_texture_spec.5 = -0.5;
                    card_texture_spec.7 = -0.5;
                }
                (Pressed, false) => {
                    card_texture_spec.5 = -0.5;
                    card_texture_spec.6 = -0.5;
                }
                (Inactive, false) => {}
            }

            draw_card(p, card_matrix, card, card_texture_spec);
        }

        for (i, piece) in pieces.into_iter().enumerate() {
            let (x, y) = card_relative_piece_coords(i);

            let piece_id = piece_id(card_id, i as _);

            let mut piece_texture_spec = piece_texture_spec(piece);

            //TODO more precise piece picking
            let on_piece = on_card &&
                if rotated {
                    //swapping x and y and inverting y is equivalent to rotation by 90 degrees
                    -card_mouse_y.signum() == x.signum() && card_mouse_x.signum() == y.signum()
                } else {
                    card_mouse_x.signum() == x.signum() && card_mouse_y.signum() == y.signum()
                };

            let button_outcome = match state.turn {
                Move => {
                    button_logic(
                        &mut state.ui_context,
                        ButtonState {
                            id: piece_id,
                            pointer_inside: on_piece,
                            mouse_pressed,
                            mouse_released,
                        },
                    )
                }
                _ => Default::default(),
            };

            if button_outcome.clicked {
                action = GrabPiece(grid_coords.clone(), i);
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

    }


    let t = state.turn;

    match state.turn {
        DrawInitialCard => {}
        SelectTurnOption => {}
        DrawThree => {}
        Grow => {}
        Spawn => {}
        Build => {}
        Move => {
            if let GrabPiece(space_coords, piece_index) = action {
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
                let orthogonally_adjacent_filled_spaces =
                    get_orthogonally_adjacent_filled_spaces(&state.board, space_coords);

                if orthogonally_adjacent_filled_spaces.contains(&key) {
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
            if let GrabSpace(key) = action {
                if let Some(space) = state.board.remove(&key) {
                    state.turn = FlySelect(key, space);
                }
            }
        }
        FlySelect(old_coords, space) => {
            let Space { card, pieces } = space;

            let adjacent_empty_spaces = {
                let mut spaces = get_all_adjacent_empty_spaces(&state.board);
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
                    },
                )
            } else {
                Default::default()
            };

            draw_card(p, card_matrix, card, card.texture_spec());

            for (i, piece) in pieces.into_iter().enumerate() {
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
                //TODO real target turn
                state.turn = Fly;
            }
        }
        Hatch => {}
        CpuTurn => {}
        Over(piece_colour) => {}
    };

    if cfg!(debug_assertions) {
        if t != state.turn {
            println!("{:?}", state.turn);
        }
    }

    match state.turn {
        FlySelect(_, _) => {}
        _ => {
            if escape_pressed {
                return true;
            }
        }
    };

    draw_hud(p, state, aspect_ratio, (mouse_x, mouse_y));

    false
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

fn get_orthogonally_adjacent_filled_spaces(board: &Board, (x, y): (i8, i8)) -> HashSet<(i8, i8)> {
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

fn get_all_adjacent_empty_spaces(board: &Board) -> HashSet<(i8, i8)> {
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

    let mut adjacent_empty_spaces = HashSet::new();

    for &(x, y) in filled_coords {
        for &(dx, dy) in offsets.iter() {
            let new_coords = (x.saturating_add(dx), y.saturating_add(dy));
            if !board.contains_key(&new_coords) {
                adjacent_empty_spaces.insert(new_coords);
            }
        }
    }

    adjacent_empty_spaces
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

    let piece_matrix = [
        scale * 5.0,
        0.0,
        0.0,
        0.0,
        0.0,
        scale * 5.0,
        0.0,
        0.0,
        0.0,
        0.0,
        1.0,
        0.0,
        x,
        y,
        0.0,
        1.0,
    ];

    (p.draw_textured_poly_with_matrix)(
        mat4x4_mul(&piece_matrix, &card_matrix),
        SQUARE_POLY_INDEX,
        piece_texture_spec,
        0,
    );
}

fn draw_card(p: &Platform, card_matrix: [f32; 16], card: Card, texture_spec: TextureSpec) {
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

fn draw_hud(p: &Platform, state: &mut State, aspect_ratio: f32, (mouse_x, mouse_y): (f32, f32)) {
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

    let hud_view = get_projection(&ProjectionSpec {
        top,
        bottom,
        left,
        right,
        near,
        far,
        projection: Perspective,
        // projection: Orthographic,
    });

    for (i, card) in state.player_hand.iter().enumerate() {
        let (card_x, card_y) = (-half_width * (13.0 - i as f32) / 16.0, -half_height * 0.75);

        let hand_camera_matrix = [
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
            card_x,
            card_y,
            0.0,
            1.0,
        ];

        let card_matrix = mat4x4_mul(&hand_camera_matrix, &hud_view);

        let mut texture_spec = card.texture_spec();

        (p.draw_textured_poly_with_matrix)(card_matrix, CARD_POLY_INDEX, texture_spec, layer);
    }

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

        let piece_texture_spec = (
            3.0 * CARD_TEXTURE_WIDTH,
            4.0 * CARD_TEXTURE_HEIGHT +
                (f32::from(piece_colour) * TOOLTIP_TEXTURE_HEIGHT_OFFSET),
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
}

fn card_id(card_x: UiId, card_y: UiId) -> UiId {
    //assumss| card_x| and |card_y| will both stay below 1000
    10_000 + (card_x + 1000) * 1000 + card_y
}

fn piece_id(card_id: UiId, offset: UiId) -> UiId {
    //assumes |offset| < 500
    card_id + 30_000 + (offset + 500) * 1000
}


#[derive(Copy, Clone, Debug)]
struct ButtonState {
    id: UiId,
    pointer_inside: bool,
    mouse_pressed: bool,
    mouse_released: bool,
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

    let draw_state = if context.active == id && button_state.mouse_pressed {
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
    match piece.pips {
        Pips::One => 33.0 / T_S,
        Pips::Two => 49.0 / T_S,
        Pips::Three => LARGEST_PIECE_TEXTURE_SIZE,
    }
}

fn piece_texture_spec(piece: Piece) -> TextureSpec {
    let size = piece_scale(piece);

    let (x, y) = match piece.pips {
        Pips::One => (114.0 / T_S, 792.0 / T_S),
        Pips::Two => (65.0 / T_S, 776.0 / T_S),
        Pips::Three => (0.0, 760.0 / T_S),
    };


    let colour_offset = LARGEST_PIECE_TEXTURE_SIZE *
        match piece.colour {
            PieceColour::Red => 0.0,
            PieceColour::Yellow => 1.0,
            PieceColour::Green => 2.0,
            PieceColour::Blue => 3.0,
        };

    (x, y + colour_offset, size, size, 0, 0.0, 0.0, 0.0, 0.0)
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
