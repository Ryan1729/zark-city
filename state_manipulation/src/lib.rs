extern crate rand;
extern crate common;

use common::*;
use common::Projection::*;
use common::Turn::*;

use rand::{StdRng, SeedableRng, Rng};

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
        turn: DrawInitialCard,
        deck,
        pile: Vec::new(),
        player_hand,
        cpu_hands,
    };

    add_random_board_card(&mut state);

    state
}


#[no_mangle]
//returns true if quit requested
pub fn update_and_render(p: &Platform, state: &mut State, events: &mut Vec<Event>) -> bool {
    let mut mouse_pressed = false;
    let mut mouse_released = false;

    for event in events {
        if cfg!(debug_assertions) {
            match *event {
                Event::MouseMove(_) => {}
                _ => println!("{:?}", *event),
            }
        }

        match *event {
            Event::Quit |
            Event::KeyDown(Keycode::Escape) |
            Event::KeyDown(Keycode::F10) => {
                return true;
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
            Event::WindowSize((w, h)) => {
                state.window_wh = (w as f32, h as f32);
                println!("{}", state.window_wh.0 / state.window_wh.1);
            }
            _ => {}
        }
    }

    state.ui_context.frame_init();

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

    for (grid_coords, &Space { card, ref pieces }) in state.board.iter() {

        let (card_x, card_y) = to_world_coords(*grid_coords);

        let card_id = card_id(card_x as _, card_y as _);

        let rotated = (grid_coords.0 + grid_coords.1) % 2 == 0;

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


        let card_matrix = mat4x4_mul(&world_matrix, &view);

        let mut card_texture_spec = card.texture_spec();

        let (card_mouse_x, card_mouse_y) = (world_mouse_x - card_x, world_mouse_y - card_y);

        let on_card = if rotated {
            (card_mouse_x).abs() <= 1.0 && (card_mouse_y).abs() <= CARD_RATIO
        } else {
            (card_mouse_x).abs() <= CARD_RATIO && (card_mouse_y).abs() <= 1.0
        };

        if on_card {
            card_texture_spec.5 = -0.5;
            card_texture_spec.7 = -0.5;
        }

        (p.draw_textured_poly_with_matrix)(card_matrix, CARD_POLY_INDEX, card_texture_spec, 0);

        for (i, piece) in pieces.iter().enumerate() {
            let (x, y) = match i {
                0 => (0.35, 0.5),
                1 => (-0.35, 0.5),
                2 => (-0.35, -0.5),
                3 => (0.35, -0.5),
                _ => (0.0, 0.0),
            };

            let piece_id = piece_id(card_id, i as _);

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

            let mut piece_texture_spec = piece_texture_spec(piece);

            //TODO more precise piece picking
            let on_piece = on_card &&
                if rotated {
                    //swapping x and y and inverting y is equivalent to rotation by 90 degrees
                    -card_mouse_y.signum() == x.signum() && card_mouse_x.signum() == y.signum()
                } else {
                    card_mouse_x.signum() == x.signum() && card_mouse_y.signum() == y.signum()
                };

            let button_outcome = button_logic(
                &mut state.ui_context,
                ButtonState {
                    id: piece_id,
                    pointer_inside: on_piece,
                    mouse_pressed,
                    mouse_released,
                },
            );

            if button_outcome.clicked {
                println!("click");
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


                (p.draw_textured_poly_with_matrix)(
                    mat4x4_mul(&piece_matrix, &card_matrix),
                    SQUARE_POLY_INDEX,
                    piece_texture_spec,
                    0,
                );
            };
        }
    }

    draw_hud(p, state, aspect_ratio, (mouse_x, mouse_y));

    let t = state.turn;

    match state.turn {
        DrawInitialCard => {}
        SelectTurnOption => {}
        DrawThree => {}
        Grow => {}
        Spawn => {}
        Build => {}
        Move => {}
        ConvertSlashDemolish => {}
        Fly => {}
        Hatch => {}
        CpuTurn => {}
        Over(piece_colour) => {}
    };

    if cfg!(debug_assertions) {
        if t != state.turn {
            println!("{:?}", state.turn);
        }
    }

    false
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

        texture_spec.8 = -0.75;

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

    (p.draw_layer)(1);
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

fn piece_scale(piece: &Piece) -> f32 {
    match piece.pips {
        Pips::One => 33.0 / T_S,
        Pips::Two => 49.0 / T_S,
        Pips::Three => LARGEST_PIECE_TEXTURE_SIZE,
    }
}

fn piece_texture_spec(piece: &Piece) -> TextureSpec {
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
