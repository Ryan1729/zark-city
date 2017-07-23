extern crate rand;
extern crate common;

use common::*;
use common::Projection::*;

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

fn make_state(rng: StdRng) -> State {
    let mut state = State {
        rng,
        cam_x: 0.0,
        cam_y: 0.0,
        zoom: f32::powi(1.25, 8),
        board: HashMap::new(),
        mouse_pos: (400, 300),
    };

    add_random_board_card(&mut state);

    state
}


#[no_mangle]
//returns true if quit requested
pub fn update_and_render(p: &Platform, state: &mut State, events: &mut Vec<Event>) -> bool {
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
                state.mouse_pos = (x, y);
            }
            _ => {}
        }
    }

    let aspect_ratio = 800.0 / 600.0;
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

    let view = mat4x4_mul(&camera, &projection);

    for (grid_coords, &Space { card, ref pieces }) in state.board.iter() {

        let (x, y) = to_world_coords(*grid_coords);

        let angle = if (grid_coords.0 + grid_coords.1) % 2 == 0 {
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
            x,
            y,
            0.0,
            1.0,
        ];


        let card_matrix = mat4x4_mul(&world_matrix, &view);

        (p.draw_textured_poly_with_matrix)(card_matrix, CARD_POLY_INDEX, card.texture_spec());

        for (i, piece) in pieces.iter().enumerate() {
            let (x, y) = match i {
                0 => (0.35, 0.5),
                1 => (-0.35, 0.5),
                2 => (-0.35, -0.5),
                3 => (0.35, -0.5),
                _ => (0.0, 0.0),
            };

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
                piece_texture_spec(piece),
            );
        }
        println!("{}", (state.mouse_pos.0 as f32 / 800.0) * 2.0 - 1.0);
        let mouse_matrix = [
            0.05,
            0.0,
            0.0,
            0.0,
            0.0,
            0.05,
            0.0,
            0.0,
            0.0,
            0.0,
            1.0,
            0.0,
            ((24.0 + state.mouse_pos.0 as f32) / 800.0) * 2.0 - 1.0,
            (1.0 - ((24.0 + state.mouse_pos.1 as f32) / 600.0)) * 2.0 - 1.0,
            0.0,
            1.0,
        ];

        (p.draw_textured_poly_with_matrix)(
            // mat4x4_mul(&mouse_matrix, &view),
            mouse_matrix,
            1,
            (0.0125, 0.0, 0.025, 0.05, 0),
        );
    }

    false
}

fn piece_scale(piece: &Piece) -> f32 {
    match piece.pips {
        Pips::One => 33.0 / 1024.0,
        Pips::Two => 49.0 / 1024.0,
        Pips::Three => 65.0 / 1024.0,
    }

}

fn piece_texture_spec(piece: &Piece) -> TextureSpec {
    let size = piece_scale(piece);

    let (x, y) = match piece.pips {
        Pips::One => (114.0 / 1024.0, 792.0 / 1024.0),
        Pips::Two => (65.0 / 1024.0, 776.0 / 1024.0),
        Pips::Three => (0.0, 760.0 / 1024.0),
    };


    let colour_offset = 65.0 / 1024.0 *
        match piece.colour {
            PieceColour::Red => 0.0,
            PieceColour::Yellow => 1.0,
            PieceColour::Green => 2.0,
            PieceColour::Blue => 3.0,
        };

    (x, y + colour_offset, size, size, 0)
}

fn to_world_coords((grid_x, grid_y): (i8, i8)) -> (f32, f32) {
    (grid_x as f32 * 2.0, grid_y as f32 * 2.0)
}

fn add_random_board_card(state: &mut State) {
    state.board.insert(
        (state.rng.gen_range(-4, 4), state.rng.gen_range(-4, 4)),
        state.rng.gen(),
    );
}

const CARD_POLY_INDEX: usize = 0;
const SQUARE_POLY_INDEX: usize = 1;

//These are the verticies of the polygons which can be drawn.
//The index refers to the index of the inner vector within the outer vecton.
#[cfg_attr(rustfmt, rustfmt_skip)]
#[no_mangle]
pub fn get_vert_vecs() -> Vec<Vec<f32>> {
    vec![
        //Card
        vec![
            -0.736842105, 1.0,
            -0.736842105, -1.0,
            0.736842105, -1.0,
            0.736842105, 1.0,
        ],
        //Square
        vec![
            -1.0, 1.0,
            -1.0, -1.0,
            1.0, -1.0,
            1.0, 1.0,
        ],
    ]
}
