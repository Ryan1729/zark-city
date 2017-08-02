extern crate open_gl_bindings;
extern crate sdl2;

extern crate common;
extern crate image_decoding;

#[cfg(debug_assertions)]
extern crate libloading;
#[cfg(not(debug_assertions))]
extern crate state_manipulation;

#[cfg(debug_assertions)]
use libloading::Library;

use open_gl_bindings::gl;

use sdl2::event::Event;

use std::fs::File;



use std::ffi::CString;
use std::str;

use common::*;

#[cfg(debug_assertions)]
const LIB_PATH: &'static str = "./target/debug/libstate_manipulation.so";
#[cfg(not(debug_assertions))]
const LIB_PATH: &'static str = "Hopefully compiled out";

#[cfg(debug_assertions)]
struct Application {
    library: Library,
}
#[cfg(not(debug_assertions))]
struct Application {}

#[cfg(debug_assertions)]
impl Application {
    fn new() -> Self {
        let library = Library::new(LIB_PATH).unwrap_or_else(|error| panic!("{}", error));

        Application { library: library }
    }

    fn new_state(&self) -> State {
        unsafe {
            let f = self.library.get::<fn() -> State>(b"new_state\0").unwrap();

            f()
        }
    }

    fn update_and_render(
        &self,
        platform: &Platform,
        state: &mut State,
        events: &Vec<common::Event>,
    ) -> bool {
        unsafe {
            let f = self.library
                .get::<fn(&Platform, &mut State, &Vec<common::Event>) -> bool>(
                    b"update_and_render\0",
                )
                .unwrap();
            f(platform, state, events)
        }
    }

    fn get_vert_vecs(&self) -> Vec<Vec<f32>> {
        unsafe {
            let f = self.library
                .get::<fn() -> Vec<Vec<f32>>>(b"get_vert_vecs\0")
                .unwrap();

            f()
        }
    }
}
#[cfg(not(debug_assertions))]
impl Application {
    fn new() -> Self {
        Application {}
    }

    fn new_state(&self) -> State {
        state_manipulation::new_state()
    }

    fn update_and_render(
        &self,
        platform: &Platform,
        state: &mut State,
        events: &mut Vec<common::Event>,
    ) -> bool {
        state_manipulation::update_and_render(platform, state, events)
    }

    fn get_vert_vecs(&self) -> Vec<Vec<f32>> {
        state_manipulation::get_vert_vecs()
    }
}

fn find_sdl_gl_driver() -> Option<u32> {
    for (index, item) in sdl2::render::drivers().enumerate() {
        if item.name == "opengl" {
            return Some(index as u32);
        }
    }
    None
}

type Ranges = [(u16, u16); 16];

static mut RESOURCES: Option<Resources> = None;

type Textures = [gl::types::GLuint; 2];

//this includes the default framebuffer
const FRAMEBUFFER_COUNT: usize = 2;
type FrameBufferHandles = [gl::types::GLuint; FRAMEBUFFER_COUNT];

struct Resources {
    ctx: gl::Gl,
    vertex_buffer: gl::types::GLuint,
    vert_ranges_len: usize,
    vert_ranges: Ranges,
    textures: Textures,
    colour_shader: ColourShader,
    texture_shader: TextureShader,
    frame_buffers: FrameBufferHandles,
    frame_buffer_textures: FrameBufferHandles,
    frame_buffer_render_buffers: FrameBufferHandles,
}

impl Resources {
    fn new(app: &Application, ctx: gl::Gl, (width, height): (u32, u32)) -> Option<Self> {

        let mut frame_buffers = [0; FRAMEBUFFER_COUNT];
        let mut frame_buffer_textures = [0; FRAMEBUFFER_COUNT];
        let mut frame_buffer_render_buffers = [0; FRAMEBUFFER_COUNT];

        unsafe {
            ctx.Viewport(0, 0, width as _, height as _);

            ctx.Enable(gl::TEXTURE_2D);
            ctx.Enable(gl::BLEND);
            reset_blend_func(&ctx);

            //create framebuffers
            ctx.GenFramebuffers(1, frame_buffers.as_mut_ptr().offset(1));
            println!("{:?}", frame_buffers);

            ctx.BindFramebuffer(gl::FRAMEBUFFER, frame_buffers[1]);

            ctx.GenTextures(1, frame_buffer_textures.as_mut_ptr().offset(1));

            let texture_handle = frame_buffer_textures[1];

            ctx.BindTexture(gl::TEXTURE_2D, texture_handle);

            ctx.TexImage2D(
                gl::TEXTURE_2D,
                0,
                gl::RGBA as _,
                width as _,
                height as _,
                0,
                gl::RGBA,
                gl::UNSIGNED_BYTE,
                std::ptr::null(),
            );

            ctx.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::LINEAR as _);
            ctx.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::LINEAR as _);
            ctx.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, gl::CLAMP_TO_EDGE as _);
            ctx.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, gl::CLAMP_TO_EDGE as _);

            ctx.FramebufferTexture2D(
                gl::FRAMEBUFFER,
                gl::COLOR_ATTACHMENT0,
                gl::TEXTURE_2D,
                texture_handle,
                0,
            );

            // unbind
            ctx.BindTexture(gl::TEXTURE_2D, 0);

            ctx.GenRenderbuffers(1, frame_buffer_render_buffers.as_mut_ptr().offset(1));
            ctx.BindRenderbuffer(gl::RENDERBUFFER, frame_buffer_render_buffers[1]);
            ctx.RenderbufferStorage(
                gl::RENDERBUFFER,
                gl::DEPTH24_STENCIL8,
                width as _,
                height as _,
            );

            if cfg!(debug_assertions) {
                for &frame_buffer in frame_buffers.iter() {
                    ctx.BindFramebuffer(gl::FRAMEBUFFER, frame_buffer);

                    let status = ctx.CheckFramebufferStatus(gl::FRAMEBUFFER);
                    debug_assert!(
                        status == gl::FRAMEBUFFER_COMPLETE,
                        "CheckFramebufferStatus returned {0:x}",
                        status
                    );
                }
            }

            clear_all(&ctx, &frame_buffers);

            ctx.BindFramebuffer(gl::FRAMEBUFFER, 0);
        }

        let colour_shader = {
            let vs = compile_shader(&ctx, UNTEXTURED_VS_SRC, gl::VERTEX_SHADER);

            let fs = compile_shader(&ctx, UNTEXTURED_FS_SRC, gl::FRAGMENT_SHADER);

            let program = link_program(&ctx, vs, fs);

            let pos_attr = unsafe {
                ctx.GetAttribLocation(program, CString::new("position").unwrap().as_ptr())
            };
            let matrix_uniform = unsafe {
                ctx.GetUniformLocation(program, CString::new("matrix").unwrap().as_ptr())
            };

            let colour_uniform = unsafe {
                ctx.GetUniformLocation(program, CString::new("colour").unwrap().as_ptr())
            };

            ColourShader {
                program,
                pos_attr,
                matrix_uniform,
                colour_uniform,
            }
        };

        let texture_shader = {
            let vs = compile_shader(&ctx, TEXTURED_VS_SRC, gl::VERTEX_SHADER);

            let fs = compile_shader(&ctx, TEXTURED_FS_SRC, gl::FRAGMENT_SHADER);

            let program = link_program(&ctx, vs, fs);

            let pos_attr = unsafe {
                ctx.GetAttribLocation(program, CString::new("position").unwrap().as_ptr())
            };
            let matrix_uniform = unsafe {
                ctx.GetUniformLocation(program, CString::new("matrix").unwrap().as_ptr())
            };

            let texture_uniforms = unsafe {
                [
                    ctx.GetUniformLocation(program, CString::new("textures[0]").unwrap().as_ptr()),
                    ctx.GetUniformLocation(program, CString::new("textures[1]").unwrap().as_ptr()),
                ]
            };

            let texture_xywh_uniform = unsafe {
                ctx.GetUniformLocation(program, CString::new("texture_xywh").unwrap().as_ptr())
            };

            let texture_index_uniform = unsafe {
                ctx.GetUniformLocation(program, CString::new("texture_index").unwrap().as_ptr())
            };

            let tint_uniform =
                unsafe { ctx.GetUniformLocation(program, CString::new("tint").unwrap().as_ptr()) };

            TextureShader {
                program,
                pos_attr,
                matrix_uniform,
                texture_uniforms,
                texture_xywh_uniform,
                texture_index_uniform,
                tint_uniform,
            }
        };

        unsafe {
            ctx.UseProgram(colour_shader.program);
        }

        let textures = [
            make_texture_from_png(&ctx, "images/texture0.png"),
            make_texture_from_png(&ctx, "images/texture1.png"),
        ];

        let mut result = Resources {
            ctx,
            vert_ranges: [(0, 0); 16],
            vert_ranges_len: 0,
            vertex_buffer: 0,
            colour_shader,
            texture_shader,
            textures,
            frame_buffers,
            frame_buffer_textures,
            frame_buffer_render_buffers,
        };

        result.set_verts(app.get_vert_vecs());

        Some(result)
    }

    fn set_verts(&mut self, vert_vecs: Vec<Vec<f32>>) {

        let (verts, vert_ranges, vert_ranges_len) = get_verts_and_ranges(vert_vecs);

        let vertex_buffer = unsafe {
            let mut buffer = 0;

            self.ctx.GenBuffers(1, &mut buffer as _);
            self.ctx.BindBuffer(gl::ARRAY_BUFFER, buffer);
            self.ctx.BufferData(
                gl::ARRAY_BUFFER,
                (verts.len() * std::mem::size_of::<f32>()) as _,
                std::mem::transmute(verts.as_ptr()),
                gl::DYNAMIC_DRAW,
            );

            buffer
        };

        //TODO (assuming we don't end up manipulating this at all)
        // Does this need to be any longer than the longest single polygon?
        // If not, is the extra GPU memory usage significant?
        let indices: Vec<gl::types::GLushort> =
            (0..verts.len()).map(|x| x as gl::types::GLushort).collect();

        unsafe {
            let mut buffer = 0;

            self.ctx.GenBuffers(1, &mut buffer as _);
            self.ctx.BindBuffer(gl::ELEMENT_ARRAY_BUFFER, buffer);
            self.ctx.BufferData(
                gl::ELEMENT_ARRAY_BUFFER,
                (indices.len() * std::mem::size_of::<gl::types::GLushort>()) as _,
                std::mem::transmute(indices.as_ptr()),
                gl::DYNAMIC_DRAW,
            );

            buffer
        };

        self.vert_ranges = vert_ranges;
        self.vert_ranges_len = vert_ranges_len;
        self.vertex_buffer = vertex_buffer;
    }
}

fn main() {
    let mut app = Application::new();

    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let gl_attr = video_subsystem.gl_attr();
    gl_attr.set_stencil_size(1);
    gl_attr.set_context_major_version(2);
    gl_attr.set_context_minor_version(1);

    let canvas = video_subsystem
        .window("Window", INITIAL_WINDOW_WIDTH, INITIAL_WINDOW_HEIGHT)
        .opengl()
        .build()
        .unwrap()
        .into_canvas()
        .index(find_sdl_gl_driver().unwrap())
        .build()
        .unwrap();

    unsafe {
        let ctx = gl::Gl::load_with(|name| video_subsystem.gl_get_proc_address(name) as *const _);
        canvas.window().gl_set_context_to_current().unwrap();
        println!("{:?}", canvas.window().drawable_size());
        RESOURCES = Resources::new(&app, ctx, canvas.window().drawable_size());
    }

    if cfg!(debug_assertions) {
        if let Some(ref resources) = unsafe { RESOURCES.as_ref() } {
            let mut err;
            while {
                err = unsafe { resources.ctx.GetError() };
                err != gl::NO_ERROR
            }
            {
                println!("Resources setup OpenGL error: {}", err);
            }
            if err != gl::NO_ERROR {
                panic!();
            }
        }
    }

    let mut state = app.new_state();

    let mut last_modified = if cfg!(debug_assertions) {
        std::fs::metadata(LIB_PATH).unwrap().modified().unwrap()
    } else {
        //hopefully this is actually compiled out
        std::time::SystemTime::now()
    };


    let platform = Platform {
        draw_poly,
        draw_poly_with_matrix,
        draw_textured_poly,
        draw_textured_poly_with_matrix,
        draw_layer,
        set_verts,
    };

    let mut events = Vec::new();

    app.update_and_render(&platform, &mut state, &mut events);

    if let Some(ref resources) = unsafe { RESOURCES.as_ref() } {
        let window = canvas.window();
        //I see a flat 16ms a lot of places. Should I be leaving that slack in here?
        let frame_duration = std::time::Duration::new(0, 16666666);

        let mut event_pump = sdl_context.event_pump().unwrap();

        loop {
            let start = std::time::Instant::now();

            events.clear();

            for event in event_pump.poll_iter() {
                match event {
                    Event::Quit { .. } => events.push(common::Event::Quit),
                    Event::KeyDown { keycode: Some(kc), .. } => {
                        events.push(common::Event::KeyDown(unsafe { std::mem::transmute(kc) }))
                    }
                    Event::KeyUp { keycode: Some(kc), .. } => {
                        events.push(common::Event::KeyUp(unsafe { std::mem::transmute(kc) }))
                    }
                    Event::MouseMotion { x, y, .. } => {
                        events.push(common::Event::MouseMove((x, y)))
                    }
                    Event::MouseButtonUp { mouse_btn: sdl2::mouse::MouseButton::Left, .. } => {
                        events.push(common::Event::LeftMouseUp)
                    }
                    Event::MouseButtonDown {
                        mouse_btn: sdl2::mouse::MouseButton::Left, ..
                    } => events.push(common::Event::LeftMouseDown),
                    Event::Window {
                        win_event: sdl2::event::WindowEvent::Resized(w, h), ..
                    } |
                    Event::Window {
                        win_event: sdl2::event::WindowEvent::SizeChanged(w, h), ..
                    } => {
                        events.push(common::Event::WindowSize((w, h)));
                        unsafe {
                            resources.ctx.Viewport(0, 0, w, h);
                        }
                    }
                    _ => {}
                }
            }

            unsafe {
                clear_all(&resources.ctx, &resources.frame_buffers);

                resources.ctx.BindFramebuffer(gl::FRAMEBUFFER, 0);
            }

            if app.update_and_render(&platform, &mut state, &mut events) {
                //quit requested
                break;
            }

            if cfg!(debug_assertions) {
                if let Ok(Ok(modified)) = std::fs::metadata(LIB_PATH).map(|m| m.modified()) {
                    if modified > last_modified {
                        drop(app);
                        app = Application::new();
                        last_modified = modified;
                    }
                }
            }

            if cfg!(debug_assertions) {
                let mut err;
                while {
                    err = unsafe { resources.ctx.GetError() };
                    err != gl::NO_ERROR
                }
                {
                    println!("OpenGL error: {}", err);
                }
                if err != gl::NO_ERROR {
                    panic!();
                }
            }

            window.gl_swap_window();

            if let Some(sleep_time) = frame_duration.checked_sub(
                std::time::Instant::now().duration_since(
                    start,
                ),
            )
            {
                std::thread::sleep(sleep_time);
            }

        }
    } else {
        println!("Could not open window.");
    }

}

fn get_frame_buffer(resources: &Resources, frame_buffer_index: usize) -> gl::types::GLuint {
    resources.frame_buffers[if frame_buffer_index < FRAMEBUFFER_COUNT {
                                frame_buffer_index
                            } else {
                                0
                            }]
}

// these `draw_` functions should probably batch draw calls to minimize shader switching,
// but I'll be able to provide the same API and change to that later so it can wait
fn draw_poly_with_matrix(world_matrix: [f32; 16], poly_index: usize, frame_buffer_index: usize) {
    if let Some(ref resources) = unsafe { RESOURCES.as_ref() } {
        unsafe {
            resources.ctx.UniformMatrix4fv(
                resources.colour_shader.matrix_uniform as _,
                1,
                gl::FALSE,
                world_matrix.as_ptr() as _,
            );
        }
        let frame_buffer = get_frame_buffer(resources, frame_buffer_index);

        let (start, end) = resources.vert_ranges[poly_index];

        draw_verts_with_outline(
            &resources.ctx,
            start as _,
            ((end + 1 - start) / 2) as _,
            resources.vertex_buffer,
            &resources.colour_shader,
            frame_buffer,
        );
    }
}

fn draw_poly(x: f32, y: f32, index: usize, frame_buffer_index: usize) {
    let mut world_matrix: [f32; 16] = [
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
        0.0,
        0.0,
        0.0,
        1.0,
    ];

    world_matrix[12] = x;
    world_matrix[13] = y;

    draw_poly_with_matrix(world_matrix, index, frame_buffer_index);
}

fn draw_textured_poly(
    x: f32,
    y: f32,
    poly_index: usize,
    texture_spec: TextureSpec,
    frame_buffer_index: usize,
) {
    let mut world_matrix: [f32; 16] = [
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
        0.0,
        0.0,
        0.0,
        1.0,
    ];

    world_matrix[12] = x;
    world_matrix[13] = y;

    draw_textured_poly_with_matrix(world_matrix, poly_index, texture_spec, frame_buffer_index);
}

fn draw_textured_poly_with_matrix(
    world_matrix: [f32; 16],
    poly_index: usize,
    (texture_x, texture_y, texture_w, texture_h, texture_index, tint_r,
tint_g,
tint_b,
tint_a): TextureSpec,
frame_buffer_index: usize
){
    if let Some(ref resources) = unsafe { RESOURCES.as_ref() } {
        unsafe {
            resources.ctx.UniformMatrix4fv(
                resources.texture_shader.matrix_uniform as _,
                1,
                gl::FALSE,
                world_matrix.as_ptr() as _,
            );
        }

        let frame_buffer = get_frame_buffer(resources, frame_buffer_index);

        let (start, end) = resources.vert_ranges[poly_index];

        draw_verts_with_texture(
            &resources.ctx,
            start as _,
            ((end + 1 - start) / 2) as _,
            resources.vertex_buffer,
            &resources.texture_shader,
            &resources.textures,
            texture_x,
            texture_y,
            texture_w,
            texture_h,
            texture_index,
            tint_r,
            tint_g,
            tint_b,
            tint_a,
            frame_buffer,
        );
    }
}

fn draw_layer(frame_buffer_index: usize) {
    if let Some(ref mut resources) = unsafe { RESOURCES.as_mut() } {
        let frame_buffer = get_frame_buffer(resources, frame_buffer_index);
        let textures = resources.textures;
        let texture_shader = &resources.texture_shader;
        let vertex_buffer = resources.vertex_buffer;
        let ctx = &resources.ctx;

        let (start, end) = resources.vert_ranges[1];
        let vert_count = ((end + 1 - start) / 2) as _;

        if frame_buffer != 0 {
            unsafe {
                ctx.BindFramebuffer(gl::FRAMEBUFFER, 0);
                ctx.BlendFunc(gl::SRC_ALPHA, gl::ONE_MINUS_SRC_ALPHA);

                let world_matrix: [f32; 16] = [
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
                    0.0,
                    0.0,
                    0.0,
                    1.0,
                ];

                resources.ctx.UniformMatrix4fv(
                    resources.texture_shader.matrix_uniform as _,
                    1,
                    gl::FALSE,
                    world_matrix.as_ptr() as _,
                );


                ctx.UseProgram(texture_shader.program);

                ctx.BindBuffer(gl::ARRAY_BUFFER, vertex_buffer);
                ctx.EnableVertexAttribArray(texture_shader.pos_attr as _);
                ctx.VertexAttribPointer(
                    texture_shader.pos_attr as _,
                    2,
                    gl::FLOAT,
                    gl::FALSE as _,
                    0,
                    std::ptr::null().offset(start as isize * std::mem::size_of::<f32>() as isize),
                );

                ctx.ActiveTexture(gl::TEXTURE0);
                ctx.BindTexture(
                    gl::TEXTURE_2D,
                    resources.frame_buffer_textures[frame_buffer_index],
                );
                ctx.Uniform1i(texture_shader.texture_uniforms[0], 0);

                ctx.ActiveTexture(gl::TEXTURE1);
                ctx.BindTexture(gl::TEXTURE_2D, textures[1]);
                ctx.Uniform1i(texture_shader.texture_uniforms[1], 1);

                ctx.Uniform1i(texture_shader.texture_index_uniform, 0);

                //1 - y = (y * -1) + 1 so this flips the y texture coord
                ctx.Uniform4f(texture_shader.texture_xywh_uniform, 0.0, 1.0, 1.0, -1.0);

                ctx.Uniform4f(texture_shader.tint_uniform, 0.0, 0.0, 0.0, -0.5);

                ctx.DrawArrays(gl::TRIANGLE_FAN, 0, vert_count);

                ctx.BindFramebuffer(gl::FRAMEBUFFER, 0);

                reset_blend_func(ctx);
            }
        }
    }
}

fn set_verts(vert_vecs: Vec<Vec<f32>>) {
    if let Some(ref mut resources) = unsafe { RESOURCES.as_mut() } {
        resources.set_verts(vert_vecs);
    }
}


unsafe fn begin_using_frame_buffer(ctx: &gl::Gl, frame_buffer: gl::types::GLuint) {
    ctx.BindFramebuffer(gl::FRAMEBUFFER, frame_buffer);
}
unsafe fn end_using_frame_buffer(ctx: &gl::Gl) {
    ctx.BindFramebuffer(gl::FRAMEBUFFER, 0);
}

unsafe fn reset_blend_func(ctx: &gl::Gl) {
    ctx.BlendFunc(gl::ONE, gl::ONE_MINUS_SRC_ALPHA);
}

unsafe fn clear_all(ctx: &gl::Gl, frame_buffers: &FrameBufferHandles) {
    let brightness = 25.0 / 255.0;
    for i in 0..frame_buffers.len() {
        let frame_buffer = frame_buffers[i];
        ctx.BindFramebuffer(gl::FRAMEBUFFER, frame_buffer);

        if i == 0 {
            ctx.ClearColor(brightness, brightness, brightness, 1.0);
        } else {
            ctx.ClearColor(0.0, 0.0, 0.0, 0.0);
        }
        ctx.Clear(
            gl::COLOR_BUFFER_BIT | gl::DEPTH_BUFFER_BIT | gl::STENCIL_BUFFER_BIT,
        );
    }
}

fn draw_verts_with_outline(
    ctx: &gl::Gl,
    start: isize,
    vert_count: gl::types::GLsizei,
    vertex_buffer: gl::types::GLuint,
    colour_shader: &ColourShader,
    frame_buffer: gl::types::GLuint,
) {
    unsafe {
        begin_using_frame_buffer(ctx, frame_buffer);

        ctx.UseProgram(colour_shader.program);

        ctx.BindBuffer(gl::ARRAY_BUFFER, vertex_buffer);
        ctx.EnableVertexAttribArray(colour_shader.pos_attr as _);
        ctx.VertexAttribPointer(
            colour_shader.pos_attr as _,
            2,
            gl::FLOAT,
            gl::FALSE as _,
            0,
            std::ptr::null().offset(start * std::mem::size_of::<f32>() as isize),
        );

        ctx.Clear(gl::STENCIL_BUFFER_BIT);

        ctx.Enable(gl::STENCIL_TEST);
        ctx.ColorMask(gl::FALSE, gl::FALSE, gl::FALSE, gl::FALSE);
        ctx.StencilOp(gl::INVERT, gl::INVERT, gl::INVERT);
        ctx.StencilFunc(gl::ALWAYS, 0x1, 0x1);

        ctx.Uniform4f(colour_shader.colour_uniform, 1.0, 1.0, 1.0, 1.0);

        ctx.DrawArrays(gl::TRIANGLE_FAN, 0, vert_count);

        ctx.ColorMask(gl::TRUE, gl::TRUE, gl::TRUE, gl::TRUE);
        ctx.Uniform4f(
            colour_shader.colour_uniform,
            32.0 / 255.0,
            32.0 / 255.0,
            63.0 / 255.0,
            1.0,
        );

        ctx.StencilOp(gl::ZERO, gl::ZERO, gl::ZERO);
        ctx.StencilFunc(gl::EQUAL, 1, 1);
        ctx.DrawArrays(gl::TRIANGLE_FAN, 0, vert_count);
        ctx.Disable(gl::STENCIL_TEST);

        //outline
        ctx.Uniform4f(
            colour_shader.colour_uniform,
            128.0 / 255.0,
            128.0 / 255.0,
            32.0 / 255.0,
            1.0,
        );
        ctx.DrawArrays(gl::LINE_STRIP, 0, vert_count);

        end_using_frame_buffer(ctx);
    }
}

//TODO can we pull a common sub-procedure out of this and draw_verts_with_outline?
fn draw_verts_with_texture(
    ctx: &gl::Gl,
    start: isize,
    vert_count: gl::types::GLsizei,
    vertex_buffer: gl::types::GLuint,
    texture_shader: &TextureShader,
    textures: &Textures,
    texture_x: gl::types::GLfloat,
    texture_y: gl::types::GLfloat,
    texture_w: gl::types::GLfloat,
    texture_h: gl::types::GLfloat,
    texture_index: gl::types::GLint,
    tint_r: gl::types::GLfloat,
    tint_g: gl::types::GLfloat,
    tint_b: gl::types::GLfloat,
    tint_a: gl::types::GLfloat,
    frame_buffer: gl::types::GLuint,
) {
    unsafe {
        ctx.BindFramebuffer(gl::FRAMEBUFFER, frame_buffer);

        ctx.UseProgram(texture_shader.program);

        ctx.BindBuffer(gl::ARRAY_BUFFER, vertex_buffer);
        ctx.EnableVertexAttribArray(texture_shader.pos_attr as _);
        ctx.VertexAttribPointer(
            texture_shader.pos_attr as _,
            2,
            gl::FLOAT,
            gl::FALSE as _,
            0,
            std::ptr::null().offset(start * std::mem::size_of::<f32>() as isize),
        );

        ctx.ActiveTexture(gl::TEXTURE0);
        ctx.BindTexture(gl::TEXTURE_2D, textures[0]);
        ctx.Uniform1i(texture_shader.texture_uniforms[0], 0);

        ctx.ActiveTexture(gl::TEXTURE1);
        ctx.BindTexture(gl::TEXTURE_2D, textures[1]);
        ctx.Uniform1i(texture_shader.texture_uniforms[1], 1);

        ctx.Uniform1i(texture_shader.texture_index_uniform, texture_index);

        ctx.Uniform4f(
            texture_shader.texture_xywh_uniform,
            texture_x,
            texture_y,
            texture_w,
            texture_h,
        );

        ctx.Uniform4f(texture_shader.tint_uniform, tint_r, tint_g, tint_b, tint_a);

        ctx.DrawArrays(gl::TRIANGLE_FAN, 0, vert_count);

        ctx.BindFramebuffer(gl::FRAMEBUFFER, 0);
    }
}

struct ColourShader {
    program: gl::types::GLuint,
    pos_attr: gl::types::GLsizei,
    matrix_uniform: gl::types::GLsizei,
    colour_uniform: gl::types::GLsizei,
}

static UNTEXTURED_VS_SRC: &'static str = "#version 120\n\
    attribute vec2 position;\n\
    uniform mat4 matrix;\n\
    void main() {\n\
    gl_Position = matrix * vec4(position, -1.0, 1.0);\n\
    }";

static UNTEXTURED_FS_SRC: &'static str = "#version 120\n\
    uniform vec4 colour;\n\
    void main() {\n\
       gl_FragColor = colour;\n\
    }";

struct TextureShader {
    program: gl::types::GLuint,
    pos_attr: gl::types::GLsizei,
    matrix_uniform: gl::types::GLsizei,
    texture_uniforms: [gl::types::GLsizei; 2],
    texture_xywh_uniform: gl::types::GLsizei,
    texture_index_uniform: gl::types::GLsizei,
    tint_uniform: gl::types::GLsizei,
}

//calculating the uvs here might be slower than passing them in.
//Then again, maybe this is faster because of better memory badwidth.
//We'll profile if it becomes a problem.
static TEXTURED_VS_SRC: &'static str = "#version 120\n\
    attribute vec2 position;\n\
    uniform mat4 matrix;\n\
    uniform vec4 texture_xywh;\n\
    varying vec2 texcoord;\n\
    void main() {\n\
        vec2 corner = vec2(clamp(position.x, -0.5, 0.5), position.y * -0.5) + vec2(0.5);
        texcoord = corner * texture_xywh.zw + texture_xywh.xy;
        gl_Position = matrix * vec4(position, -1.0, 1.0);\n\
    }";

static TEXTURED_FS_SRC: &'static str = "#version 120\n\
    uniform sampler2D textures[2];\n\
    uniform int texture_index;\n\
    uniform vec4 tint;\n\
    varying vec2 texcoord;\n\
    void main() {\n\
        vec4 tex;
        if (texture_index == 1) {
            tex = texture2D(textures[1], texcoord);\n\
        } else {
            tex = texture2D(textures[0], texcoord);\n\
        }

        gl_FragColor = tex + tint * tex.a;
    }";


//shader helper functions based on https://gist.github.com/simias/c140d1479ada4d6218c0
fn compile_shader(ctx: &gl::Gl, src: &str, shader_type: gl::types::GLenum) -> gl::types::GLuint {
    let shader;
    unsafe {
        shader = ctx.CreateShader(shader_type);
        // Attempt to compile the shader
        let c_str = CString::new(src.as_bytes()).unwrap();
        ctx.ShaderSource(shader, 1, &c_str.as_ptr(), std::ptr::null());
        ctx.CompileShader(shader);

        // Get the compile status
        let mut status = gl::FALSE as gl::types::GLint;
        ctx.GetShaderiv(shader, gl::COMPILE_STATUS, &mut status);

        // Fail on error
        if status != (gl::TRUE as gl::types::GLint) {
            let mut buffer = [0u8; 512];
            let mut length: i32 = 0;
            ctx.GetShaderInfoLog(
                shader,
                buffer.len() as i32,
                &mut length,
                buffer.as_mut_ptr() as *mut i8,
            );
            panic!(
                "Compiler log (length: {}):\n{}",
                length,
                std::str::from_utf8(
                    std::ffi::CStr::from_ptr(std::mem::transmute(&buffer)).to_bytes(),
                ).unwrap()
            );
        }
    }
    shader
}

fn link_program(ctx: &gl::Gl, vs: gl::types::GLuint, fs: gl::types::GLuint) -> gl::types::GLuint {
    unsafe {
        let program = ctx.CreateProgram();
        ctx.AttachShader(program, vs);
        ctx.AttachShader(program, fs);
        ctx.LinkProgram(program);
        // Get the link status
        let mut status = gl::FALSE as gl::types::GLint;
        ctx.GetProgramiv(program, gl::LINK_STATUS, &mut status);

        // Fail on error
        if status != (gl::TRUE as gl::types::GLint) {
            let mut buffer = [0u8; 512];
            let mut length: i32 = 0;
            ctx.GetProgramInfoLog(
                program,
                buffer.len() as i32,
                &mut length,
                buffer.as_mut_ptr() as *mut i8,
            );
            panic!(
                "Compiler log (length: {}):\n{}",
                length,
                std::str::from_utf8(
                    std::ffi::CStr::from_ptr(std::mem::transmute(&buffer)).to_bytes(),
                ).unwrap()
            );
        }
        program
    }
}

fn get_verts_and_ranges(mut vert_vecs: Vec<Vec<f32>>) -> (Vec<f32>, Ranges, usize) {
    let mut verts = Vec::new();
    let mut ranges = [(0, 0); 16];
    let mut used_len = 0;

    let mut start = 0;

    for mut vec in vert_vecs.iter_mut() {
        let end = start + vec.len() - 1;
        ranges[used_len] = (start as u16, end as u16);

        start = end + 1;

        verts.append(&mut vec);
        used_len += 1;
    }

    (verts, ranges, used_len)
}

fn make_texture_from_png(ctx: &gl::Gl, filename: &str) -> gl::types::GLuint {
    let mut texture = 0;

    if let Ok(image) = File::open(filename) {
        match image_decoding::decode_png(image) {
            (Ok((width, height)), Ok(colortype), Ok(pixels)) => {
                let (external_format, data_type) = match colortype {
                    image_decoding::ColorType::RGB(8) => (gl::RGB, gl::UNSIGNED_BYTE),
                    image_decoding::ColorType::RGB(16) => (gl::RGB, gl::UNSIGNED_SHORT),
                    image_decoding::ColorType::RGBA(8) => (gl::RGBA, gl::UNSIGNED_BYTE),
                    image_decoding::ColorType::RGBA(16) => (gl::RGBA, gl::UNSIGNED_SHORT),
                    _ => {
                        //TODO make this case more distinct
                        return 0;
                    }
                };

                unsafe {
                    ctx.GenTextures(1, &mut texture as _);

                    ctx.BindTexture(gl::TEXTURE_2D, texture);

                    ctx.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MIN_FILTER, gl::LINEAR as _);
                    ctx.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_MAG_FILTER, gl::LINEAR as _);
                    ctx.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_S, gl::CLAMP_TO_EDGE as _);
                    ctx.TexParameteri(gl::TEXTURE_2D, gl::TEXTURE_WRAP_T, gl::CLAMP_TO_EDGE as _);

                    ctx.TexImage2D(
                        gl::TEXTURE_2D,
                        0,
                        gl::RGBA8 as _,
                        width as _,
                        height as _,
                        0,
                        external_format,
                        data_type,
                        (match pixels {
                             image_decoding::DecodingResult::U8(v) => v.as_ptr() as _,
                             image_decoding::DecodingResult::U16(v) => v.as_ptr() as _,
                         }),
                    );
                }
            }
            _ => {
                return 0;
            }
        }
    }
    return texture;
}
