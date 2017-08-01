extern crate gl_generator;

use gl_generator::{Registry, Api, Profile, Fallbacks, StructGenerator};
use std::env;
use std::fs::File;
use std::path::Path;

fn main() {
    let dest = env::var("OUT_DIR").unwrap();
    let mut file = File::create(&Path::new(&dest).join("bindings.rs")).unwrap();

    Registry::new(
        Api::Gl,
        (2, 1),
        Profile::Core,
        Fallbacks::All,
        ["GL_ARB_framebuffer_object"],
    ).write_bindings(StructGenerator, &mut file)
        .unwrap();
}
