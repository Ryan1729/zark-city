extern crate image;
use image::ImageDecoder;

pub use image::ColorType;
pub use image::ImageResult;
pub use image::DecodingResult;

use std::fs::File;

pub fn decode_png(
    image: File,
) -> (ImageResult<(u32, u32)>, ImageResult<ColorType>, ImageResult<DecodingResult>) {
    let mut decoder = image::png::PNGDecoder::new(image);

    (
        decoder.dimensions(),
        decoder.colortype(),
        decoder.read_image(),
    )
}
