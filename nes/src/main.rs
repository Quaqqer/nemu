use clap::Parser;
use macroquad::prelude::*;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    nes_rom: String,
}

#[macroquad::main(window_conf)]
async fn main() {
    let args = Args::parse();

    let mut emu = nes::emulator::Emulator::new(nes::cart::Cart::read_ines1_0(&args.nes_rom));
    let disp = emu.render_frame();

    loop {
        clear_background(BLACK);

        draw_line(40.0, 40.0, 100.0, 200.0, 15.0, BLUE);
        draw_rectangle(screen_width() / 2.0 - 60.0, 100.0, 120.0, 60.0, GREEN);
        draw_circle(screen_width() - 30.0, screen_height() - 30.0, 15.0, YELLOW);

        draw_text("IT WORKS!", 20.0, 20.0, 30.0, DARKGRAY);

        next_frame().await
    }
}

fn window_conf() -> macroquad::window::Conf {
    macroquad::window::Conf {
        window_title: "Nemu".to_string(),
        window_width: 341,
        window_height: 262,
        platform: miniquad::conf::Platform {
            linux_backend: miniquad::conf::LinuxBackend::WaylandWithX11Fallback,
            ..Default::default()
        },
        ..Default::default()
    }
}
