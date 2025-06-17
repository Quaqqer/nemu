use libretro_rs::{
    RetroAudioInfo, RetroCore, RetroJoypadButton, RetroLoadGameResult, RetroVideoInfo,
    libretro_core,
};
use nemu_emulator::{
    carts::reader::read_rom, config::NemuConfig, controller::NesController, emulator::Emulator,
};

struct NemuCore {
    config: NemuConfig,
    emu: Option<Emulator>,
}

fn update_inputs(emulator: &mut Emulator, runtime: &libretro_rs::RetroRuntime) {
    const MAPPINGS: [(NesController, RetroJoypadButton); 8] = [
        (NesController::RIGHT, RetroJoypadButton::Right),
        (NesController::LEFT, RetroJoypadButton::Left),
        (NesController::DOWN, RetroJoypadButton::Down),
        (NesController::UP, RetroJoypadButton::Up),
        (NesController::START, RetroJoypadButton::Start),
        (NesController::SELECT, RetroJoypadButton::Select),
        (NesController::B, RetroJoypadButton::B),
        (NesController::A, RetroJoypadButton::A),
    ];

    let mut p1 = NesController::from_bits_truncate(0);
    let mut p2 = NesController::from_bits_truncate(0);

    for (player, port) in [(&mut p1, 0), (&mut p2, 1)] {
        for (nes_btn, libretro_btn) in MAPPINGS {
            let pressed = runtime.is_joypad_button_pressed(port, libretro_btn);
            player.set(nes_btn, pressed);
        }
    }

    emulator.controllers = [p1, p2];
}

impl RetroCore for NemuCore {
    fn init(_env: &libretro_rs::RetroEnvironment) -> Self {
        Self {
            config: NemuConfig::default(),
            emu: None,
        }
    }

    fn get_system_info() -> libretro_rs::RetroSystemInfo {
        libretro_rs::RetroSystemInfo::new("Nemu", env!("CARGO_PKG_VERSION"))
            .with_valid_extensions(&["nes"])
    }

    fn reset(&mut self, _env: &libretro_rs::RetroEnvironment) {
        if let Some(emu) = self.emu.as_mut() {
            emu.reset();
        }
    }

    fn run(&mut self, _env: &libretro_rs::RetroEnvironment, runtime: &libretro_rs::RetroRuntime) {
        if let Some(emu) = self.emu.as_mut() {
            update_inputs(emu, runtime);

            emu.step_frame(&self.config);

            let framebuffer_rgb = emu.display().pixels;
            let mut framebuffer_bgra: [u8; 4 * 256 * 240] = [0; 4 * 256 * 240];
            for i in 0..framebuffer_rgb.len() / 3 {
                let r = framebuffer_rgb[i * 3];
                let g = framebuffer_rgb[i * 3 + 1];
                let b = framebuffer_rgb[i * 3 + 2];

                framebuffer_bgra[i * 4] = b;
                framebuffer_bgra[i * 4 + 1] = g;
                framebuffer_bgra[i * 4 + 2] = r;
            }

            runtime.upload_video_frame(&framebuffer_bgra, 256, 240, 0);
        }
    }

    fn load_game(
        &mut self,
        _env: &libretro_rs::RetroEnvironment,
        game: libretro_rs::RetroGame,
    ) -> libretro_rs::RetroLoadGameResult {
        let cart = match game {
            libretro_rs::RetroGame::None { meta: _ } => return RetroLoadGameResult::Failure,
            libretro_rs::RetroGame::Path { meta: _, path } => match std::fs::read(path) {
                Ok(data) => read_rom(&data[..]),
                Err(_) => return RetroLoadGameResult::Failure,
            },
            libretro_rs::RetroGame::Data { meta: _, data } => read_rom(data),
        };

        let cart = match cart {
            Ok(v) => v,
            Err(_) => return RetroLoadGameResult::Failure,
        };

        self.emu = Some(Emulator::new(cart));

        RetroLoadGameResult::Success {
            audio: RetroAudioInfo::new(1.79e9),
            video: RetroVideoInfo::new(60., 256, 240)
                .with_pixel_format(libretro_rs::RetroPixelFormat::XRGB8888),
        }
    }
}

libretro_core!(NemuCore);
