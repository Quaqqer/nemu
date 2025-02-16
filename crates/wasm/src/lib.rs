// For some reason everything is considered unused when using wasm_bindgen, maybe because of the
// target?
#![allow(unused)]

use nemu_emulator::{
    carts::reader::read_rom, config::NemuConfig, controller::NesController, emulator::Emulator, ppu,
};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
struct Nemu {
    emulator: Emulator,
    config: NemuConfig,
}

#[wasm_bindgen]
impl Nemu {
    pub fn new(bin: &[u8]) -> Result<Nemu, String> {
        let cart = read_rom(bin).map_err(|e| e.to_string())?;
        let config = NemuConfig::default();

        Ok(Self {
            emulator: Emulator::new(cart),
            config,
        })
    }

    pub fn next_frame(&mut self) -> Vec<u8> {
        self.emulator.step_frame(&self.config);
        let disp = self.emulator.ppu.display();

        let mut buf = Vec::new();
        buf.resize(ppu::Display::WIDTH * ppu::Display::HEIGHT * 4, 0xff);

        for i in 0..ppu::Display::WIDTH * ppu::Display::HEIGHT {
            buf[i * 4] = disp.pixels[i * 3];
            buf[i * 4 + 1] = disp.pixels[i * 3 + 1];
            buf[i * 4 + 2] = disp.pixels[i * 3 + 2];
        }

        buf
    }

    pub fn update_controller(&mut self, controller: &Controller) {
        let mut nes_controller = NesController::empty();

        if controller.dpad_n {
            nes_controller |= NesController::UP;
        }

        if controller.dpad_s {
            nes_controller |= NesController::DOWN;
        }

        if controller.dpad_w {
            nes_controller |= NesController::LEFT;
        }

        if controller.dpad_e {
            nes_controller |= NesController::RIGHT;
        }

        if controller.start {
            nes_controller |= NesController::START;
        }

        if controller.select {
            nes_controller |= NesController::SELECT;
        }

        if controller.a {
            nes_controller |= NesController::A;
        }

        if controller.b {
            nes_controller |= NesController::B;
        }

        self.emulator.controllers[0] = nes_controller;
    }
}

#[wasm_bindgen]
#[derive(Clone, Copy)]
struct Controller {
    pub dpad_n: bool,
    pub dpad_s: bool,
    pub dpad_w: bool,
    pub dpad_e: bool,

    pub start: bool,
    pub select: bool,

    pub a: bool,
    pub b: bool,
}

#[wasm_bindgen]
impl Controller {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Controller {
        Controller {
            dpad_n: false,
            dpad_s: false,
            dpad_w: false,
            dpad_e: false,
            start: false,
            select: false,
            a: false,
            b: false,
        }
    }
}

#[wasm_bindgen]
pub fn set_panic_hook() {
    console_error_panic_hook::set_once();
}
