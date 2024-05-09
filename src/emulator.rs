use crate::{cart::Cart, cpu::Cpu, ppu};

pub struct Emulator {
    cpu: Cpu,
}

impl Emulator {
    pub fn render_frame(&mut self) -> &ppu::Display {
        self.cpu.frame()
    }

    pub fn new(cart: Cart) -> Self {
        Self {
            cpu: Cpu::new(cart),
        }
    }
}
