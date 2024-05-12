use crate::{cart::Cart, cpu::Cpu, ppu};

pub struct Emulator {
    pub cpu: Cpu,
}

impl Emulator {
    pub fn render_frame(&mut self) -> &ppu::Display {
        self.cpu.tick();
        self.cpu.bus.ppu.display()
    }

    pub fn new(cart: Cart) -> Self {
        Self {
            cpu: Cpu::new(cart),
        }
    }
}
