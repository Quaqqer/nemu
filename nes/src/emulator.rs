use crate::{cart::Cart, cpu::Cpu};

struct Emulator {
    cpu: Cpu,
}

impl<'a> Emulator {
    pub fn headless_frame(&mut self) {
        self.cpu.tick();
    }

    pub fn render_frame(&mut self) -> &'a [u8] {
        self.headless_frame();
        todo!()
    }

    pub fn new(cart: Cart) -> Self {
        Self {
            cpu: Cpu::new(cart),
        }
    }
}
