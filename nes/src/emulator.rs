use crate::{bus::Bus, cart::Cart, cpu::Cpu};

struct Emulator {
    cpu: Cpu,
    bus: Bus,
}

impl<'a> Emulator {
    pub fn new(cart: Cart) -> Self {
        Self {
            cpu: Cpu::new(),
            bus: Bus::new(cart),
        }
    }

    pub fn reset(&mut self) {
        self.cpu.reset();
        self.bus.reset();
    }
}
