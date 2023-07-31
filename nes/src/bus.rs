use crate::{ppu::Ppu, cart::Cart, apu::Apu};

pub struct Bus {
    pub cart: Cart,
    pub ppu: Ppu,
    pub apu: Apu,
}

impl Bus {
    pub fn new(cart: Cart) -> Self {
        Bus {
            cart,
            ppu: Ppu::new(),
            apu: Apu::new(),
        }
    }

    pub fn reset(&mut self) {
        self.ppu.reset();
        self.apu.reset();
    }
}
