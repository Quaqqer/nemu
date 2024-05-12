use crate::{apu::Apu, cart::Cart, ppu::Ppu};

pub struct Bus {
    pub cart: Cart,
    pub ppu: Ppu,
    pub apu: Apu,
}

impl Bus {
    pub fn new(cart: Cart) -> Self {
        Bus {
            ppu: Ppu::new(&cart),
            apu: Apu::new(),
            cart,
        }
    }

    pub fn reset(&mut self) {
        self.ppu.reset();
        self.apu.reset();
    }
}
