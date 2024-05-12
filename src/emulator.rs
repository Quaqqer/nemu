use crate::{
    apu::Apu,
    cart::Cart,
    cpu::{Cpu, CpuBus},
    ppu::{self, Ppu},
};

pub struct Emulator {
    pub cpu: Cpu,
    pub apu: Apu,
    pub ppu: Ppu,
    pub cart: Cart,
}

impl Emulator {
    pub fn render_frame(&mut self) -> &ppu::Display {
        let Emulator {
            cpu,
            apu,
            ppu,
            cart,
        } = self;

        let cpu_cycles = self.cpu.tick(&mut CpuBus { apu, ppu, cart });

        for _ in 0..cpu_cycles * 3 {
            self.ppu.cycle(cart);
        }

        self.ppu.display()
    }

    pub fn reset(&mut self) {
        self.cpu.reset();
        self.apu.reset();
        self.ppu.reset();
        self.cart.reset();
    }

    pub fn new(cart: Cart) -> Self {
        let mut emu = Self {
            cpu: Cpu::new(),
            apu: Apu::new(),
            ppu: Ppu::new(),
            cart,
        };

        let Emulator {
            cpu,
            apu,
            ppu,
            cart,
        } = &mut emu;

        cpu.init(&mut CpuBus { apu, ppu, cart });

        emu
    }
}
