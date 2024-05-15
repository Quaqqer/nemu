use crate::{
    apu::Apu,
    cart::Cart,
    cpu::{Cpu, CpuBus},
    ppu::{Display, Ppu},
};

pub struct Emulator {
    pub cpu: Cpu,
    pub apu: Apu,
    pub ppu: Ppu,
    pub cart: Cart,
}

impl Emulator {
    pub fn step_frame(&mut self) {
        while !self.ppu.frame_end {
            self.step();
        }
        self.ppu.frame_end = false;
    }

    pub fn reset(&mut self) {
        self.cpu.reset();
        self.apu.reset();
        self.ppu.reset();
        self.cart.reset();
    }

    pub fn step(&mut self) {
        let Emulator {
            cpu,
            apu,
            ppu,
            cart,
        } = self;

        if ppu.nmi {
            cpu.nmi_interrupt(&mut CpuBus { apu, ppu, cart });
        }
        ppu.nmi = false;

        let cpu_cycles = cpu.tick(&mut CpuBus { apu, ppu, cart });

        for _ in 0..cpu_cycles * 3 {
            ppu.cycle(cart);
        }
    }

    pub fn display(&self) -> &Display {
        self.ppu.display()
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
