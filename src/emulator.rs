use crate::{
    apu::Apu,
    cart::Cart,
    cpu::{Cpu, CpuBus},
    ppu::{Display, Ppu, PpuCtrl},
};

pub struct Emulator {
    pub cpu: Cpu,
    pub apu: Apu,
    pub ppu: Ppu,
    pub cart: Cart,
}

impl Emulator {
    pub fn step_frame(&mut self) {
        while !self.step() {}
    }

    pub fn step_scanline(&mut self) {
        let start = self.ppu.scanline;

        while self.ppu.scanline == start {
            self.step();
        }
    }

    pub fn reset(&mut self) {
        self.cpu.reset();
        self.apu.reset();
        self.ppu.reset();
        self.cart.reset();
    }

    pub fn step(&mut self) -> bool {
        let Emulator {
            cpu,
            apu,
            ppu,
            cart,
        } = self;

        let did_nmi = ppu.nmi;
        if ppu.nmi && ppu.ppuctrl.intersects(PpuCtrl::NMI_ENABLE) {
            cpu.nmi_interrupt(&mut CpuBus { apu, ppu, cart });
        }
        ppu.nmi = false;

        let cpu_cycles = cpu.tick(&mut CpuBus { apu, ppu, cart });

        for _ in 0..cpu_cycles * 3 {
            ppu.cycle(cart);
        }
        did_nmi
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
