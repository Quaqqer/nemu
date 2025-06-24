use std::time::Duration;

use chips::Ricoh6502;

use crate::{
    apu::Apu,
    carts::{generic_cart::GenericCart, Cart},
    config::NemuConfig,
    controller::NesController,
    cpu::{self, CpuMemory},
    nes_cpu_bus::NesCpuBus,
    ppu::{Display, Ppu, PpuCtrl},
};

#[derive(Clone)]
pub struct Emulator {
    pub cpu: Ricoh6502,
    pub apu: Apu,
    pub ppu: Ppu,
    pub cart: GenericCart,
    pub controllers: [NesController; 2],
    pub controller_shifters: [u8; 2],
    pub ram: [u8; 0x800],
}

impl Emulator {
    pub fn step_frame(&mut self, config: &NemuConfig) {
        while !self.step(config) {}
    }

    pub fn step_scanline(&mut self, config: &NemuConfig) {
        let start = self.ppu.scanline;

        while self.ppu.scanline == start {
            self.step(config);
        }
    }

    pub fn reset(&mut self) {
        // self.cpu.reset();
        self.apu.reset();
        self.ppu.reset();
        self.cart.reset();
        todo!();
    }

    pub fn step(&mut self, config: &NemuConfig) -> bool {
        let Emulator {
            cpu,
            apu,
            ppu,
            cart,
            controllers,
            controller_shifters,
            ram,
        } = self;

        cpu.set_nmi(false);
        cpu.set_irq(false);

        // Perform NMI interrupt every frame
        let end_of_frame = ppu.nmi;
        if ppu.nmi && ppu.ppuctrl.intersects(PpuCtrl::NMI_ENABLE) {
            cpu.set_nmi(true);
        }
        ppu.nmi = false;

        // Interrupt requests from carts
        if cart.irq_state() {
            cpu.set_irq(true);
        }

        // Execute cpu instructions
        let mut cycles = 0;
        let addr = cpu.address_bus();
        {
            let mut bus = NesCpuBus {
                ram,
                apu,
                ppu,
                cart,
                controllers,
                controller_shifters,
            };
            if cpu.is_reading() {
                // println!("Read {}", addr);
                cpu.set_data_bus(bus.read(addr));
            } else {
                // println!("Write {} to {}", cpu.data_bus(), addr);
                if addr == 0x4014 {
                    cycles += 513;
                }
                bus.write(addr, cpu.data_bus());
            }
        }

        cpu.step();
        cycles += 1;

        // Execute ppu instructions
        for _ in 0..cycles * 3 {
            ppu.cycle(cart, config);
        }

        end_of_frame
    }

    pub fn display(&self) -> &Display {
        self.ppu.display()
    }

    pub fn new(cart: GenericCart) -> Self {
        Self {
            cpu: Ricoh6502::new(),
            apu: Apu::new(),
            ppu: Ppu::new(),
            cart,
            controllers: [NesController::empty(); 2],
            controller_shifters: [0x0; 2],
            ram: [0x00; 0x800],
        }
    }
}
