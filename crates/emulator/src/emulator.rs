use crate::{
    apu::Apu,
    carts::{generic_cart::GenericCart, Cart},
    config::NemuConfig,
    controller::NesController,
    cpu::Cpu,
    nes_cpu_bus::NesCpuBus,
    ppu::{Display, Ppu, PpuCtrl},
};

#[derive(Clone, bincode::Encode, bincode::Decode)]
pub struct Emulator {
    pub cpu: Cpu,
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
        self.cpu.reset();
        self.apu.reset();
        self.ppu.reset();
        self.cart.reset();
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

        // Perform NMI interrupt every frame
        let end_of_frame = ppu.nmi;
        if ppu.nmi && ppu.ppuctrl.intersects(PpuCtrl::NMI_ENABLE) {
            cpu.nmi_interrupt(&mut NesCpuBus {
                apu,
                ppu,
                cart,
                controllers,
                controller_shifters,
                ram,
            });
        }
        ppu.nmi = false;

        // Interrupt requests from carts
        if cart.irq_state() {
            cart.irq_clear();
            cpu.irq(&mut NesCpuBus {
                apu,
                ppu,
                cart,
                controllers,
                controller_shifters,
                ram,
            });
        }

        // Execute cpu instructions
        let cpu_cycles = cpu.tick(&mut NesCpuBus {
            apu,
            ppu,
            cart,
            controllers,
            controller_shifters,
            ram,
        });

        // Execute ppu instructions
        for _ in 0..cpu_cycles * 3 {
            ppu.cycle(cart, config);
        }

        end_of_frame
    }

    pub fn display(&self) -> &Display {
        self.ppu.display()
    }

    pub fn new(cart: GenericCart) -> Self {
        let mut emu = Self {
            cpu: Cpu::new(),
            apu: Apu::new(),
            ppu: Ppu::new(),
            cart,
            controllers: [NesController::empty(); 2],
            controller_shifters: [0x0; 2],
            ram: [0x00; 0x800],
        };

        let Emulator {
            cpu,
            apu,
            ppu,
            cart,
            controllers,
            controller_shifters,
            ram,
        } = &mut emu;

        cpu.init(&mut NesCpuBus {
            apu,
            ppu,
            cart,
            controllers,
            controller_shifters,
            ram,
        });

        emu
    }
}
