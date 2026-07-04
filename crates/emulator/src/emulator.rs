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

    fn cpu_and_bus<'a>(&'a mut self) -> (&'a mut Cpu, NesCpuBus<'a>) {
        let Emulator {
            cpu,
            apu,
            ppu,
            cart,
            controllers,
            controller_shifters,
            ram,
        } = self;

        (
            cpu,
            NesCpuBus {
                ram,
                apu,
                ppu,
                cart,
                controllers,
                controller_shifters,
            },
        )
    }

    pub fn reset(&mut self) {
        let (cpu, mut bus) = self.cpu_and_bus();
        cpu.reset(&mut bus);
        self.apu.reset();
        self.ppu.reset();
        self.cart.reset();
    }

    #[inline]
    pub fn tick_cpu(&mut self) -> u64 {
        let (cpu, mut bus) = self.cpu_and_bus();
        cpu.tick(&mut bus)
    }

    #[inline]
    pub fn tick_apu(&mut self) {
        self.apu.tick();
    }

    #[inline]
    pub fn sample_apu(&self) -> f32 {
        self.apu.sample()
    }

    #[inline]
    pub fn tick_ppu(&mut self, config: &NemuConfig) -> bool {
        let nmi = self.ppu.nmi;

        if nmi && self.ppu.ppuctrl.intersects(PpuCtrl::NMI_ENABLE) {
            let (cpu, mut bus) = self.cpu_and_bus();
            cpu.nmi_interrupt(&mut bus);
        }
        self.ppu.nmi = false;

        if self.cart.irq_state() {
            self.cart.irq_clear();
            let (cpu, mut bus) = self.cpu_and_bus();
            cpu.irq(&mut bus);
        }

        self.ppu.tick(&mut self.cart, config);

        nmi
    }

    pub fn step(&mut self, config: &NemuConfig) -> bool {
        // Perform NMI interrupt every frame
        let end_of_frame = self.ppu.nmi;
        if self.ppu.nmi && self.ppu.ppuctrl.intersects(PpuCtrl::NMI_ENABLE) {
            let (cpu, mut bus) = self.cpu_and_bus();
            cpu.nmi_interrupt(&mut bus);
        }
        self.ppu.nmi = false;

        // Interrupt requests from carts
        if self.cart.irq_state() {
            self.cart.irq_clear();
            let (cpu, mut bus) = self.cpu_and_bus();
            cpu.irq(&mut bus);
        }

        // Execute cpu instructions
        let (cpu, mut bus) = self.cpu_and_bus();
        let cpu_cycles = cpu.tick(&mut bus);

        // Execute ppu instructions
        for _ in 0..cpu_cycles * 3 {
            self.ppu.tick(&mut self.cart, config);
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

        let (cpu, mut bus) = emu.cpu_and_bus();
        cpu.init(&mut bus);

        emu
    }
}
