use super::{
    mappers::{cnrom::CNROM, mmc1::MMC1, mmc3::MMC3, nrom::NROM, uxrom::UxROM},
    Cart, Mirroring,
};

#[allow(clippy::large_enum_variant)]
#[derive(Clone, bincode::Encode, bincode::Decode)]
pub enum GenericCart {
    CNROM(CNROM),
    MMC1(MMC1),
    MMC3(MMC3),
    NROM(NROM),
    UxROM(UxROM),
}

#[rustfmt::skip]
impl Cart for GenericCart {
    fn mirroring(&self) -> Mirroring {
        match self {
            GenericCart::CNROM(rom) => Cart::mirroring(rom),
            GenericCart::MMC1(rom)  => Cart::mirroring(rom),
            GenericCart::MMC3(rom)  => Cart::mirroring(rom),
            GenericCart::NROM(rom)  => Cart::mirroring(rom),
            GenericCart::UxROM(rom) => Cart::mirroring(rom),
        }
    }

    fn cpu_read(&mut self, addr: u16) -> u8 {
        match self {
            GenericCart::CNROM(rom) => Cart::cpu_read(rom, addr),
            GenericCart::MMC1(rom)  => Cart::cpu_read(rom, addr),
            GenericCart::MMC3(rom)  => Cart::cpu_read(rom, addr),
            GenericCart::NROM(rom)  => Cart::cpu_read(rom, addr),
            GenericCart::UxROM(rom) => Cart::cpu_read(rom, addr),
        }
    }

    fn cpu_inspect(&self, addr: u16) -> u8 {
        match self {
            GenericCart::CNROM(rom) => Cart::cpu_inspect(rom, addr),
            GenericCart::MMC1(rom)  => Cart::cpu_inspect(rom, addr),
            GenericCart::MMC3(rom)  => Cart::cpu_inspect(rom, addr),
            GenericCart::NROM(rom)  => Cart::cpu_inspect(rom, addr),
            GenericCart::UxROM(rom) => Cart::cpu_inspect(rom, addr),
        }
    }

    fn cpu_write(&mut self, addr: u16, v: u8) {
        match self {
            GenericCart::CNROM(rom) => Cart::cpu_write(rom, addr, v),
            GenericCart::MMC1(rom)  => Cart::cpu_write(rom, addr, v),
            GenericCart::MMC3(rom)  => Cart::cpu_write(rom, addr, v),
            GenericCart::NROM(rom)  => Cart::cpu_write(rom, addr, v),
            GenericCart::UxROM(rom) => Cart::cpu_write(rom, addr, v),
        }
    }

    fn ppu_read(&mut self, addr: u16) -> u8 {
        match self {
            GenericCart::CNROM(rom) => Cart::ppu_read(rom, addr),
            GenericCart::MMC1(rom)  => Cart::ppu_read(rom, addr),
            GenericCart::MMC3(rom)  => Cart::ppu_read(rom, addr),
            GenericCart::NROM(rom)  => Cart::ppu_read(rom, addr),
            GenericCart::UxROM(rom) => Cart::ppu_read(rom, addr),
        }
    }

    fn ppu_inspect(&self, addr: u16) -> u8 {
        match self {
            GenericCart::CNROM(rom) => Cart::ppu_inspect(rom, addr),
            GenericCart::MMC1(rom)  => Cart::ppu_inspect(rom, addr),
            GenericCart::MMC3(rom)  => Cart::ppu_inspect(rom, addr),
            GenericCart::NROM(rom)  => Cart::ppu_inspect(rom, addr),
            GenericCart::UxROM(rom) => Cart::ppu_inspect(rom, addr),
        }
    }

    fn ppu_write(&mut self, addr: u16, v: u8) {
        match self {
            GenericCart::CNROM(rom) => Cart::ppu_write(rom, addr, v),
            GenericCart::MMC1(rom)  => Cart::ppu_write(rom, addr, v),
            GenericCart::MMC3(rom)  => Cart::ppu_write(rom, addr, v),
            GenericCart::NROM(rom)  => Cart::ppu_write(rom, addr, v),
            GenericCart::UxROM(rom) => Cart::ppu_write(rom, addr, v),
        }
    }

    fn reset(&mut self) {
        match self {
            GenericCart::CNROM(rom) => Cart::reset(rom),
            GenericCart::MMC1(rom)  => Cart::reset(rom),
            GenericCart::MMC3(rom)  => Cart::reset(rom),
            GenericCart::NROM(rom)  => Cart::reset(rom),
            GenericCart::UxROM(rom) => Cart::reset(rom),
        }
    }

    fn box_cloned(&self) -> Box<dyn Cart> {
        match self {
            GenericCart::CNROM(rom) => Cart::box_cloned(rom),
            GenericCart::MMC1(rom)  => Cart::box_cloned(rom),
            GenericCart::MMC3(rom)  => Cart::box_cloned(rom),
            GenericCart::NROM(rom)  => Cart::box_cloned(rom),
            GenericCart::UxROM(rom) => Cart::box_cloned(rom),
        }
    }

    fn irq_state(&self) -> bool {
        match self {
            GenericCart::CNROM(rom) => Cart::irq_state(rom),
            GenericCart::MMC1(rom)  => Cart::irq_state(rom),
            GenericCart::MMC3(rom)  => Cart::irq_state(rom),
            GenericCart::NROM(rom)  => Cart::irq_state(rom),
            GenericCart::UxROM(rom) => Cart::irq_state(rom),
        }
    }

    fn irq_clear(&mut self) {
        match self {
            GenericCart::CNROM(rom) => Cart::irq_clear(rom),
            GenericCart::MMC1(rom)  => Cart::irq_clear(rom),
            GenericCart::MMC3(rom)  => Cart::irq_clear(rom),
            GenericCart::NROM(rom)  => Cart::irq_clear(rom),
            GenericCart::UxROM(rom) => Cart::irq_clear(rom),
        }
    }

    fn end_of_scanline(&mut self) {
        match self {
            GenericCart::CNROM(rom) => Cart::end_of_scanline(rom),
            GenericCart::MMC1(rom)  => Cart::end_of_scanline(rom),
            GenericCart::MMC3(rom)  => Cart::end_of_scanline(rom),
            GenericCart::NROM(rom)  => Cart::end_of_scanline(rom),
            GenericCart::UxROM(rom) => Cart::end_of_scanline(rom),
        }
    }
}
