pub mod generic_cart;
pub mod mappers;
pub mod reader;

#[derive(Clone, Copy, PartialEq, Eq, Debug, bincode::Encode, bincode::Decode)]
pub enum Mirroring {
    Horizontal,
    Vertical,
    Single,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TvSystem {
    NTSC,
    PAL,
    Dual,
}

pub trait Cart {
    fn mirroring(&self) -> Mirroring;
    fn cpu_read(&mut self, addr: u16) -> u8;
    fn cpu_inspect(&self, addr: u16) -> u8;
    fn cpu_write(&mut self, addr: u16, v: u8);
    fn ppu_read(&mut self, addr: u16) -> u8;
    fn ppu_inspect(&self, addr: u16) -> u8;
    fn ppu_write(&mut self, addr: u16, v: u8);
    fn reset(&mut self);
    fn box_cloned(&self) -> Box<dyn Cart>;
    fn irq_state(&self) -> bool;
    fn irq_clear(&mut self);
    fn end_of_scanline(&mut self);
}
