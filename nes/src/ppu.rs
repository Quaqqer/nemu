pub struct Ppu {}

impl Ppu {
    pub fn new() -> Self {
        Ppu {}
    }

    pub fn read_register(&self, addr: u16) -> u8 {
        match (addr - 0x2000) % 0x8 {
            _ => 0,
        }
    }

    pub fn write_register(&self, addr: u16, v: u8) {
        match (addr - 0x2000) % 0x8 {
            _ => {}
        }
    }

    pub fn reset(&mut self) {}
}
