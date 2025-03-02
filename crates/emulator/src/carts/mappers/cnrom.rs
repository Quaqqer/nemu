use crate::carts::{Cart, Mirroring};

#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, bincode::Encode, bincode::Decode)]
pub struct CNROM {
    mirroring: Mirroring,
    prg_rom: Vec<u8>,
    chr_rom: Vec<u8>,
    bank: u8,
}

impl CNROM {
    pub fn new(mirroring: Mirroring, prg_rom: Vec<u8>, chr_rom: Vec<u8>) -> Self {
        Self {
            mirroring,
            prg_rom,
            chr_rom,
            bank: 0,
        }
    }
}

impl Cart for CNROM {
    fn mirroring(&self) -> Mirroring {
        self.mirroring
    }

    fn cpu_read(&mut self, addr: u16) -> u8 {
        self.cpu_inspect(addr)
    }

    fn cpu_inspect(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x7FFF => 0,
            0x8000..=0xFFFF => self.prg_rom[(addr as usize - 0x8000) % self.prg_rom.len()],
        }
    }

    fn cpu_write(&mut self, addr: u16, v: u8) {
        match addr {
            0x0000..=0x7FFF => {}
            0x8000..=0xFFFF => {
                self.bank = v & 0x03;
            }
        }
    }

    fn ppu_read(&mut self, addr: u16) -> u8 {
        self.ppu_inspect(addr)
    }

    fn ppu_inspect(&self, addr: u16) -> u8 {
        let i = self.bank as usize * 0x2000 + addr as usize % self.chr_rom.len();
        self.chr_rom[i]
    }

    fn ppu_write(&mut self, addr: u16, v: u8) {
        let i = self.bank as usize * 0x2000 + addr as usize % self.chr_rom.len();
        self.chr_rom[i] = v;
    }

    fn reset(&mut self) {
        self.bank = 0;
    }

    fn box_cloned(&self) -> Box<dyn Cart> {
        Box::new(self.clone())
    }

    fn irq_state(&self) -> bool {
        false
    }

    fn irq_clear(&mut self) {}

    fn end_of_scanline(&mut self) {}
}
