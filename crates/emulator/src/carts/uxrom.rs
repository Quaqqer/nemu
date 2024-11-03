use super::{Cart, Mirroring};

#[derive(Clone)]
pub struct UxROM {
    mirroring: Mirroring,
    prg_rom: Vec<u8>,
    chr_rom: Vec<u8>,
    bank_lo: u8,
    bank_hi: u8,
}

impl UxROM {
    pub fn new(mirroring: Mirroring, prg_rom: Vec<u8>, chr_rom: Vec<u8>) -> Self {
        let bank_hi = ((prg_rom.len() / 0x4000) - 1) as u8;

        Self {
            mirroring,
            prg_rom,
            chr_rom,
            bank_lo: 0,
            bank_hi,
        }
    }
}

impl Cart for UxROM {
    fn mirroring(&self) -> Mirroring {
        self.mirroring
    }

    fn cpu_read(&mut self, addr: u16) -> u8 {
        self.cpu_inspect(addr)
    }

    fn cpu_inspect(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x7FFF => 0,
            0x8000..=0xBFFF => {
                self.prg_rom[self.bank_lo as usize * 0x4000 + (addr as usize % 0x4000)]
            }
            0xC000..=0xFFFF => {
                self.prg_rom[self.bank_hi as usize * 0x4000 + (addr as usize % 0x4000)]
            }
        }
    }

    fn cpu_write(&mut self, addr: u16, v: u8) {
        match addr {
            0x0000..=0x7FFF => {}
            0x8000..=0xFFFF => {
                self.bank_lo = v & 0x0F;
            }
        }
    }

    fn ppu_read(&mut self, addr: u16) -> u8 {
        let i = addr as usize % self.chr_rom.len();
        self.chr_rom[i]
    }

    fn ppu_write(&mut self, addr: u16, v: u8) {
        let i = addr as usize % self.chr_rom.len();
        self.chr_rom[i] = v;
    }

    fn reset(&mut self) {
        let bank_hi = ((self.prg_rom.len() / 0x4000) - 1) as u8;

        self.bank_lo = 0;
        self.bank_hi = bank_hi;
    }

    fn box_cloned(&self) -> Box<dyn Cart> {
        Box::new(self.clone())
    }
}
