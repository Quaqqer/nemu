use super::{Cart, Mirroring};

#[derive(Clone)]
pub struct NROM {
    pub mirroring: Mirroring,
    pub prg_rom: Vec<u8>,
    pub prg_ram: Vec<u8>,
    pub chr_rom: Vec<u8>,
}

impl Cart for NROM {
    fn mirroring(&self) -> Mirroring {
        self.mirroring
    }

    fn cpu_read(&mut self, addr: u16) -> u8 {
        self.cpu_inspect(addr)
    }

    fn cpu_inspect(&self, addr: u16) -> u8 {
        match addr {
            ..0x6000 => 0,
            0x6000..0x8000 => self.prg_ram[(addr as usize - 0x6000) % self.prg_ram.len()],
            0x8000.. => self.prg_rom[(addr as usize - 0x8000) % self.prg_rom.len()],
        }
    }

    fn cpu_write(&mut self, addr: u16, v: u8) {
        match addr {
            ..0x6000 => {}
            0x6000..0x8000 => {
                let i = (addr as usize - 0x6000) % self.prg_ram.len();
                self.prg_ram[i] = v
            }
            0x8000.. => {}
        }
    }

    fn ppu_read(&mut self, addr: u16) -> u8 {
        if self.chr_rom.is_empty() {
            return 0;
        }

        let i = addr as usize % self.chr_rom.len();
        self.chr_rom[i]
    }

    fn ppu_write(&mut self, _addr: u16, _v: u8) {}

    fn reset(&mut self) {
        todo!()
    }

    fn box_cloned(&self) -> Box<dyn Cart> {
        Box::new(self.clone())
    }
}
