use super::{Cart, Mirroring};

#[derive(Clone)]
pub struct MMC3 {
    mirroring: Mirroring,
    prg_ram: [u8; 0x2000],
    n_prg_banks: u8,
    prg_rom: Vec<u8>,
    chr_rom: Vec<u8>,

    p_register: [u8; 8],
    prg_banks: [u8; 4],
    chr_banks: [u8; 8],

    target_register: u8,
    prg_bank_mode: bool,
    chr_inversion: bool,

    irq_enable: bool,
    irq_active: bool,
    irq_reload: u16,
    irq_counter: u16,
}

impl MMC3 {
    pub fn new(mirroring: Mirroring, prg_rom: Vec<u8>, chr_rom: Vec<u8>) -> Self {
        Self {
            mirroring,
            prg_ram: [0; 0x2000],
            n_prg_banks: (prg_rom.len() / 0x2000) as u8,
            prg_rom,
            chr_rom,

            p_register: [0; 8],
            prg_banks: [0; 4],
            chr_banks: [0; 8],

            target_register: 0,
            prg_bank_mode: false,
            chr_inversion: false,

            irq_enable: false,
            irq_active: false,
            irq_reload: 0,
            irq_counter: 0,
        }
    }

    fn prg_addr(&self, bank: u8, addr: u16) -> usize {
        self.prg_banks[bank as usize] as usize * 0x2000 + (addr as usize & 0x1FFF)
    }

    fn chr_addr(&self, bank: u8, addr: u16) -> usize {
        self.chr_banks[bank as usize] as usize * 0x400 + (addr as usize & 0x03FF)
    }
}

impl Cart for MMC3 {
    fn mirroring(&self) -> super::Mirroring {
        self.mirroring
    }

    fn cpu_read(&mut self, addr: u16) -> u8 {
        self.cpu_inspect(addr)
    }

    fn cpu_inspect(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x5FFF => 0,
            0x6000..=0x7FFF => self.prg_ram[addr as usize - 0x6000],
            0x8000..=0x9FFF => self.prg_rom[self.prg_addr(0, addr)],
            0xA000..=0xBFFF => self.prg_rom[self.prg_addr(1, addr)],
            0xC000..=0xDFFF => self.prg_rom[self.prg_addr(2, addr)],
            0xE000..=0xFFFF => self.prg_rom[self.prg_addr(3, addr)],
        }
    }

    fn cpu_write(&mut self, addr: u16, v: u8) {
        match addr {
            0x0000..=0x5FFF => {}
            0x6000..=0x7FFF => {
                self.prg_ram[addr as usize - 0x6000] = v;
            }
            0x8000..=0x9FFF => {
                if addr % 2 == 1 {
                    self.target_register = v & 0x7;
                    self.prg_bank_mode = v & 0x40 != 0;
                    self.chr_inversion = v & 0x80 != 0;
                } else {
                    self.p_register[self.target_register as usize] = v;

                    if self.chr_inversion {
                        self.chr_banks[0] = self.p_register[2];
                        self.chr_banks[1] = self.p_register[3];
                        self.chr_banks[2] = self.p_register[4];
                        self.chr_banks[3] = self.p_register[5];
                        self.chr_banks[4] = self.p_register[0] & 0xFE;
                        self.chr_banks[5] = self.p_register[0] + 1;
                        self.chr_banks[6] = self.p_register[1] & 0xFE;
                        self.chr_banks[7] = self.p_register[1] + 1;
                    } else {
                        self.chr_banks[0] = self.p_register[0] & 0xFE;
                        self.chr_banks[1] = self.p_register[0] + 1;
                        self.chr_banks[2] = self.p_register[1] & 0xFE;
                        self.chr_banks[3] = self.p_register[1] + 1;
                        self.chr_banks[4] = self.p_register[2];
                        self.chr_banks[5] = self.p_register[3];
                        self.chr_banks[6] = self.p_register[4];
                        self.chr_banks[7] = self.p_register[5];
                    }

                    if self.prg_bank_mode {
                        self.prg_banks[2] = self.p_register[6] & 0x3F;
                        self.prg_banks[0] = self.n_prg_banks * 2 - 2;
                    } else {
                        self.prg_banks[0] = self.p_register[6] & 0x3F;
                        self.prg_banks[2] = self.n_prg_banks * 2 - 2;
                    }

                    self.prg_banks[1] = self.p_register[7] & 0x3F;
                    self.prg_banks[3] = self.n_prg_banks * 2 - 1;
                }
            }
            0xA000..=0xBFFF => {
                if addr % 2 == 0 {
                    self.mirroring = if v % 2 == 0 {
                        Mirroring::Vertical
                    } else {
                        Mirroring::Horizontal
                    };
                } else {
                    // TODO: Ram protect
                }
            }
            0xC000..=0xDFFF => {
                if addr % 2 == 0 {
                    self.irq_reload = v as u16;
                } else {
                    self.irq_counter = 0;
                }
            }
            0xE000..=0xFFFF => {
                if addr % 2 == 0 {
                    self.irq_enable = false;
                    self.irq_active = false;
                } else {
                    self.irq_enable = true;
                }
            }
        }
    }

    fn ppu_read(&mut self, addr: u16) -> u8 {
        self.ppu_inspect(addr)
    }

    fn ppu_inspect(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x03FF => self.chr_rom[self.chr_addr(0, addr)],
            0x0400..=0x07FF => self.chr_rom[self.chr_addr(1, addr)],
            0x0800..=0x0BFF => self.chr_rom[self.chr_addr(2, addr)],
            0x0C00..=0x0FFF => self.chr_rom[self.chr_addr(3, addr)],
            0x1000..=0x13FF => self.chr_rom[self.chr_addr(4, addr)],
            0x1400..=0x17FF => self.chr_rom[self.chr_addr(5, addr)],
            0x1800..=0x1BFF => self.chr_rom[self.chr_addr(6, addr)],
            0x1C00..=0x1FFF => self.chr_rom[self.chr_addr(7, addr)],
            _ => unreachable!(),
        }
    }

    fn ppu_write(&mut self, addr: u16, v: u8) {
        let ppu_addr = match addr {
            0x0000..=0x03FF => self.chr_addr(0, addr),
            0x0400..=0x07FF => self.chr_addr(1, addr),
            0x0800..=0x0BFF => self.chr_addr(2, addr),
            0x0C00..=0x0FFF => self.chr_addr(3, addr),
            0x1000..=0x13FF => self.chr_addr(4, addr),
            0x1400..=0x17FF => self.chr_addr(5, addr),
            0x1800..=0x1BFF => self.chr_addr(6, addr),
            0x1C00..=0x1FFF => self.chr_addr(7, addr),
            _ => unreachable!(),
        };

        self.chr_rom[ppu_addr] = v;
    }

    fn reset(&mut self) {
        self.prg_banks = [0; 4];
        self.chr_banks = [0; 8];

        self.prg_banks[0] = 0;
        self.prg_banks[1] = 1;
        self.prg_banks[2] = self.n_prg_banks * 2 - 2;
        self.prg_banks[3] = self.n_prg_banks * 2 - 1;
    }

    fn box_cloned(&self) -> Box<dyn Cart> {
        Box::new(self.clone())
    }

    fn irq_state(&self) -> bool {
        self.irq_active
    }

    fn irq_clear(&mut self) {
        self.irq_active = false;
    }

    fn end_of_scanline(&mut self) {
        if self.irq_counter == 0 {
            self.irq_counter = self.irq_reload;
        } else {
            self.irq_counter -= 1;
        }

        if self.irq_counter == 0 && self.irq_enable {
            self.irq_active = true;
        }
    }
}
