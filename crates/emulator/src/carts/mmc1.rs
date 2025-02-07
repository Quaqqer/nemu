use bitfield_struct::bitfield;

use super::{Cart, Mirroring};

#[derive(Clone)]
pub struct MMC1 {
    prg_rom: Vec<u8>,
    chr_rom: Vec<u8>,
    prg_ram: Vec<u8>,

    n_load: u8,

    reg_load: u8,
    reg_control: MMC1Control,
    reg_chr_bank0: u8,
    reg_chr_bank1: u8,
    reg_prg_bank: u8,
}

#[bitfield(u8)]
struct MMC1Control {
    #[bits(2)]
    /// The arrangement of nametables
    ///
    /// 0: one screen, lower bank
    /// 1: one screen, upper bank
    /// 2: vertical mirroring
    /// 3: horizontal mirroring
    nametable_arrangement: u8,
    #[bits(2)]
    /// The prg rom bank mode
    ///
    /// 0 / 1: switch 32 kb at 0x8000, ignore low bit of bank number
    /// 2: 0x8000 fixed to first bank, 0xC000 switched
    /// 3: 0x8000 switched, 0xC000 fixed to last bank
    prg_rom_bank_mode: u8,
    /// The chr rom bank mode
    ///
    /// 0: switch 8 kb at a time
    /// 1: switch 2 separate 4 kb banks
    chr_rom_bank_mode: bool,
    #[bits(3)]
    _unused: u8,
}

impl MMC1 {
    pub fn new(prg_rom: Vec<u8>, chr_rom: Vec<u8>) -> Self {
        Self {
            prg_rom,
            chr_rom,
            prg_ram: vec![0x00; 0x2000],

            n_load: 0,

            reg_load: 0,
            reg_control: MMC1Control::from(0x1c),
            reg_chr_bank0: 0,
            reg_chr_bank1: 0,
            reg_prg_bank: 0,
        }
    }
}

impl Cart for MMC1 {
    fn mirroring(&self) -> Mirroring {
        match self.reg_control.nametable_arrangement() {
            0 | 1 => Mirroring::Single,
            2 => Mirroring::Vertical,
            3 => Mirroring::Horizontal,
            _ => unreachable!(),
        }
    }

    fn cpu_read(&mut self, addr: u16) -> u8 {
        self.cpu_inspect(addr)
    }

    fn cpu_inspect(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x5FFF => 0,
            0x6000..=0x7FFF => self.prg_ram[addr as usize - 0x6000],
            0x8000..=0xBFFF => match self.reg_control.prg_rom_bank_mode() {
                0 | 1 => {
                    self.prg_rom
                        [(self.reg_prg_bank as usize & !1) * 0x4000 + addr as usize - 0x8000]
                }
                2 => self.prg_rom[addr as usize - 0x8000],
                3 => self.prg_rom[(self.reg_prg_bank as usize) * 0x4000 + addr as usize - 0x8000],
                _ => unreachable!(),
            },
            0xC000..=0xFFFF => match self.reg_control.prg_rom_bank_mode() {
                0 | 1 => {
                    self.prg_rom[(self.reg_prg_bank as usize | 1) * 0x4000 + addr as usize - 0xC000]
                }
                2 => self.prg_rom[(self.reg_prg_bank as usize) * 0x4000 + addr as usize - 0xC000],
                3 => {
                    self.prg_rom
                        [(self.prg_rom.len() / 0x4000 - 1) * 0x4000 + addr as usize - 0xC000]
                }
                _ => unreachable!(),
            },
        }
    }

    fn cpu_write(&mut self, addr: u16, v: u8) {
        match addr {
            0x0000..=0x5FFF => {}
            0x6000..=0x7FFF => {
                self.prg_ram[addr as usize - 0x6000] = v;
            }
            0x8000..=0xFFFF => {
                if v & 0x80 != 0 {
                    self.reg_load = 0;
                    self.n_load = 0;
                } else {
                    self.reg_load = (self.reg_load >> 1) | (v & 0x1) << 4;
                    self.n_load += 1;

                    if self.n_load == 5 {
                        match addr {
                            0x0000..=0x7FFF => unreachable!(),
                            0x8000..=0x9FFF => self.reg_control = MMC1Control::from(self.reg_load),
                            0xA000..=0xBFFF => {
                                let n_chr_banks = self.chr_rom.len() / 0x1000;
                                self.reg_chr_bank0 = self.reg_load % n_chr_banks as u8;
                            }
                            0xC000..=0xDFFF => {
                                let n_chr_banks = self.chr_rom.len() / 0x1000;
                                self.reg_chr_bank1 = self.reg_load % n_chr_banks as u8;
                            }
                            0xE000..=0xFFFF => self.reg_prg_bank = self.reg_load,
                        }

                        self.reg_load = 0;
                        self.n_load = 0;
                    }
                }
            }
        }
    }

    fn ppu_read(&mut self, addr: u16) -> u8 {
        self.ppu_inspect(addr)
    }

    fn ppu_inspect(&self, addr: u16) -> u8 {
        if self.reg_control.chr_rom_bank_mode() {
            match addr {
                0x0000..=0x0FFF => {
                    self.chr_rom[self.reg_chr_bank0 as usize * 0x1000 + addr as usize]
                }
                0x1000..=0x1FFF => {
                    self.chr_rom[self.reg_chr_bank1 as usize * 0x1000 + addr as usize - 0x1000]
                }
                _ => unreachable!(),
            }
        } else {
            self.chr_rom[self.reg_chr_bank0 as usize * 0x1000 + addr as usize]
        }
    }

    fn ppu_write(&mut self, addr: u16, v: u8) {
        if self.reg_control.chr_rom_bank_mode() {
            match addr {
                0x0000..=0x0FFF => {
                    self.chr_rom[self.reg_chr_bank0 as usize * 0x1000 + addr as usize] = v;
                }
                0x1000..=0x1FFF => {
                    self.chr_rom[self.reg_chr_bank1 as usize * 0x1000 + addr as usize - 0x1000] = v;
                }
                _ => unreachable!(),
            }
        } else {
            self.chr_rom[self.reg_chr_bank0 as usize * 0x1000 + addr as usize] = v;
        }
    }

    fn reset(&mut self) {
        self.n_load = 0;
        self.reg_load = 0;
        self.reg_control = MMC1Control::from(0x1c);
        self.reg_chr_bank0 = 0;
        self.reg_chr_bank1 = 0;
        self.reg_prg_bank = 0;
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
