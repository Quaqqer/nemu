const CTRL_X: u8 = 1 << 0;
const CTRL_Y: u8 = 1 << 1;
const CTRL_INCREMENT: u8 = 1 << 2;
const CTRL_SPRITE_ADDRESS: u8 = 1 << 3;
const CTRL_BACKGROUND: u8 = 1 << 4;
const CTRL_SPRITE_SIZE: u8 = 1 << 5;
const CTRL_MASTER_SLAVE: u8 = 1 << 6;
const CTRL_NMI: u8 = 1 << 7;

pub struct Ppu {
    ppuctrl: u8,
    ppumask: u8,
    oamaddr: u8,
    latch: u8,
    latch_toggle: bool,
    ppuscroll: u16,
    ppuaddr: u16,
    oamdma: u8,

    vram: [u8; 0x800],
    palette: [u8; 32],
    pub oam: [u8; 256],
    odd: bool,

    vblank: bool,
    sprite_0_hit: bool,
    sprite_overflow: bool,

    cyc: u64,
}

impl Ppu {
    pub fn new() -> Self {
        Ppu {
            ppuctrl: 0x00,
            ppumask: 0x00,
            oamaddr: 0x00,
            latch: 0x00,
            latch_toggle: false,
            ppuscroll: 0x0000,
            ppuaddr: 0x0000,
            oamdma: 0x00,

            vram: [0x00; 0x800],
            palette: [0x00; 32],
            oam: [0x00; 256],
            odd: false,

            vblank: true,
            sprite_0_hit: false,
            sprite_overflow: true,

            cyc: 0,
        }
    }

    pub fn read_register(&mut self, addr: u16) -> u8 {
        if addr == 0x4014 {
            0
        } else {
            match (addr - 0x2000) % 0x8 {
                0x0 => 0,
                0x1 => 0,
                0x2 => {
                    let status = self.ppustatus();
                    self.vblank = false;
                    self.latch = 0x00;
                    self.latch_toggle = false;
                    status
                }
                0x3 => 0,
                0x4 => self.oam[self.oamaddr as usize],
                0x5 => self.latch,
                0x6 => self.latch,
                0x7 => self.read_ppudata(),
                _ => unreachable!(),
            }
        }
    }

    pub fn write_register(&mut self, addr: u16, v: u8) {
        if addr == 0x4014 {
            self.oamdma = v;
        } else {
            match (addr - 0x2000) % 0x8 {
                0x0 => self.ppuctrl = v,
                0x1 => self.ppumask = v,
                0x2 => {}
                0x3 => self.oamaddr = v,
                0x4 => {
                    self.oam[self.oamaddr as usize] = v;
                    self.oamaddr = self.oamaddr.wrapping_add(1);
                }
                0x5 => {
                    if self.latch_toggle {
                        self.ppuscroll = ((self.latch as u16) << 8) + v as u16;
                    } else {
                        self.latch = v;
                    }
                    self.latch_toggle = !self.latch_toggle;
                }
                0x6 => {
                    if self.latch_toggle {
                        self.ppuaddr = ((self.latch as u16) << 8) + v as u16;
                    } else {
                        self.latch = v;
                    }
                    self.latch_toggle = !self.latch_toggle;
                }

                0x7 => self.write_ppudata(v),
                _ => unreachable!(),
            }
        }
    }

    fn ppustatus(&self) -> u8 {
        let mut v: u8 = 0x00;
        if self.sprite_overflow {
            v |= 1 << 5;
        }
        if self.sprite_0_hit {
            v |= 1 << 6;
        }
        if self.vblank {
            v |= 1 << 7;
        }
        v
    }

    fn nametable_address(&self) -> u16 {
        match self.ppuctrl & 0x03 {
            0 => 0x2000,
            1 => 0x2400,
            2 => 0x2800,
            3 => 0x2C00,
            _ => unreachable!(),
        }
    }

    fn ppudata_increase(&self) -> u16 {
        match self.ppuctrl & CTRL_INCREMENT != 0 {
            false => 1,
            true => 32,
        }
    }

    pub fn reset(&mut self) {
        self.ppuctrl = 0x00;
        self.ppumask = 0x00;
        self.ppuscroll = 0x0000;
        self.latch = 0x00;
        self.latch_toggle = false;

        self.odd = false;
    }

    fn vblank(&mut self) {
        self.sprite_0_hit = false;
    }

    pub fn cycle(&mut self) {
        match self.cyc {
            257..=320 => {
                self.oamaddr = 0x00;
            }
            _ => {}
        }

        self.cyc += 1;
    }

    fn read_mem(&mut self, addr: u16) -> u8 {
        todo!()
    }
    fn write_mem(&mut self, addr: u16, v: u8) {
        todo!()
    }

    fn read_ppudata(&mut self) -> u8 {
        self.read_mem(self.ppuaddr)
    }

    fn write_ppudata(&mut self, v: u8) {
        self.write_mem(self.ppuaddr, v);
        self.ppuaddr = self.ppuaddr.wrapping_add(self.ppudata_increase());
    }
}
