const CTRL_X: u8 = 1 << 0;
const CTRL_Y: u8 = 1 << 1;
const CTRL_VRAM: u8 = 1 << 2;
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
    ppudata: u8,
    oamdma: u8,

    pub oam: [u8; 256],
    odd: bool,

    vblank: bool,
    sprite_0_hit: bool,
    sprite_overflow: bool,

    cyc: u64,
}

struct Latch16 {
    v: u16,
    l: Option<u8>,
}

impl Latch16 {
    pub fn new(v: u16) -> Self {
        Latch16 { v, l: None }
    }

    pub fn read(&self) -> u16 {
        self.v
    }

    pub fn read_latch(&self) -> u8 {
        if let Some(v) = self.l {
            v
        } else {
            0x00
        }
    }

    pub fn write(&mut self, v: u8) {
        if let Some(v_) = self.l {
            self.v = (v_ as u16) << 8 + v as u16;
            self.l = None;
        } else {
            self.l = Some(v)
        }
    }

    pub fn reset_latch(&mut self) {
        self.l = None;
    }

    pub fn reset(&mut self, v: u16) {
        self.v = v;
        self.reset_latch();
    }
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
            ppudata: 0x00,
            oamdma: 0x00,

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
                0x7 => self.ppudata,
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

                0x7 => self.ppudata = v,
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

    pub fn reset(&mut self) {
        self.ppuctrl = 0x00;
        self.ppumask = 0x00;
        self.ppuscroll = 0x0000;
        self.latch = 0x00;
        self.latch_toggle = false;
        self.ppudata = 0x00;

        self.odd = false;
    }

    fn vblank(&mut self) {
        self.sprite_0_hit = false;
    }

    fn cycle(&mut self) {
        match self.cyc {
            257..=320 => {
                self.oamaddr = 0x00;
            }
            _ => {}
        }

        self.cyc += 1;
    }
}
