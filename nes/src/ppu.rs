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
    ppustatus: u8,
    oamaddr: u8,
    oamdata: u8,
    ppuscroll: Latch16,
    ppuaddr: Latch16,
    ppudata: u8,
    oamdma: u8,
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

    pub fn write(&mut self, v: u8) {
        if let Some(v_) = self.l {
            self.v = (v_ as u16) << 8 + v as u16;
            self.l = None;
        } else {
            self.l = Some(v)
        }
    }
}

impl Ppu {
    pub fn new() -> Self {
        Ppu {
            ppuctrl: 0x00,
            ppumask: 0x00,
            ppustatus: 0x00,
            oamaddr: 0x00,
            oamdata: 0x00,
            ppuscroll: Latch16::new(0x0000),
            ppuaddr: Latch16::new(0x0000),
            ppudata: 0x00,
            oamdma: 0x00,
        }
    }

    pub fn read_register(&self, addr: u16) -> u8 {
        if addr == 0x4014 {
            0
        } else {
            match (addr - 0x2000) % 0x8 {
                0x0 => 0,
                0x1 => 0,
                0x2 => self.ppustatus(),
                0x3 => 0,
                0x4 => self.oamdata,
                0x5 => 0,
                0x6 => 0,
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
                0x2 => self.ppustatus = v,
                0x3 => self.oamaddr = v,
                0x4 => self.oamdata = v,
                0x5 => self.ppuscroll.write(v),
                0x6 => self.ppuaddr.write(v),
                0x7 => self.ppudata = v,
                _ => unreachable!(),
            }
        }
    }

    fn ppustatus(&self) -> u8 {
        todo!()
    }

    pub fn reset(&mut self) {}
}
