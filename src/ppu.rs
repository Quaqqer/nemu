use crate::cart::Cart;

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

    scanline: u16,
    col: u16,

    nmi: bool,

    display: Display,

    chr: Vec<u8>,
}

impl Ppu {
    #[allow(clippy::new_without_default)]
    pub fn new(cart: &Cart) -> Self {
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

            scanline: 0,
            col: 0,

            nmi: false,

            display: Display::new(),

            chr: cart.chr.clone(),
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

    pub fn tick(&mut self) {
        // match (line, col) {}
        // match self.cyc {
        //     257..=320 => {
        //         self.oamaddr = 0x00;
        //     }
        //     _ => {}
        // }

        match (self.scanline, self.col, self.odd) {
            (261, 339, false) | (261, 340, true) => {
                self.col = 0;
                self.scanline = 0;
                self.odd = !self.odd;
            }
            (_, 340, _) => {
                self.col = 0;
                self.scanline += 1;
            }
            _ => {}
        }
    }

    fn read_mem(&mut self, _addr: u16) -> u8 {
        todo!()
    }
    fn write_mem(&mut self, _addr: u16, _v: u8) {
        // TODO: Should probably do something
    }

    fn read_ppudata(&mut self) -> u8 {
        let v = self.read_mem(self.ppuaddr);
        self.ppuaddr = self.ppuaddr.wrapping_add(self.ppudata_increase());

        v
    }

    fn write_ppudata(&mut self, v: u8) {
        self.write_mem(self.ppuaddr, v);
        self.ppuaddr = self.ppuaddr.wrapping_add(self.ppudata_increase());
    }

    pub fn display(&self) -> &Display {
        &self.display
    }

    /// Get a slice of the 16 bytes that a sprite is contained in.
    ///
    /// Every pixel has 2 bits of information, the first bit is stored in the first 8 bytes, the
    /// second bit is stored in the last 8 bytes.
    ///
    /// * `page`: The page, either 0 or 1
    /// * `sprite_x`: The sprite x coordinate
    /// * `sprite_y`: The sprite y coordinate
    pub fn get_sprite(&self, page: u8, sprite_x: u8, sprite_y: u8) -> &[u8] {
        debug_assert!(page <= 1);
        debug_assert!(sprite_x <= 0xF);
        debug_assert!(sprite_y <= 0xF);

        let base = page as usize * 0x1000 + ((sprite_y as usize * 16 + sprite_x as usize) * 16);
        &self.chr[base..base + 16]
    }

    /// Get a single pixel from a sprite
    ///
    /// * `page`: The page, either 0 or 1
    /// * `sprite_x`: The sprite x-coordinate
    /// * `sprite_y`: The sprite y-coordinate
    /// * `x_offset`: The pixel offset
    /// * `y_offset`: The pixel offset
    pub fn get_sprite_pixel(
        &self,
        page: u8,
        sprite_x: u8,
        sprite_y: u8,
        x_offset: u8,
        y_offset: u8,
    ) -> u8 {
        debug_assert!(x_offset <= 0x8);
        debug_assert!(y_offset <= 0x8);

        let sprite = self.get_sprite(page, sprite_x, sprite_y);
        let l = (sprite[y_offset as usize] >> (7 - x_offset)) & 1;
        let r = (sprite[y_offset as usize + 8] >> (7 - x_offset)) & 1;
        (r << 1) | l
    }
}

pub struct Display {
    pub pixels: [u8; Self::WIDTH * Self::HEIGHT * 3],
}

impl Display {
    pub const WIDTH: usize = 256;
    pub const HEIGHT: usize = 240;

    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            pixels: [0x0; Self::WIDTH * Self::HEIGHT * 3],
        }
    }

    pub fn get_pixel(&self, x: usize, y: usize) -> (u8, u8, u8) {
        assert!(x < Self::WIDTH);
        assert!(y < Self::HEIGHT);
        let base = ((y * Self::WIDTH) + x) * 3;

        let r = self.pixels[base];
        let g = self.pixels[base + 1];
        let b = self.pixels[base + 2];
        (r, g, b)
    }

    pub fn set_pixel(&mut self, x: usize, y: usize, rgb: (u8, u8, u8)) {
        assert!(x < Self::WIDTH);
        assert!(y < Self::HEIGHT);
        let base = ((y * Self::WIDTH) + x) * 3;

        let (r, g, b) = rgb;
        self.pixels[base] = r;
        self.pixels[base + 1] = g;
        self.pixels[base + 2] = b;
    }
}
