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

    pub vram: [u8; 0x800],
    palette: [u8; 0x20],
    pub oam: [u8; 0x100],
    odd: bool,

    vblank: bool,
    sprite_0_hit: bool,
    sprite_overflow: bool,

    scanline: u16,
    col: u16,

    nt: u8,
    at: u8,
    pt: u16,
    next_nt: u8,
    next_at: u8,
    next_pt: u16,

    nmi: bool,

    display: Display,
}

impl Ppu {
    #[allow(clippy::new_without_default)]
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

            scanline: 0,
            col: 0,

            nt: 0,
            at: 0,
            pt: 0,
            next_nt: 0,
            next_at: 0,
            next_pt: 0,

            nmi: false,

            display: Display::new(),
        }
    }

    /// Read a PPU register from the CPU.
    ///
    /// The PPU exposes 8 registers to the CPU, which are mirrored every 8 bytes.
    /// Expects an address in the range of [0x2000, 0x3FFF] or 0x4014.
    ///
    /// * `cart`: The cart
    /// * `addr`: The memory address
    pub(crate) fn cpu_read_register(&mut self, cart: &mut Cart, addr: u16) -> u8 {
        debug_assert!(0x2000 <= addr && addr < 0x4000 || addr == 0x4014);

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
                0x7 => self.read_ppudata(cart),
                _ => unreachable!(),
            }
        }
    }

    /// Write to a PPU register from the CPU.
    ///
    /// The PPU exposes 8 registers to the CPU, which are mirrored every 8 bytes.
    /// Expects an address in the range of [0x2000, 0x3FFF] or 0x4014.
    ///
    /// * `cart`: The cart
    /// * `addr`: The memory address
    /// * `v`: The value
    pub(crate) fn cpu_write_register(&mut self, cart: &mut Cart, addr: u16, v: u8) {
        debug_assert!(0x2000 <= addr && addr < 0x4000);

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

                0x7 => self.write_ppudata(cart, v),
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

    pub fn cycle(&mut self, cart: &Cart) {
        // Fetch data
        match self.col % 8 {
            1 => {
                // self.next_nt = self.read_mem
            }
            3 => {
                // TODO: Fetch palette
                // self.next_at = todo!();
            }
            5 => {
                // self.next_pt_l = todo!();
            }
            7 => {
                // self.next_pt_h = todo!();
            }
            _ => {}
        }

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

    fn read_mem(&mut self, cart: &mut Cart, addr: u16) -> u8 {
        match addr {
            // Pattern tables
            0x0000..=0x0FFF => cart.read_pattern_table(self, 0, addr % 0x1000),
            0x1000..=0x1FFF => cart.read_pattern_table(self, 1, addr % 0x1000),
            // Name tables
            0x2000..=0x23FF => cart.read_nametable(self, 0, (addr - 0x2000) % 0x400),
            0x2400..=0x27FF => cart.read_nametable(self, 1, (addr - 0x2400) % 0x400),
            0x2800..=0x2BFF => cart.read_nametable(self, 2, (addr - 0x2800) % 0x400),
            0x2C00..=0x2FFF => cart.read_nametable(self, 3, (addr - 0x2c00) % 0x400),
            // Unused address, do nothing for now
            // TODO: Mapped by cartridge
            0x3000..=0x3EFF => 0,
            // Palette ram indexes, mirrored every 0x20 values
            0x3F00..=0x3F1F => self.palette[(addr % 0x20) as usize],

            _ => unreachable!(
                "Address ${:#04x} is outside of the address space for the PPU",
                addr
            ),
        }
    }

    fn write_mem(&mut self, cart: &mut Cart, addr: u16, v: u8) {
        match addr {
            // Pattern tables
            0x0000..=0x0FFF => cart.write_pattern_table(self, 0, addr % 0x1000, v),
            0x1000..=0x1FFF => cart.write_pattern_table(self, 1, addr % 0x1000, v),
            // Name tables
            0x2000..=0x23FF => cart.write_nametable(self, 0, (addr - 0x2000) % 0x400, v),
            0x2400..=0x27FF => cart.write_nametable(self, 1, (addr - 0x2400) % 0x400, v),
            0x2800..=0x2BFF => cart.write_nametable(self, 2, (addr - 0x2800) % 0x400, v),
            0x2C00..=0x2FFF => cart.write_nametable(self, 3, (addr - 0x2c00) % 0x400, v),
            // Unused address, do nothing for now
            // TODO: Mapped by cartridge
            0x3000..=0x3EFF => {}
            // Palette ram indexes, mirrored every 0x20 values
            0x3F00..=0x3F1F => self.palette[(addr % 0x20) as usize] = v,

            _ => unreachable!(
                "Address ${:#04x} is outside of the address space for the PPU",
                addr
            ),
        }
    }

    fn read_ppudata(&mut self, cart: &mut Cart) -> u8 {
        let v = self.read_mem(cart, self.ppuaddr);
        self.ppuaddr = self.ppuaddr.wrapping_add(self.ppudata_increase());
        v
    }

    fn write_ppudata(&mut self, cart: &mut Cart, v: u8) {
        self.write_mem(cart, self.ppuaddr, v);
        self.ppuaddr = self.ppuaddr.wrapping_add(self.ppudata_increase());
    }

    pub fn display(&self) -> &Display {
        &self.display
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
