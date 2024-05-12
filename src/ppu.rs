use crate::cart::Cart;
use bitflags::{bitflags, Flags};

#[derive(Clone, Copy)]
pub struct PpuCtrl(u8);

bitflags! {
    impl PpuCtrl: u8 {
        /// The 9th bit of the x-coordinate of the scroll
        const X                 = 0b00000001;
        /// The 9th bit of the y-coordinate of the scroll
        const Y                 = 0b00000010;
        const NAMETABLE_ADDRESS = 0b00000011;
        const INCREMENT         = 0b00000100;
        const SPRITE_ADDRESS    = 0b00001000;
        const BACKGROUND        = 0b00010000;
        const SPRITE_SIZE       = 0b00100000;
        const MASTER_SLAVE      = 0b01000000;
        const NMI               = 0b10000000;
    }
}

#[derive(Clone, Copy)]
pub struct PpuMask(u8);

bitflags! {
    impl PpuMask: u8 {
        /// Grayscale mode
        const GREYSCALE             = 0b00000001;
        /// Show leftmost 8 bg pixels
        const SHOW_LEFTMOST_BG      = 0b00000010;
        /// Show leftmost 8 sprite pixels
        const SHOW_LEFTMOST_SPRITES = 0b00000100;
        const SHOW_BACKGROUND       = 0b00001000;
        const SHOW_SPRITES          = 0b00010000;
        const EMPH_RED              = 0b00100000;
        const EMPH_GREEN            = 0b01000000;
        const EMPH_BLUE             = 0b10000000;
    }
}

#[derive(Clone, Copy)]
pub struct PpuStatus(u8);
bitflags! {
    impl PpuStatus: u8 {
        /// Stale PPU bus contents
        const OPEN_BUS        = 0b00011111;
        /// Sprite overflow, if more than 8 sprites are rendered on the same scanline (has false
        /// positives and negatives)
        const SPRITE_OVERFLOW = 0b00100000;
        /// Set when non-zero sprite of sprite 0 overlaps with non-zero background pixel
        const SPRITE_0_HIT    = 0b01000000;
        /// Vertical blank started
        const VBLANK          = 0b10000000;
    }
}

pub struct Ppu {
    ppuctrl: PpuCtrl,
    ppumask: PpuMask,
    ppustatus: PpuStatus,
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

    scanline: u16,
    col: u16,

    nt: u8,
    at: u8,
    pt: u16,
    next_nt: u8,
    next_at: u8,
    next_pt: u16,

    nmi: bool,
    pub frame_end: bool,

    display: Display,
}

impl Ppu {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Ppu {
            ppuctrl: PpuCtrl::empty(),
            ppumask: PpuMask::empty(),
            ppustatus: PpuStatus::SPRITE_OVERFLOW | PpuStatus::VBLANK,
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

            scanline: 0,
            col: 0,

            nt: 0,
            at: 0,
            pt: 0,
            next_nt: 0,
            next_at: 0,
            next_pt: 0,

            nmi: false,
            frame_end: false,

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
        debug_assert!((0x2000..0x4000).contains(&addr) || addr == 0x4014);

        if addr == 0x4014 {
            0
        } else {
            match (addr - 0x2000) % 0x8 {
                0x0 => 0,
                0x1 => 0,
                0x2 => {
                    let status = self.ppustatus.bits();
                    self.ppustatus -= PpuStatus::VBLANK;
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
        debug_assert!((0x2000..0x4000).contains(&addr));

        if addr == 0x4014 {
            self.oamdma = v;
        } else {
            match (addr - 0x2000) % 0x8 {
                0x0 => {
                    // TODO: If currently in a vblank and PPUSTATUS vblank flag is set, setting nmi
                    // flag will generate NMI.
                    //
                    // Master/slave mode and EXT pins
                    //
                    // https://www.nesdev.org/wiki/PPU_registers#Controller_($2000)_%3E_write
                    self.ppuctrl = PpuCtrl::from_bits(v).unwrap()
                }
                0x1 => self.ppumask = PpuMask::from_bits(v).unwrap(),
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

    fn nametable_address(&self) -> u16 {
        match self.ppuctrl.bits() & 0b11 {
            0 => 0x2000,
            1 => 0x2400,
            2 => 0x2800,
            3 => 0x2C00,
            _ => unreachable!(),
        }
    }

    fn ppudata_increase(&self) -> u16 {
        match self.ppuctrl.intersects(PpuCtrl::INCREMENT) {
            false => 1,
            true => 32,
        }
    }

    pub fn reset(&mut self) {
        self.ppuctrl = PpuCtrl::empty();
        self.ppumask = PpuMask::empty();
        self.ppuscroll = 0x0000;
        self.latch = 0x00;
        self.latch_toggle = false;

        self.odd = false;
    }

    fn vblank(&mut self) {
        self.ppustatus -= PpuStatus::SPRITE_0_HIT;
    }

    pub fn cycle(&mut self, cart: &Cart) {
        let screen_x = self.col;
        let screen_y = self.scanline;

        if self.col % 8 == 0 {
            self.nt = self.next_nt;
            self.at = self.next_at;
            self.pt = self.next_pt;
        }

        let pt_x = self.col
            + (if self.ppuctrl.intersects(PpuCtrl::X) {
                0x100
            } else {
                0x0
            })
            + (self.ppuscroll >> 8);

        let pt_y = self.scanline
            + (if self.ppuctrl.intersects(PpuCtrl::Y) {
                0x100
            } else {
                0x0
            })
            + (self.ppuscroll & 0xFF);

        let pt_tile_x = pt_x / 8;
        let pt_tile_y = pt_y / 8;

        if self.col < 256 && self.scanline < 240 {
            let color = match rand::random::<u8>() & 0b11 {
                0 => (0, 0, 0),
                1 => (125, 0, 0),
                2 => (0, 125, 0),
                3 => (0, 0, 125),
                _ => unreachable!(),
            };
            self.display
                .set_pixel(self.col as usize, self.scanline as usize, color);
        }

        // Fetches
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

        let rendering_enabled = self
            .ppumask
            .intersects(PpuMask::SHOW_BACKGROUND | PpuMask::SHOW_SPRITES);

        if self.scanline == 241 && self.col == 1 {
            self.ppustatus |= PpuStatus::VBLANK;
            self.frame_end = true;
        }

        if self.scanline == 261 && self.col == 1 {
            self.ppustatus -= PpuStatus::VBLANK;
        }

        match (self.scanline, self.col, self.odd && rendering_enabled) {
            // Skip 1 cycle if odd and rendering is enabled
            (261, 340, _) | (261, 339, true) => {
                self.col = 0;
                self.scanline = 0;
                self.odd = !self.odd;
            }
            (_, 340, _) => {
                self.col = 0;
                self.scanline += 1;
            }
            _ => {
                self.col += 1;
            }
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
