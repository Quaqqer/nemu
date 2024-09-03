use crate::cart::Cart;
use bitflags::bitflags;

#[derive(Clone, Copy)]
pub struct PpuCtrl(u8);

bitflags! {
    impl PpuCtrl: u8 {
        /// The 9th bit of the x-coordinate of the scroll
        const X                = 0b00000001;
        /// The 9th bit of the y-coordinate of the scroll
        const Y                = 0b00000010;
        const NAMETABLE_SELECT = 0b00000011;
        const INCREMENT_MODE   = 0b00000100;
        const SPRITE_TILE      = 0b00001000;
        const BACKGROUND_TILE  = 0b00010000;
        const SPRITE_HEIGHT    = 0b00100000;
        // Not implemented
        const MASTER_SLAVE     = 0b01000000;
        const NMI_ENABLE       = 0b10000000;
    }
}

impl std::fmt::Display for PpuCtrl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        crate::util::fmt_bitflags_u8(self.bits(), ['n', 'm', 's', 'b', 'a', 'i', 'y', 'x'], f)
    }
}

#[derive(Clone, Copy)]
pub struct PpuMask(u8);

bitflags! {
    impl PpuMask: u8 {
        /// Not implemented
        const GREYSCALE              = 0b00000001;
        /// Show leftmost 8 bg pixels
        const BACKGROUND_LEFT_ENABLE = 0b00000010;
        /// Show leftmost 8 sprite pixels
        const SPRITE_LEFT_ENABLE     = 0b00000100;
        const BACKGROUND_ENABLE      = 0b00001000;
        const SPRITE_ENABLE          = 0b00010000;
        // Not implemented
        const EMPH_RED               = 0b00100000;
        // Not implemented
        const EMPH_GREEN             = 0b01000000;
        // Not implemented
        const EMPH_BLUE              = 0b10000000;
    }
}

impl std::fmt::Display for PpuMask {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        crate::util::fmt_bitflags_u8(self.bits(), ['b', 'g', 'r', 's', 'b', 's', 'b', 'm'], f)
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

impl std::fmt::Display for PpuStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        crate::util::fmt_bitflags_u8(self.bits(), ['v', 's', 'o', 'x', 'x', 'x', 'x', 'x'], f)
    }
}

pub struct Ppu {
    // Registers
    pub ppuctrl: PpuCtrl,
    pub ppumask: PpuMask,
    pub ppustatus: PpuStatus,
    oamaddr: u8,
    latch_toggle: bool,
    pub t: u16,
    pub v: u16,
    oamdma: u8,

    // Current PPU position
    pub scanline: u16,
    pub cycle: u16,

    pub fine_x: u8,

    pub vram: [u8; 0x800],
    pub palette: [u8; 0x20],
    pub oam: [u8; 0x100],
    odd: bool,

    pub nmi: bool,

    display: Display,

    nt: u8,
    at: u8,
    pt_low: u8,
    pt_high: u8,
    palette_i: u8,
    next_nt: u8,
    next_at: u8,
    next_pt_low: u8,
    next_pt_high: u8,
    next_palette_i: u8,
    next2_nt: u8,
    next2_at: u8,
    next2_pt_low: u8,
    next2_pt_high: u8,
    next2_palette_i: u8,
}

impl Ppu {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Ppu {
            ppuctrl: PpuCtrl::empty(),
            ppumask: PpuMask::empty(),
            ppustatus: PpuStatus::SPRITE_OVERFLOW | PpuStatus::VBLANK,
            oamaddr: 0x00,
            latch_toggle: false,
            t: 0x0000,
            v: 0x0000,
            oamdma: 0x00,

            vram: [0x00; 0x800],
            palette: [0x00; 32],
            oam: [0x00; 256],
            odd: false,

            scanline: 261,
            cycle: 0,

            fine_x: 0,

            nmi: false,

            display: Display::new(),

            nt: 0,
            at: 0,
            pt_low: 0,
            pt_high: 0,
            palette_i: 0,
            next_nt: 0,
            next_at: 0,
            next_pt_low: 0,
            next_pt_high: 0,
            next_palette_i: 0,
            next2_nt: 0,
            next2_at: 0,
            next2_pt_low: 0,
            next2_pt_high: 0,
            next2_palette_i: 0,
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
                // $2000
                //
                // Control
                0 => 0,
                // $2001
                //
                // Mask
                1 => 0,
                // $2002
                //
                // Status
                2 => {
                    let status = self.ppustatus.bits();
                    self.ppustatus -= PpuStatus::VBLANK;
                    self.latch_toggle = false;
                    status
                }
                // $2003
                //
                // OAM address
                3 => 0,
                // $2004
                //
                // OAM data
                4 => self.oam[self.oamaddr as usize],
                // $2005
                //
                // Scroll
                5 => 0,
                // $2006
                //
                // Addr
                6 => 0,
                // $2007
                //
                // Data
                7 => {
                    // TODO: Some stuff should be done different here apparently
                    let v = self.read_mem(cart, self.v);
                    self.v = self.v.wrapping_add(self.ppudata_increase());
                    v
                }
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
        debug_assert!((0x2000..0x4000).contains(&addr) || addr == 0x4014);

        if addr == 0x4014 {
            self.oamdma = v;
        } else {
            match (addr - 0x2000) % 8 {
                // Control
                0 => {
                    self.ppuctrl = PpuCtrl::from_bits(v).unwrap();
                    self.t &= !(0b11 << 10);
                    self.t |= (v as u16 & 0b11) << 10;
                }
                // Mask
                1 => self.ppumask = PpuMask::from_bits(v).unwrap(),
                // Status
                2 => {}
                // OAM Address
                3 => self.oamaddr = v,
                // OAM data
                4 => {
                    self.oam[self.oamaddr as usize] = v;
                    self.oamaddr = self.oamaddr.wrapping_add(1);
                }
                // Scroll
                5 => {
                    if !self.latch_toggle {
                        // Set fine x
                        self.fine_x = v & 0b111;

                        // Set coarse x
                        self.t &= !(0b11111);
                        self.t |= v as u16 >> 3;
                    } else {
                        // Set coarse y
                        self.t &= !(0b11111 << 5);
                        self.t |= ((v as u16 >> 3) & 0b11111) << 5;

                        // Set fine y
                        self.t &= !(0b111 << 12);
                        self.t |= (v as u16 & 0b111) << 12;
                    }
                    self.latch_toggle ^= true;
                }
                // Addr
                6 => {
                    if !self.latch_toggle {
                        self.t &= !(0b1111111 << 8);
                        self.t |= (v as u16 & 0b111111) << 8;
                    } else {
                        self.t &= !0xFF;
                        self.t |= v as u16;
                        self.v = self.t;
                    }
                    self.latch_toggle ^= true;
                }
                // Data
                7 => {
                    self.write_mem(cart, self.v, v);
                    self.v = self.v.wrapping_add(self.ppudata_increase());
                }
                _ => unreachable!(),
            }
        }
    }

    fn ppudata_increase(&self) -> u16 {
        match self.ppuctrl.intersects(PpuCtrl::INCREMENT_MODE) {
            false => 1,
            true => 32,
        }
    }

    pub fn reset(&mut self) {
        self.ppuctrl = PpuCtrl::empty();
        self.ppumask = PpuMask::empty();
        self.t = 0x0000;
        self.latch_toggle = false;

        self.odd = false;
    }

    pub fn cycle(&mut self, cart: &mut Cart) {
        if self.ppumask.intersects(PpuMask::BACKGROUND_ENABLE) {
            self.render_bg(cart);
        }

        match (self.scanline, self.cycle) {
            (241, 1) => {
                // Start vblank
                // Remove sprite 0 hit on vblank?
                self.ppustatus |= PpuStatus::VBLANK;

                self.nmi = true;
            }
            (261, 1) => {
                self.ppustatus -=
                    PpuStatus::VBLANK | PpuStatus::SPRITE_0_HIT | PpuStatus::SPRITE_OVERFLOW;
            }
            _ => {}
        }

        self.cycle += 1;
        if self.cycle > 340 {
            self.cycle = 0;
            self.scanline += 1;
            if self.scanline > 261 {
                self.scanline = 0;

                self.odd = !self.odd;
            }
        }
    }

    fn render_bg(&mut self, cart: &mut Cart) {
        let cycle = self.cycle;
        let scanline = self.scanline;

        match scanline {
            // Visible scanlines and pre-render scanline
            0..240 | 261 => {
                // Set shift registers
                if cycle % 8 == 1 {
                    self.shift_registers();
                }

                // Fetches
                match cycle {
                    0..=256 | 321..=336 if cycle % 8 == 1 => {
                        self.next2_nt = self.fetch_nt(cart);
                    }
                    0..=256 | 321..=336 if cycle % 8 == 3 => {
                        self.next2_at = self.fetch_at(cart);
                        self.next2_palette_i = self.fetch_palette_i();
                    }
                    0..=256 | 321..=336 if cycle % 8 == 5 => {
                        self.next2_pt_low = self.fetch_pt_low(cart);
                    }
                    0..=256 | 321..=336 if cycle % 8 == 7 => {
                        self.next2_pt_high = self.fetch_pt_high(cart);
                    }

                    257..=320 if cycle % 8 == 1 => {
                        self.fetch_nt(cart);
                    }
                    257..=320 if cycle % 8 == 3 => {
                        self.fetch_nt(cart);
                    }
                    337 | 339 => {
                        self.fetch_nt(cart);
                    }
                    _ => {}
                }

                // Scroll
                match cycle {
                    8..256 if cycle % 8 == 0 => {
                        self.inc_coarse_x();
                    }
                    256 => {
                        self.inc_coarse_x();
                        self.inc_fine_y();
                    }
                    257 => {
                        self.reset_coarse_x();
                    }
                    328 | 336 => self.inc_coarse_x(),
                    _ => {}
                }
                if scanline == 261 {
                    match cycle {
                        280..=304 => self.reset_coarse_y(),
                        _ => {}
                    }
                }

                // Draw background pixel
                if scanline != 261 {
                    match cycle {
                        1..8 => {
                            if self.ppumask.intersects(PpuMask::BACKGROUND_LEFT_ENABLE) {
                                self.draw_bg_px()
                            }
                        }
                        8..=256 => {
                            self.draw_bg_px();
                        }
                        _ => {}
                    }
                }
            }

            // Post-render scanline, idle
            240 => {}
            // Vertical blanking lines
            241..261 => {}
            // Pre-render scanline
            _ => unreachable!(),
        }
    }

    fn read_mem(&mut self, cart: &mut Cart, addr: u16) -> u8 {
        match addr % 0x4000 {
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
            0x3F00..=0x3FFF => self.palette[(addr % 0x20) as usize],

            _ => unreachable!(),
        }
    }

    fn write_mem(&mut self, cart: &mut Cart, addr: u16, v: u8) {
        match addr % 0x4000 {
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
            0x3F00..=0x3FFF => self.palette[(addr % 0x20) as usize] = v,

            _ => {
                unreachable!()
            }
        }
    }

    pub fn display(&self) -> &Display {
        &self.display
    }

    fn shift_registers(&mut self) {
        self.next_nt = self.next2_nt;
        self.next_at = self.next2_at;
        self.next_pt_low = self.next2_pt_low;
        self.next_pt_high = self.next2_pt_high;
        self.next_palette_i = self.next2_palette_i;
        self.nt = self.next_nt;
        self.at = self.next_at;
        self.pt_low = self.next_pt_low;
        self.pt_high = self.next_pt_high;
        self.palette_i = self.next_palette_i;
    }

    fn fetch_nt(&mut self, cart: &mut Cart) -> u8 {
        let nt_addr = 0x2000 | (self.v & 0x0FFF);
        self.read_mem(cart, nt_addr)
    }

    fn fetch_at(&mut self, cart: &mut Cart) -> u8 {
        let attr_addr =
            0x23C0 | (self.v & 0x0C00) | ((self.v >> 4) & 0x38) | ((self.v >> 2) & 0x07);
        self.read_mem(cart, attr_addr)
    }

    fn fetch_pt_low(&mut self, cart: &mut Cart) -> u8 {
        cart.get_sprite_i(
            self.ppuctrl.intersects(PpuCtrl::BACKGROUND_TILE) as u8,
            self.next_nt,
        )[self.fine_y() as usize]
    }

    fn fetch_pt_high(&mut self, cart: &mut Cart) -> u8 {
        cart.get_sprite_i(
            self.ppuctrl.intersects(PpuCtrl::BACKGROUND_TILE) as u8,
            self.next_nt,
        )[8 + self.fine_y() as usize]
    }

    fn fine_y(&self) -> u8 {
        (self.v >> 12) as u8 & 0b111
    }

    fn inc_fine_y(&mut self) {
        if (self.v & 0x7000) != 0x7000 {
            self.v += 0x1000;
        } else {
            self.v &= !0x7000;
            let mut y = (self.v & 0x03E0) >> 5;
            if y == 29 {
                y = 0;
                self.v ^= 0x0800;
            } else if y == 31 {
                y = 0;
            } else {
                y += 1;
            }
            self.v = (self.v & !0x03E0) | (y << 5);
        }
    }

    fn inc_coarse_x(&mut self) {
        if (self.v & 0x001F) == 31 {
            self.v &= !0x0001F;
            self.v ^= 0x0400;
        } else {
            self.v += 1;
        }
    }

    fn coarse_x(&self) -> u16 {
        self.v & 0x1FF
    }

    fn coarse_y(&self) -> u16 {
        (self.v >> 5) & 0x1FF
    }

    fn draw_bg_px(&mut self) {
        debug_assert!((1..257).contains(&self.cycle));
        debug_assert!((0..240).contains(&self.scanline));

        let x = self.cycle - 1;
        let y = self.scanline;

        let px_low = (self.pt_low >> ((self.fine_x as u16).wrapping_sub(self.cycle) % 8)) & 0b1;
        let px_high = (self.pt_high >> ((self.fine_x as u16).wrapping_sub(self.cycle) % 8)) & 0b1;

        let px = (px_high << 1) | px_low;

        let palette = self.get_palette(if px != 0 { self.palette_i } else { 0 });

        let palette_color_i = palette[px as usize] & 0x7F;

        let &[r, g, b] = &PALETTE[palette_color_i as usize * 3..palette_color_i as usize * 3 + 3]
        else {
            unreachable!()
        };

        self.display.set_pixel(x as usize, y as usize, (r, g, b));
    }

    fn draw_oam_px(&mut self) {
        for (i, bytes) in self.oam.chunks(4).enumerate() {
            let &[b0, b1, b2, b3] = bytes else {
                unreachable!();
            };

            let y = b0;

            let pattern_table = if self.ppuctrl.intersects(PpuCtrl::SPRITE_HEIGHT) {
                b1 & 0x1
            } else {
                self.ppuctrl.intersects(PpuCtrl::SPRITE_TILE) as u8
            };

            let tile_number = b1 >> 1;
        }
    }

    /// Get a palette
    ///
    /// * `i`: Palette index, BG: 0-3, FG: 4-7
    fn get_palette(&self, i: u8) -> &[u8; 4] {
        (&self.palette[i as usize * 4..i as usize * 4 + 4])
            .try_into()
            .unwrap()
    }

    fn reset_coarse_x(&mut self) {
        self.v &= !(0b0100_00011111);
        self.v |= self.t & (0b0100_00011111);
    }

    fn reset_coarse_y(&mut self) {
        self.v &= !(0b111101111100000);
        self.v |= self.t & (0b111101111100000);
    }

    fn fetch_palette_i(&self) -> u8 {
        match (self.coarse_y() % 4 / 2, self.coarse_x() % 4 / 2) {
            (0, 0) => (self.next_at >> 0) & 0x3,
            (0, 1) => (self.next_at >> 2) & 0x3,
            (1, 0) => (self.next_at >> 4) & 0x3,
            (1, 1) => (self.next_at >> 6) & 0x3,
            _ => unreachable!(),
        }
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

pub const PALETTE: [u8; 3 * 64] = {
    let mut palette = [0; 3 * 64];
    let bytes = include_bytes!("../res/palette/2c02.pal");
    let mut i = 0;
    while i < 3 * 64 {
        palette[i] = bytes[i];
        i += 1;
    }
    palette
};

#[cfg(test)]
mod tests {
    use crate::cart::Cart;

    use super::Ppu;

    #[test]
    fn ppu_scroll_registers() {
        // Test taken from https://www.nesdev.org/wiki/PPU_scrolling#Summary
        let mut ppu = Ppu::new();
        let cart = &mut Cart::read_ines1_0("roms/nestest/nestest.nes");

        // $2000 write
        ppu.cpu_write_register(cart, 0x2000, 0b00000000);
        assert!(ppu.t & (0b11 << 10) == 0);

        // $2002 read
        ppu.cpu_read_register(cart, 0x2002);
        assert!(!ppu.latch_toggle);

        // $2005 write 1
        ppu.cpu_write_register(cart, 0x2005, 0b01111101);
        assert!(ppu.t & 0b11000011111 == 0b1111);
        assert!(ppu.fine_x == 0b101);
        assert!(ppu.latch_toggle);

        // $2005 write 2
        ppu.cpu_write_register(cart, 0x2005, 0b01011110);
        assert!(ppu.t == 0b01100001_01101111);
        assert!(!ppu.latch_toggle);

        // $2006 write 1
        ppu.cpu_write_register(cart, 0x2006, 0b00111101);
        assert!(ppu.t == 0b00111101_01101111);
        assert!(ppu.latch_toggle);

        // $2006 write 2
        ppu.cpu_write_register(cart, 0x2006, 0b11110000);
        assert!(ppu.t == 0b00111101_11110000);
        assert!(ppu.v == 0b00111101_11110000);
        assert!(!ppu.latch_toggle);
    }
}
