use bitfield_struct::bitfield;
use bitflags::bitflags;

use crate::{carts::Cart, config::NemuConfig};

#[derive(Clone, Copy, bincode::Encode, bincode::Decode)]
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

#[derive(Clone, Copy, PartialEq, Eq, bincode::Encode, bincode::Decode)]
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

bitflags! {
    #[derive(Clone, Copy)]
    pub struct SpriteFlags: u8 {
        const FLIP_V = 0x80;
        const FLIP_H = 0x40;
        const PRIO = 0x20;
        const PALETTE = 0x3;
    }
}

impl std::fmt::Display for PpuMask {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        crate::util::fmt_bitflags_u8(self.bits(), ['b', 'g', 'r', 's', 'b', 's', 'b', 'm'], f)
    }
}

#[bitfield(u16)]
#[derive(bincode::Encode, bincode::Decode)]
pub struct LoopyRegister {
    #[bits(5)]
    pub coarse_x: u8,
    #[bits(5)]
    pub coarse_y: u8,
    #[bits(2)]
    pub nametable: u8,
    #[bits(3)]
    pub fine_y: u8,
    _unused: bool,
}

#[derive(Clone, Copy, bincode::Encode, bincode::Decode)]
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

#[derive(Clone, bincode::Encode, bincode::Decode)]
pub struct Ppu {
    // Registers
    pub ppuctrl: PpuCtrl,
    pub ppumask: PpuMask,
    pub ppustatus: PpuStatus,
    oamaddr: u8,
    latch_toggle: bool,
    pub t: LoopyRegister,
    pub v: LoopyRegister,
    oamdma: u8,

    // Current PPU position
    pub scanline: u16,
    pub cycle: u16,

    pub fine_x: u8,

    pub vram: [u8; 0x800],
    pub palette: [u8; 0x20],
    pub oam: [u8; 0x100],
    odd: bool,

    ppudata_read: u8,

    sprite_scanline: Vec<[u8; 4]>,
    sprite_0_in_scanline: bool,
    sprite_px_lo: Vec<u8>,
    sprite_px_hi: Vec<u8>,

    pub nmi: bool,

    display: Display,

    bg_shift_pt_low: u16,
    bg_shift_pt_high: u16,
    bg_shift_at_low: u16,
    bg_shift_at_high: u16,

    bg_next_nt: u8,
    bg_next_at: u8,
    bg_next_pt_low: u8,
    bg_next_pt_high: u8,
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
            t: LoopyRegister::from(0x0000),
            v: LoopyRegister::from(0x0000),
            oamdma: 0x00,

            vram: [0x00; 0x800],
            palette: [0x00; 32],
            oam: [0x00; 256],
            odd: false,

            ppudata_read: 0,

            sprite_scanline: Vec::new(),
            sprite_0_in_scanline: false,
            sprite_px_lo: Vec::new(),
            sprite_px_hi: Vec::new(),

            scanline: 261,
            cycle: 0,

            fine_x: 0,

            nmi: false,

            display: Display::new(),

            bg_shift_pt_low: 0,
            bg_shift_pt_high: 0,
            bg_shift_at_low: 0,
            bg_shift_at_high: 0,
            bg_next_nt: 0,
            bg_next_at: 0,
            bg_next_pt_low: 0,
            bg_next_pt_high: 0,
        }
    }

    /// Read a PPU register from the CPU.
    ///
    /// The PPU exposes 8 registers to the CPU, which are mirrored every 8 bytes.
    /// Expects an address in the range of [0x2000, 0x3FFF] or 0x4014.
    ///
    /// * `cart`: The cart
    /// * `addr`: The memory address
    pub(crate) fn cpu_read_register(&mut self, cart: &mut dyn Cart, addr: u16) -> u8 {
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
                    let read = self.ppudata_read;
                    self.ppudata_read = self.read_mem(cart, self.v.into_bits());
                    self.v = self
                        .v
                        .into_bits()
                        .wrapping_add(self.ppudata_increase())
                        .into();
                    read
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
    pub(crate) fn cpu_write_register(&mut self, cart: &mut dyn Cart, addr: u16, v: u8) {
        debug_assert!((0x2000..0x4000).contains(&addr) || addr == 0x4014);

        if addr == 0x4014 {
            self.oamdma = v;
        } else {
            match (addr - 0x2000) % 8 {
                // Control
                0 => {
                    self.ppuctrl = PpuCtrl::from_bits(v).unwrap();
                    self.t.set_nametable(v & 0x3);
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
                        self.t.set_coarse_x((v >> 3) & 0x1F);
                    } else {
                        // Set coarse y
                        self.t.set_coarse_y((v >> 3) & 0x1F);

                        // Set fine y
                        self.t.set_fine_y(v & 0x7);
                    }
                    self.latch_toggle ^= true;
                }
                // Addr
                6 => {
                    if !self.latch_toggle {
                        let mut t = self.t.into_bits();
                        t &= !(0b11_11111 << 8);
                        t |= (v as u16 & 0b11_1111) << 8;
                        self.t = LoopyRegister::from_bits(t);
                    } else {
                        let mut t = self.t.into_bits();
                        t &= !0xFF;
                        t |= v as u16;
                        self.t = LoopyRegister::from_bits(t);
                        self.v = self.t;
                    }
                    self.latch_toggle ^= true;
                }
                // Data
                7 => {
                    self.write_mem(cart, self.v.into_bits(), v);
                    self.v = self
                        .v
                        .into_bits()
                        .wrapping_add(self.ppudata_increase())
                        .into();
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
        self.t = LoopyRegister::from_bits(0x0000);
        self.latch_toggle = false;

        self.odd = false;
    }

    pub fn cycle(&mut self, cart: &mut dyn Cart, config: &NemuConfig) {
        match self.scanline {
            // Visible scanlines and pre-render scanline
            0..=239 | 261 => {
                // Update background shifter registers
                match self.cycle {
                    2..258 | 321..338 => {
                        self.update_bg_shifters();
                    }
                    _ => {}
                }

                // Update foreground shifter registers
                match self.cycle {
                    2..258 => self.update_fg_shifters(),
                    _ => {}
                }

                if self.scanline != 261 {
                    // Find sprites for line
                    if self.cycle == 257 {
                        self.sprite_scanline.clear();
                        self.sprite_0_in_scanline = false;

                        for i in 0..64 {
                            let &[b0, b1, b2, b3] = &self.oam[i * 4..i * 4 + 4] else {
                                unreachable!();
                            };

                            let y = b0;

                            let diff = self.scanline as i32 - y as i32;
                            let sprite_h = if self.ppuctrl.intersects(PpuCtrl::SPRITE_HEIGHT) {
                                16
                            } else {
                                8
                            };

                            if (0..sprite_h).contains(&diff) {
                                if self.sprite_scanline.len() == 8 {
                                    self.ppustatus |= PpuStatus::SPRITE_OVERFLOW;

                                    if !config.override_sprite_limit {
                                        break;
                                    }
                                }

                                self.sprite_scanline.push([b0, b1, b2, b3]);

                                if i == 0 {
                                    self.sprite_0_in_scanline = true;
                                }
                            }
                        }
                    }

                    // Load sprites
                    if self.cycle == 340 {
                        self.sprite_px_lo.clear();
                        self.sprite_px_hi.clear();

                        for i in 0..self.sprite_scanline.len() {
                            let &[b0, b1, b2, _] = &self.sprite_scanline[i];

                            let attrs = SpriteFlags::from_bits_truncate(b2);

                            let pattern_lo_addr: u16;
                            if self.ppuctrl.intersects(PpuCtrl::SPRITE_HEIGHT) {
                                // 8x16
                                if attrs.intersects(SpriteFlags::FLIP_V) {
                                    if self.scanline.wrapping_sub(b0 as u16) < 8 {
                                        pattern_lo_addr = (((b1 as u16) & 0x01) << 12)
                                            | ((((b1 as u16) & 0xFE) + 1) << 4)
                                            | (7 - (self.scanline.wrapping_sub(b0 as u16) & 0x07));
                                    } else {
                                        pattern_lo_addr = (((b1 as u16) & 0x01) << 12)
                                            | (((b1 as u16) & 0xFE) << 4)
                                            | (7 - (self.scanline.wrapping_sub(b0 as u16) & 0x07));
                                    }
                                } else {
                                    #[allow(clippy::collapsible_if)]
                                    if self.scanline.wrapping_sub(b0 as u16) < 8 {
                                        pattern_lo_addr = (((b1 as u16) & 0x01) << 12)
                                            | (((b1 as u16) & 0xFE) << 4)
                                            | ((self.scanline.wrapping_sub(b0 as u16)) & 0x07);
                                    } else {
                                        pattern_lo_addr = (((b1 as u16) & 0x01) << 12)
                                            | ((((b1 as u16) & 0xFE) + 1) << 4)
                                            | ((self.scanline.wrapping_sub(b0 as u16)) & 0x07);
                                    }
                                }
                            } else {
                                // 8x8

                                if attrs.intersects(SpriteFlags::FLIP_V) {
                                    pattern_lo_addr =
                                        ((self.ppuctrl.intersects(PpuCtrl::SPRITE_TILE) as u16)
                                            << 12)
                                            | ((b1 as u16) << 4)
                                            | (7 - self.scanline.wrapping_sub(b0 as u16));
                                } else {
                                    pattern_lo_addr =
                                        ((self.ppuctrl.intersects(PpuCtrl::SPRITE_TILE) as u16)
                                            << 12)
                                            | ((b1 as u16) << 4)
                                            | self.scanline.wrapping_sub(b0 as u16);
                                }
                            }

                            let pattern_hi_addr = pattern_lo_addr + 8;

                            let mut bits_lo = self.read_mem(cart, pattern_lo_addr);
                            let mut bits_hi = self.read_mem(cart, pattern_hi_addr);

                            if attrs.intersects(SpriteFlags::FLIP_H) {
                                bits_lo = bits_lo.reverse_bits();
                                bits_hi = bits_hi.reverse_bits();
                            }

                            self.sprite_px_lo.push(bits_lo);
                            self.sprite_px_hi.push(bits_hi);
                        }
                    }
                }

                let ordinary_bg_fetch = |ppu: &mut Ppu, cart, cycle| match cycle % 8 {
                    1 => {
                        ppu.load_shifters();
                        ppu.bg_next_nt = ppu.fetch_nt(cart)
                    }
                    3 => ppu.bg_next_at = ppu.fetch_at(cart),
                    5 => ppu.bg_next_pt_low = ppu.fetch_pt_low(cart),
                    7 => ppu.bg_next_pt_high = ppu.fetch_pt_high(cart),
                    _ => {}
                };

                let bg_garbage_nts = |ppu: &mut Ppu, cart, cycle| match cycle % 8 {
                    1 | 3 => {
                        ppu.bg_next_nt = ppu.fetch_nt(cart);
                    }
                    _ => {}
                };

                // Fetches
                match self.cycle {
                    1..=256 => ordinary_bg_fetch(self, cart, self.cycle),
                    257..=320 => bg_garbage_nts(self, cart, self.cycle),
                    321..=336 => ordinary_bg_fetch(self, cart, self.cycle),
                    337..=340 => bg_garbage_nts(self, cart, self.cycle),
                    _ => {}
                }

                // Scroll
                // y scroll
                match self.cycle {
                    256 => self.inc_fine_y(),
                    280..=304 if self.scanline == 261 => self.transfer_y(),
                    _ => {}
                }
                // x scroll
                match self.cycle {
                    8..=256 if self.cycle % 8 == 0 => self.inc_coarse_x(),
                    257 => self.transfer_coarse_x(),
                    328 | 336 => self.inc_coarse_x(),
                    _ => {}
                }

                // Pixel
                if self.scanline != 261 && (1..=256).contains(&self.cycle) {
                    let (bg_px, bg_pal) = {
                        let bit_mux = 0x8000 >> self.fine_x;

                        let px_low = ((self.bg_shift_pt_low & bit_mux) != 0) as u8;
                        let px_high = ((self.bg_shift_pt_high & bit_mux) != 0) as u8;
                        let px = (px_high << 1) | px_low;

                        let at_low = ((self.bg_shift_at_low & bit_mux) != 0) as u8;
                        let at_high = ((self.bg_shift_at_high & bit_mux) != 0) as u8;
                        let pal = (at_high << 1) | at_low;

                        let bg_disable = !self.ppumask.intersects(PpuMask::BACKGROUND_ENABLE)
                            || ((1..=8).contains(&self.cycle)
                                && !self.ppumask.intersects(PpuMask::BACKGROUND_LEFT_ENABLE));

                        if bg_disable {
                            (0, 0)
                        } else {
                            (px, pal)
                        }
                    };

                    let mut sprite_0_rendered = false;

                    let (fg_px, fg_pal, fg_prio) = {
                        let mut fg_px = 0;
                        let mut fg_pal = 0;
                        let mut fg_prio = false;

                        for i in 0..self.sprite_scanline.len() {
                            if self.sprite_scanline[i][3] == 0 {
                                let px_lo = (self.sprite_px_lo[i] & 0x80 != 0) as u8;
                                let px_hi = (self.sprite_px_hi[i] & 0x80 != 0) as u8;
                                fg_px = (px_hi << 1) | px_lo;
                                let attrs =
                                    SpriteFlags::from_bits_truncate(self.sprite_scanline[i][2]);
                                fg_pal = attrs.intersection(SpriteFlags::PALETTE).bits() + 0x04;
                                fg_prio = !attrs.intersects(SpriteFlags::PRIO);

                                if fg_px != 0 {
                                    if i == 0 {
                                        sprite_0_rendered = true;
                                    }

                                    break;
                                }
                            }
                        }

                        let fg_disable = !self.ppumask.intersects(PpuMask::SPRITE_ENABLE)
                            || (1..=8).contains(&self.cycle)
                                && !self.ppumask.intersects(PpuMask::SPRITE_LEFT_ENABLE);

                        if fg_disable {
                            (0, 0, false)
                        } else {
                            (fg_px, fg_pal, fg_prio)
                        }
                    };

                    let (px, pal) = match (bg_px, fg_px) {
                        (0, 0) => (0, 0),
                        (0, _) => (fg_px, fg_pal),
                        (_, 0) => (bg_px, bg_pal),
                        (_, _) => {
                            // Update sprite 0 hit detection
                            if self.sprite_0_in_scanline && sprite_0_rendered {
                                self.ppustatus |= PpuStatus::SPRITE_0_HIT;
                            }

                            // Get the prioritised pixel
                            if fg_prio {
                                (fg_px, fg_pal)
                            } else {
                                (bg_px, bg_pal)
                            }
                        }
                    };

                    self.display.set_pixel(
                        self.cycle as usize - 1,
                        self.scanline as usize,
                        self.get_palette_color(pal, px),
                    );
                }
            }

            // Post-render scanline, idle
            240 => {}
            // Vertical blanking lines
            241..=260 => {}
            // Pre-render scanline
            _ => unreachable!(),
        }

        // Resets
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
                self.sprite_scanline.clear();
                self.sprite_px_lo.clear();
                self.sprite_px_hi.clear();
            }
            _ => {}
        }

        if self
            .ppumask
            .intersects(PpuMask::BACKGROUND_ENABLE | PpuMask::SPRITE_ENABLE)
            && self.cycle == 260
            && self.scanline < 240
        {
            cart.end_of_scanline();
        }

        self.cycle += 1;
        if self.cycle > 340 {
            self.cycle = 0;
            self.scanline += 1;

            if self.scanline > 261 {
                self.display.clear();

                self.scanline = 0;

                self.odd = !self.odd;

                // Skip (0, 0) on odd cycles
                if self.odd {
                    self.cycle = 1
                };
            }
        }
    }

    pub fn inspect_mem(&self, cart: &dyn Cart, addr: u16) -> u8 {
        let addr = addr % 0x4000;

        macro_rules! read_nametable {
            ($n:expr, $addr:expr) => {{
                match cart.mirroring() {
                    crate::carts::Mirroring::Horizontal => match $n {
                        0 => self.vram[$addr as usize],
                        1 => self.vram[$addr as usize],
                        2 => self.vram[0x400 + $addr as usize],
                        3 => self.vram[0x400 + $addr as usize],
                        _ => unreachable!(),
                    },
                    crate::carts::Mirroring::Vertical => match $n {
                        0 => self.vram[$addr as usize],
                        1 => self.vram[0x400 + $addr as usize],
                        2 => self.vram[$addr as usize],
                        3 => self.vram[0x400 + $addr as usize],
                        _ => unreachable!(),
                    },
                    crate::carts::Mirroring::Single => self.vram[$addr as usize],
                }
            }};
        }

        match addr {
            // Pattern tables
            0x0000..0x2000 => cart.ppu_inspect(addr),
            // Name tables
            0x2000..=0x23FF => read_nametable!(0, addr - 0x2000),
            0x2400..=0x27FF => read_nametable!(1, addr - 0x2400),
            0x2800..=0x2BFF => read_nametable!(2, addr - 0x2800),
            0x2C00..=0x2FFF => read_nametable!(3, addr - 0x2C00),
            // Unused address, do nothing for now
            // TODO: Mapped by cartridge
            0x3000..=0x3EFF => 0,
            // Palette ram indexes, mirrored every 0x20 values
            0x3F00..=0x3FFF => {
                if addr % 0x10 == 0 {
                    self.palette[0]
                } else {
                    self.palette[(addr % 0x20) as usize]
                }
            }

            _ => unreachable!(),
        }
    }

    fn read_mem(&self, cart: &mut dyn Cart, addr: u16) -> u8 {
        let addr = addr % 0x4000;
        match addr {
            0x0000..0x2000 => cart.ppu_read(addr),
            _ => self.inspect_mem(cart, addr),
        }
    }

    fn write_mem(&mut self, cart: &mut dyn Cart, addr: u16, v: u8) {
        let addr = addr % 0x4000;

        macro_rules! write_nametable {
            ($n:expr, $addr:expr, $v: expr) => {{
                let p = match cart.mirroring() {
                    crate::carts::Mirroring::Horizontal => match $n {
                        0 => &mut self.vram[$addr as usize],
                        1 => &mut self.vram[$addr as usize],
                        2 => &mut self.vram[0x400 + $addr as usize],
                        3 => &mut self.vram[0x400 + $addr as usize],
                        _ => unreachable!(),
                    },
                    crate::carts::Mirroring::Vertical => match $n {
                        0 => &mut self.vram[$addr as usize],
                        1 => &mut self.vram[0x400 + $addr as usize],
                        2 => &mut self.vram[$addr as usize],
                        3 => &mut self.vram[0x400 + $addr as usize],
                        _ => unreachable!(),
                    },
                    crate::carts::Mirroring::Single => &mut self.vram[$addr as usize],
                };
                *p = v;
            }};
        }

        match addr {
            // Pattern tables
            0x0000..0x2000 => cart.ppu_write(addr % 0x4000, v),
            // Name tables
            0x2000..=0x23FF => write_nametable!(0, addr - 0x2000, v),
            0x2400..=0x27FF => write_nametable!(1, addr - 0x2400, v),
            0x2800..=0x2BFF => write_nametable!(2, addr - 0x2800, v),
            0x2C00..=0x2FFF => write_nametable!(3, addr - 0x2C00, v),
            // Unused address, do nothing for now
            // TODO: Mapped by cartridge
            0x3000..=0x3EFF => {}
            // Palette ram indexes, mirrored every 0x20 values
            0x3F00..=0x3FFF => {
                if addr % 0x10 == 0 {
                    self.palette[0] = v;
                } else {
                    self.palette[(addr % 0x20) as usize] = v;
                }
            }

            _ => {
                unreachable!()
            }
        }
    }

    pub fn display(&self) -> &Display {
        &self.display
    }

    fn load_shifters(&mut self) {
        self.bg_shift_pt_low = (self.bg_shift_pt_low & 0xff00) | self.bg_next_pt_low as u16;
        self.bg_shift_pt_high = (self.bg_shift_pt_high & 0xff00) | self.bg_next_pt_high as u16;
        self.bg_shift_at_low =
            (self.bg_shift_at_low & 0xff00) | if self.bg_next_at & 1 != 0 { 0xff } else { 0x0 };
        self.bg_shift_at_high =
            (self.bg_shift_at_high & 0xff00) | if self.bg_next_at & 2 != 0 { 0xff } else { 0x0 };
    }

    fn update_bg_shifters(&mut self) {
        if !self.ppumask.intersects(PpuMask::BACKGROUND_ENABLE) {
            return;
        }

        self.bg_shift_pt_low <<= 1;
        self.bg_shift_pt_high <<= 1;
        self.bg_shift_at_low <<= 1;
        self.bg_shift_at_high <<= 1;
    }

    fn update_fg_shifters(&mut self) {
        if !self.ppumask.intersects(PpuMask::SPRITE_ENABLE) {
            return;
        }

        for i in 0..self.sprite_scanline.len() {
            if self.sprite_scanline[i][3] > 0 {
                self.sprite_scanline[i][3] -= 1;
            } else {
                self.sprite_px_lo[i] <<= 1;
                self.sprite_px_hi[i] <<= 1;
            }
        }
    }

    fn fetch_nt(&mut self, cart: &mut dyn Cart) -> u8 {
        let nt_addr = 0x2000 | (self.v.into_bits() & 0x0FFF);
        self.read_mem(cart, nt_addr)
    }

    fn fetch_at(&mut self, cart: &mut dyn Cart) -> u8 {
        let attr_addr: u16 = 0x23C0
            | ((self.v.nametable() as u16) << 10)
            | ((self.v.coarse_y() as u16 & 0b1_1100) << 1)
            | ((self.v.coarse_x() as u16 & 0b1_1100) >> 2);
        let mut attr = self.read_mem(cart, attr_addr);
        if self.v.coarse_y() & 0x02 != 0 {
            attr >>= 4;
        }
        if self.v.coarse_x() & 0x02 != 0 {
            attr >>= 2;
        }
        attr &= 0x03;
        attr
    }

    fn fetch_pt_low(&mut self, cart: &mut dyn Cart) -> u8 {
        self.read_mem(
            cart,
            ((self.ppuctrl.intersects(PpuCtrl::BACKGROUND_TILE) as u16) << 12)
                | ((self.bg_next_nt as u16) << 4)
                | (self.v.fine_y() as u16 + 0),
        )
    }

    fn fetch_pt_high(&mut self, cart: &mut dyn Cart) -> u8 {
        self.read_mem(
            cart,
            ((self.ppuctrl.intersects(PpuCtrl::BACKGROUND_TILE) as u16) << 12)
                | ((self.bg_next_nt as u16) << 4)
                | (self.v.fine_y() as u16 + 8),
        )
    }

    fn inc_fine_y(&mut self) {
        if !self
            .ppumask
            .intersects(PpuMask::BACKGROUND_ENABLE | PpuMask::SPRITE_ENABLE)
        {
            return;
        }

        if self.v.fine_y() < 7 {
            self.v.set_fine_y((self.v.fine_y() + 1) & 0x7);
        } else {
            self.v.set_fine_y(0);

            match self.v.coarse_y() {
                29 => {
                    self.v.set_coarse_y(0);
                    self.v.set_nametable(self.v.nametable() ^ 0x2);
                }
                31 => {
                    self.v.set_coarse_y(0);
                }
                _ => {
                    self.v.set_coarse_y(self.v.coarse_y() + 1);
                }
            }
        }
    }

    fn inc_coarse_x(&mut self) {
        if !self
            .ppumask
            .intersects(PpuMask::BACKGROUND_ENABLE | PpuMask::SPRITE_ENABLE)
        {
            return;
        }

        match self.v.coarse_x() {
            31 => {
                self.v.set_coarse_x(0);
                self.v.set_nametable(self.v.nametable() ^ 0x1);
            }
            _ => {
                self.v.set_coarse_x(self.v.coarse_x() + 1);
            }
        }
    }

    pub fn fine_x(&self) -> u8 {
        self.fine_x
    }

    /// Get a palette
    ///
    /// * `i`: Palette index, BG: 0-3, FG: 4-7
    fn get_palette(&self, i: u8) -> &[u8; 4] {
        (&self.palette[i as usize * 4..i as usize * 4 + 4])
            .try_into()
            .unwrap()
    }

    /// Get a color from the palette
    ///
    /// * `palette_i`: The palette index, 0-7
    /// * `color`: The color in the palette, 0-3
    fn get_palette_color(&self, palette_i: u8, color: u8) -> (u8, u8, u8) {
        let palette = self.get_palette(palette_i);
        let c = palette[color as usize] as usize & 0x3F;
        let &[r, g, b] = &PALETTE[c * 3..c * 3 + 3] else {
            unreachable!()
        };
        (r, g, b)
    }

    fn transfer_coarse_x(&mut self) {
        if !self
            .ppumask
            .intersects(PpuMask::BACKGROUND_ENABLE | PpuMask::SPRITE_ENABLE)
        {
            return;
        }

        self.v
            .set_nametable(self.v.nametable() & 0x2 | self.t.nametable() & 0x1);
        self.v.set_coarse_x(self.t.coarse_x());
    }

    fn transfer_y(&mut self) {
        if !self
            .ppumask
            .intersects(PpuMask::BACKGROUND_ENABLE | PpuMask::SPRITE_ENABLE)
        {
            return;
        }

        self.v
            .set_nametable(self.t.nametable() & 0x2 | self.v.nametable() & 0x1);
        self.v.set_fine_y(self.t.fine_y());
        self.v.set_coarse_y(self.t.coarse_y());
    }
}

#[derive(Clone, bincode::Encode, bincode::Decode)]
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

    pub fn clear(&mut self) {
        self.pixels.fill(0);
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
    use crate::carts::reader::read_rom;

    use super::Ppu;

    #[test]
    fn ppu_scroll_registers() {
        // Test taken from https://www.nesdev.org/wiki/PPU_scrolling#Summary
        let mut ppu = Ppu::new();
        let mut cart = read_rom(include_bytes!("../res/nes-test-roms/other/nestest.nes")).unwrap();

        // $2000 write
        ppu.cpu_write_register(&mut cart, 0x2000, 0b00000000);
        assert!(ppu.t.into_bits() & (0b11 << 10) == 0);

        // $2002 read
        ppu.cpu_read_register(&mut cart, 0x2002);
        assert!(!ppu.latch_toggle);

        // $2005 write 1
        ppu.cpu_write_register(&mut cart, 0x2005, 0b01111101);
        assert!(ppu.t.into_bits() & 0b11000011111 == 0b1111);
        assert!(ppu.fine_x == 0b101);
        assert!(ppu.latch_toggle);

        // $2005 write 2
        ppu.cpu_write_register(&mut cart, 0x2005, 0b01011110);
        assert!(ppu.t.into_bits() == 0b01100001_01101111);
        assert!(!ppu.latch_toggle);

        // $2006 write 1
        ppu.cpu_write_register(&mut cart, 0x2006, 0b00111101);
        assert!(ppu.t.into_bits() == 0b00111101_01101111);
        assert!(ppu.latch_toggle);

        // $2006 write 2
        ppu.cpu_write_register(&mut cart, 0x2006, 0b11110000);
        assert!(ppu.t.into_bits() == 0b00111101_11110000);
        assert!(ppu.v.into_bits() == 0b00111101_11110000);
        assert!(!ppu.latch_toggle);
    }
}
