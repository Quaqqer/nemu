use crate::ppu::Ppu;

pub struct Cart {
    pub prg_pages: u8,
    pub prg: Vec<u8>,
    pub prg_ram: Vec<u8>,
    pub chr: Vec<u8>,
    pub mapper: Mapper,
    pub mirroring: Mirroring,
}

pub enum Mirroring {
    Horizontal,
    Vertical,
}

pub enum TvSystem {
    NTSC,
    PAL,
    Dual,
}

impl Cart {
    pub fn read_ines1_0(path: &str) -> Self {
        let bin = std::fs::read(path).unwrap();
        assert!(bin[0..4] == [0x4E, 0x45, 0x53, 0x1A]);
        let prg_pages: u8 = bin[4];
        let chr_size: usize = bin[5] as usize * 8192;

        let flags_6 = bin[6];
        let vertical_mirroring = flags_6 & 1 != 0;

        let mirroring = if vertical_mirroring {
            Mirroring::Vertical
        } else {
            Mirroring::Horizontal
        };

        let _has_battery_prg_ram = flags_6 & (1 << 1) != 0;
        let has_trainer = flags_6 & (1 << 2) != 0;
        let _ignore_mirroring_control = flags_6 & (1 << 3) != 0;

        let flags_7 = bin[7];

        let mapper_n = (flags_6 << 4) + ((flags_7 & 0xf0) >> 4);
        let mapper = Mapper::from_number(mapper_n);

        let flags_8 = bin[8];
        let prg_ram_size = flags_8.max(1) as usize * 8192;
        let prg_ram = vec![0; prg_ram_size];

        let flags_9 = bin[9];
        let _pal = flags_9 & 1 != 0;

        let flags_10 = bin[10];
        let _tv_system = match flags_10 & 0b11 {
            0 => TvSystem::NTSC,
            2 => TvSystem::PAL,
            _ => TvSystem::Dual,
        };
        let _unused_padding = &bin[11..16];

        let mut ptr: usize = 16;

        if has_trainer {
            ptr += 512;
        }

        let prg_size = prg_pages as usize * 16384;
        let prg = bin[ptr..ptr + prg_size].to_vec();
        ptr += prg_size;

        let chr = bin[ptr..ptr + chr_size].to_vec();
        #[allow(unused_assignments)]
        let _ = ptr += chr_size;

        Self {
            prg_pages,
            prg,
            chr,
            mapper,
            prg_ram,
            mirroring,
        }
    }

    pub fn read8(&self, addr: u16) -> u8 {
        match self.mapper {
            Mapper::NES1_0 => match addr {
                0x0..=0x5FFF => 0,
                0x6000..=0x7FFF => self.prg_ram[(addr as usize - 0x6000) % self.prg_ram.len()],
                0x8000..=0xFFFF => self.prg[(addr as usize - 0x8000) % self.prg.len()],
            },
        }
    }

    pub fn write8(&mut self, addr: u16, val: u8) {
        match self.mapper {
            Mapper::NES1_0 => match addr {
                0x6000..=0x7FFF => {
                    let prg_ram_size = self.prg_ram.len();
                    self.prg_ram[(addr as usize - 0x6000) % prg_ram_size] = val;
                }
                _ => {}
            },
        }
    }

    pub(crate) fn reset(&self) {
        todo!()
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
        debug_assert!(sprite_x <= 0xF);
        debug_assert!(sprite_y <= 0xF);

        self.get_sprite_i(page, sprite_y * 8 + sprite_x)
    }

    pub fn get_sprite_i(&self, page: u8, sprite_i: u8) -> &[u8] {
        debug_assert!(page <= 1);

        let base = page as usize * 0x1000 + sprite_i as usize * 16;
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
        self.get_sprite_i_pixel(page, sprite_y * 8 + sprite_x, x_offset, y_offset)
    }

    pub fn get_sprite_i_pixel(&self, page: u8, sprite_i: u8, x_offset: u8, y_offset: u8) -> u8 {
        let sprite = self.get_sprite_i(page, sprite_i);
        let l = (sprite[y_offset as usize] >> (7 - x_offset)) & 1;
        let r = (sprite[y_offset as usize + 8] >> (7 - x_offset)) & 1;
        (r << 1) | l
    }

    pub fn read_pattern_table(&mut self, _ppu: &mut Ppu, pattern_table: u8, addr: u16) -> u8 {
        debug_assert!(addr < 0x1000);
        debug_assert!(pattern_table <= 1);

        self.chr[0x1000 * pattern_table as usize + addr as usize]
    }

    pub fn write_pattern_table(&mut self, _ppu: &mut Ppu, pattern_table: u8, addr: u16, v: u8) {
        // TODO: Shouldn't always be readable?
        debug_assert!(addr < 0x1000);
        debug_assert!(pattern_table <= 1);

        self.chr[0x1000 * pattern_table as usize + addr as usize] = v
    }

    pub fn read_nametable_tile(
        &mut self,
        ppu: &mut Ppu,
        nametable: u8,
        tile_x: u8,
        tile_y: u8,
    ) -> u8 {
        self.read_nametable(ppu, nametable, tile_y as u16 * 32 + tile_x as u16)
    }

    pub fn read_nametable(&mut self, ppu: &mut Ppu, nametable: u8, addr: u16) -> u8 {
        debug_assert!(addr < 0x400);
        debug_assert!(nametable <= 3);

        match self.mirroring {
            Mirroring::Horizontal => match nametable {
                0 => ppu.vram[addr as usize],
                1 => ppu.vram[0x400 + addr as usize],
                2 => ppu.vram[addr as usize],
                3 => ppu.vram[0x400 + addr as usize],
                _ => unreachable!(),
            },
            Mirroring::Vertical => match nametable {
                0 => ppu.vram[addr as usize],
                1 => ppu.vram[addr as usize],
                2 => ppu.vram[0x400 + addr as usize],
                3 => ppu.vram[0x400 + addr as usize],
                _ => unreachable!(),
            },
        }
    }

    pub fn inspect_nametable(&self, ppu: &Ppu, nametable: u8, addr: u16) -> u8 {
        debug_assert!(addr < 0x400);
        debug_assert!(nametable <= 3);

        match self.mirroring {
            Mirroring::Horizontal => match nametable {
                0 => ppu.vram[addr as usize],
                1 => ppu.vram[0x400 + addr as usize],
                2 => ppu.vram[addr as usize],
                3 => ppu.vram[0x400 + addr as usize],
                _ => unreachable!(),
            },
            Mirroring::Vertical => match nametable {
                0 => ppu.vram[addr as usize],
                1 => ppu.vram[addr as usize],
                2 => ppu.vram[0x400 + addr as usize],
                3 => ppu.vram[0x400 + addr as usize],
                _ => unreachable!(),
            },
        }
    }

    pub fn write_nametable(&mut self, ppu: &mut Ppu, nametable: u8, addr: u16, v: u8) {
        debug_assert!(addr < 0x400);
        debug_assert!(nametable <= 3);

        match self.mirroring {
            Mirroring::Horizontal => match nametable {
                0 => ppu.vram[addr as usize] = v,
                2 => ppu.vram[addr as usize] = v,
                1 => ppu.vram[0x400 + addr as usize] = v,
                3 => ppu.vram[0x400 + addr as usize] = v,
                _ => unreachable!(),
            },
            Mirroring::Vertical => match nametable {
                0 => ppu.vram[addr as usize] = v,
                2 => ppu.vram[0x400 + addr as usize] = v,
                1 => ppu.vram[addr as usize] = v,
                3 => ppu.vram[0x400 + addr as usize] = v,
                _ => unreachable!(),
            },
        }
    }
    //
    // pub fn read_attribute(&mut self, ppu: &mut Ppu, tile_i: u16) ->
}

#[derive(Clone, Copy)]
pub enum Mapper {
    NES1_0,
}

impl Mapper {
    fn from_number(n: u8) -> Self {
        match n {
            0 => Self::NES1_0,
            _ => todo!("Mapper number {} is not implemented yet", n),
        }
    }
}
