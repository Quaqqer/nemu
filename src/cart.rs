pub struct Cart {
    pub prg_pages: u8,
    pub prg: Vec<u8>,
    pub prg_ram: Vec<u8>,
    pub chr: Vec<u8>,
    pub mapper: Mapper,
}

enum Mirroring {
    Horizontal,
    Vertical,
}

enum TvSystem {
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
        let _vertical_mirroring = flags_6 & 1 != 0;
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
        }
    }

    pub fn read8(&self, addr: u16) -> u8 {
        self.mapper.read8(self, addr)
    }

    pub fn write8(&mut self, addr: u16, val: u8) {
        let mapper = self.mapper;
        mapper.write8(self, addr, val);
    }
}

#[derive(Clone, Copy)]
pub enum Mapper {
    NES1_0,
}

impl Mapper {
    pub fn read8(&self, cart: &Cart, addr: u16) -> u8 {
        match self {
            Mapper::NES1_0 => match addr {
                0x0..=0x5FFF => 0,
                0x6000..=0x7FFF => cart.prg_ram[(addr as usize - 0x6000) % cart.prg_ram.len()],
                0x8000..=0xFFFF => cart.prg[(addr as usize - 0x8000) % cart.prg.len()],
            },
        }
    }

    pub fn write8(&self, cart: &mut Cart, addr: u16, val: u8) {
        match self {
            Mapper::NES1_0 => match addr {
                0x6000..=0x7FFF => {
                    let prg_ram_size = cart.prg_ram.len();
                    cart.prg_ram[(addr as usize - 0x6000) % prg_ram_size] = val;
                }
                _ => {}
            },
        }
    }

    fn from_number(n: u8) -> Self {
        match n {
            0 => Self::NES1_0,
            _ => todo!("Mapper number {} is not implemented yet", n),
        }
    }
}
