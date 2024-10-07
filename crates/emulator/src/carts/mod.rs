use bitfield_struct::bitfield;
use nrom::NROM;

mod nrom;

#[derive(Clone, Debug)]
struct INesHeader {
    prg_rom_size: usize,
    chr_rom_size: usize,
}

#[bitfield(u128)]
struct INes1Header {
    magic_bytes: u32,
    // PRG ROM size in 16 KiB units
    prg_rom_n: u8,
    // CHR ROM size in 8 KiB units
    chr_rom_n: u8,
    // Flags 6
    horizontal_mirroring: bool,
    battery_prg_ram: bool,
    trainer: bool,
    _alternate_nametable: bool,
    #[bits(4)]
    mapper_number_lower: u8,
    // Flags 7
    vs_unisystem: bool,
    playchoice_10: bool,
    // If Nes2 == 2 flags 8-15 are in NES2 format
    #[bits(2)]
    nes2: u8,
    #[bits(4)]
    mapper_number_upper: u8,
    // Flags 8
    // PRG ram size in 8 KiB units
    prg_ram_size: u8,
    // Flags 9
    pal: bool,
    #[bits(7)]
    _reserved: u8,
    // Flags 10
    // TV System (0: NTSC, 2: PAL, 1/3: Dual)
    #[bits(2)]
    tv_system: u8,
    #[bits(2)]
    _unused1: u8,
    prg_ram_present: bool,
    bus_conflicts: bool,
    #[bits(2)]
    _unused2: u8,
    // Unused bytes
    #[bits(40)]
    _padding: u64,
}

pub fn read_rom(bin: &[u8]) -> Option<Cart> {
    // Read Nes1 format
    let flags = INes1Header::from_bits(u128::from_le_bytes(
        TryInto::<[u8; 16]>::try_into(&bin[0..16]).ok()?,
    ));

    if flags.nes2() == 2 {
        // This is NES2 format
        eprintln!("NES2 format not implemented yet");
        return None;
    } else {
        let mut i = 16;

        let _trainer = if flags.trainer() {
            let v = Some(Vec::from(bin.get(i..i + 512)?));
            i += 512;
            v
        } else {
            None
        };

        let prg_rom_size = flags.prg_rom_n() as usize * 0x4000;
        let prg_rom = Vec::from(bin.get(i..i + prg_rom_size)?);
        i += prg_rom_size;

        let chr_rom_size = flags.chr_rom_n() as usize * 0x2000;
        let chr_rom = Vec::from(bin.get(i..i + chr_rom_size)?);
        i += prg_rom_size;

        if flags.playchoice_10() {
            eprintln!("Playchoice 10 is unsupported");
            return None;
        }

        let mirroring = match !flags.horizontal_mirroring() {
            true => Mirroring::Horizontal,
            false => Mirroring::Vertical,
        };

        let mapper_number = flags.mapper_number_upper() << 4 | flags.mapper_number_lower();

        let mapper = match mapper_number {
            0 => Mapper::NROM,
            _ => {
                eprintln!("Mapper number {} not implemented", mapper_number);
                return None;
            }
        };

        let prg_ram = vec![0x00; 0x2000];
        let chr_ram = Vec::new();

        Some(Cart {
            mapper,
            mirroring,
            prg_rom,
            prg_ram,
            chr_rom,
            chr_ram,
        })
    }
}

#[derive(Clone)]
pub struct Cart {
    pub mapper: Mapper,
    pub mirroring: Mirroring,
    pub prg_rom: Vec<u8>,
    pub prg_ram: Vec<u8>,
    pub chr_rom: Vec<u8>,
    pub chr_ram: Vec<u8>,
}

#[derive(Clone, Copy)]
enum Mapper {
    NROM,
}

impl Cart {
    pub fn cpu_read(&mut self, addr: u16) -> u8 {
        match self.mapper {
            Mapper::NROM => NROM::new().cpu_read(self, addr),
        }
    }

    pub fn cpu_inspect(&self, addr: u16) -> u8 {
        match self.mapper {
            Mapper::NROM => NROM::new().cpu_inspect(self, addr),
        }
    }

    pub fn cpu_write(&mut self, addr: u16, v: u8) {
        match self.mapper {
            Mapper::NROM => NROM::new().cpu_write(self, addr, v),
        }
    }

    pub fn ppu_read(&mut self, addr: u16) -> u8 {
        match self.mapper {
            Mapper::NROM => NROM::new().ppu_read(self, addr),
        }
    }

    pub fn ppu_write(&mut self, addr: u16, v: u8) {
        match self.mapper {
            Mapper::NROM => NROM::new().ppu_write(self, addr, v),
        }
    }

    pub fn reset(&mut self) {}
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Mirroring {
    Horizontal,
    Vertical,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TvSystem {
    NTSC,
    PAL,
    Dual,
}

trait MapperTrait {
    fn cpu_read(&self, cart: &mut Cart, addr: u16) -> u8;
    fn cpu_inspect(&self, cart: &Cart, addr: u16) -> u8;
    fn cpu_write(&self, cart: &mut Cart, addr: u16, v: u8);
    fn ppu_read(&self, cart: &mut Cart, addr: u16) -> u8;
    fn ppu_write(&self, cart: &mut Cart, addr: u16, v: u8);
    fn reset(&self, cart: &mut Cart);
}
