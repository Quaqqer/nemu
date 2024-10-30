use bitfield_struct::bitfield;
use nrom::NROM;

mod nrom;

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

#[bitfield(u128)]
struct INes2Header {
    // NES header 'NES<eof>'
    magic_bytes: u32,

    // PRG ROM size in 16 KiB units
    prg_rom_lsb: u8,

    // CHR ROM size in 8 KiB units
    chr_rom_lsb: u8,

    // Flags 6
    vertical_mirroring: bool,
    battery: bool,
    trainer: bool,
    _alternate_nametable: bool,
    #[bits(4)]
    mapper_number_nibble1: u8,

    // Flags 7
    #[bits(2)]
    console_type: u8,
    // If Nes2 == 2 flags 8-15 are in NES2 format
    #[bits(2)]
    nes2: u8,
    #[bits(4)]
    mapper_number_nibble2: u8,

    // Flags 8
    #[bits(4)]
    mapper_number_nibble3: u8,
    #[bits(4)]
    submapper: u8,

    // Flags 9
    #[bits(4)]
    prg_rom_msb: u8,
    #[bits(4)]
    chr_rom_msb: u8,

    // Flags 10
    #[bits(4)]
    prg_ram_shift: u8,
    #[bits(4)]
    prg_nvram_shift: u8,

    // Flags 11
    #[bits(4)]
    chr_ram_shift: u8,
    #[bits(4)]
    chr_nvram_shift: u8,

    // Flags 12
    #[bits(2)]
    cpu_timing: u8,
    #[bits(6)]
    _unused: u8,

    // Flags 13
    // Used when the system is a VS system
    #[bits(4)]
    ppu_type: u8,
    #[bits(4)]
    hardware_type: u8,

    // Flags 14
    #[bits(2)]
    misc_roms: u8,
    #[bits(6)]
    _unused: u8,

    // Flags 15
    #[bits(6)]
    default_extension_device: u8,
    #[bits(2)]
    _unused: u8,
}

pub fn read_rom(bin: &[u8]) -> Option<Box<dyn Cart>> {
    let header_bytes = u128::from_le_bytes(TryInto::<[u8; 16]>::try_into(&bin[0..16]).ok()?);

    // Read Nes1 format
    let header = INes1Header::from_bits(header_bytes);

    if header.nes2() == 2 {}

    if header.nes2() == 2 {
        let header = INes2Header::from_bits(header_bytes);
        read_ines2(bin, header)
    } else {
        read_ines1(bin, header)
    }
}

fn read_ines1(bin: &[u8], header: INes1Header) -> Option<Box<dyn Cart>> {
    let mut i = 16;

    let _trainer = if header.trainer() {
        let v = Some(Vec::from(bin.get(i..i + 512)?));
        i += 512;
        v
    } else {
        None
    };

    let prg_rom_size = header.prg_rom_n() as usize * 0x4000;
    let prg_rom = Vec::from(bin.get(i..i + prg_rom_size)?);
    i += prg_rom_size;

    let chr_rom_size = header.chr_rom_n() as usize * 0x2000;
    let chr_rom = Vec::from(bin.get(i..i + chr_rom_size)?);
    i += prg_rom_size;

    if header.playchoice_10() {
        eprintln!("Playchoice 10 is unsupported");
        return None;
    }

    let mirroring = match !header.horizontal_mirroring() {
        true => Mirroring::Horizontal,
        false => Mirroring::Vertical,
    };

    let mapper_number = header.mapper_number_upper() << 4 | header.mapper_number_lower();

    Some(match mapper_number {
        0 => Box::new(NROM {
            mirroring,
            prg_rom,
            prg_ram: vec![0x00; 0x2000],
            chr_rom,
        }),
        _ => {
            eprintln!("Mapper number {} not implemented", mapper_number);
            return None;
        }
    })
}

fn read_ines2(bin: &[u8], header: INes2Header) -> Option<Box<dyn Cart>> {
    eprintln!("iNES 2 roms not supported yet");
    None
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

pub trait Cart {
    fn mirroring(&self) -> Mirroring;
    fn cpu_read(&mut self, addr: u16) -> u8;
    fn cpu_inspect(&self, addr: u16) -> u8;
    fn cpu_write(&mut self, addr: u16, v: u8);
    fn ppu_read(&mut self, addr: u16) -> u8;
    fn ppu_write(&mut self, addr: u16, v: u8);
    fn reset(&mut self);
    fn box_cloned(&self) -> Box<dyn Cart>;
}
