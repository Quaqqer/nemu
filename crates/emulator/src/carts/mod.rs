use anyhow::{anyhow, bail, ensure, Result};
use bitfield_struct::bitfield;
use cnrom::CNROM;
use mmc1::MMC1;
use nrom::NROM;
use uxrom::UxROM;

mod cnrom;
mod mmc1;
mod nrom;
mod uxrom;

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
struct Nes2Header {
    // NES header 'NES<eof>'
    magic_bytes: u32,

    // PRG ROM size in 16 KiB units
    prg_rom_lsb: u8,

    // CHR ROM size in 8 KiB units
    chr_rom_lsb: u8,

    // Flags 6
    horizontal_mirroring: bool,
    battery: bool,
    trainer: bool,
    _alternate_nametable: bool,
    #[bits(4)]
    mapper_number_nibble1: u16,

    // Flags 7
    #[bits(2)]
    console_type: u8,
    // If Nes2 == 2 flags 8-15 are in NES2 format
    #[bits(2)]
    nes2: u8,
    #[bits(4)]
    mapper_number_nibble2: u16,

    // Flags 8
    #[bits(4)]
    mapper_number_nibble3: u16,
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

pub fn read_rom(bin: &[u8]) -> Result<Box<dyn Cart>> {
    let header_bytes = u128::from_le_bytes(
        TryInto::<[u8; 16]>::try_into(&bin[0..16])
            .map_err(|_| anyhow!("Failed to read header bytes"))?,
    );

    // Read Nes1 format
    let header = INes1Header::from_bits(header_bytes);

    ensure!(
        header.magic_bytes().to_le_bytes() == [0x4E, 0x45, 0x53, 0x1A],
        "Missing magic bytes, is this really a NES rom?"
    );

    if header.nes2() == 2 {
        let header = Nes2Header::from_bits(header_bytes);
        read_nes2(bin, header)
    } else {
        read_ines(bin, header)
    }
}

fn read_ines(bin: &[u8], header: INes1Header) -> Result<Box<dyn Cart>> {
    let mut i = 16;

    let _trainer = if header.trainer() {
        let v = Some(Vec::from(
            bin.get(i..i + 512)
                .ok_or(anyhow!("Failed to read trainer bytes"))?,
        ));
        i += 512;
        v
    } else {
        None
    };

    let prg_rom_size = header.prg_rom_n() as usize * 0x4000;
    let prg_rom = Vec::from(
        bin.get(i..i + prg_rom_size)
            .ok_or_else(|| anyhow!("Failed to read prg rom bytes"))?,
    );
    i += prg_rom_size;

    let chr_rom_size = header.chr_rom_n() as usize * 0x2000;
    let chr_rom = if chr_rom_size == 0 {
        vec![0x00; 0x2000]
    } else {
        Vec::from(
            bin.get(i..i + chr_rom_size)
                .ok_or_else(|| anyhow!("Failed to read chr rom bytes"))?,
        )
    };
    i += prg_rom_size;

    if header.playchoice_10() {
        bail!("Playchoice 10 is unsupported");
    }

    let mirroring = match !header.horizontal_mirroring() {
        true => Mirroring::Horizontal,
        false => Mirroring::Vertical,
    };

    let mapper_number = header.mapper_number_upper() << 4 | header.mapper_number_lower();

    Ok(match mapper_number {
        0 => Box::new(NROM::new(mirroring, prg_rom, chr_rom)),
        1 => Box::new(MMC1::new(prg_rom, chr_rom)),
        2 => Box::new(UxROM::new(mirroring, prg_rom, chr_rom)),
        3 => Box::new(CNROM::new(mirroring, prg_rom, chr_rom)),
        _ => bail!("Mapper number {} is not implemented yet", mapper_number),
    })
}

fn read_nes2(bin: &[u8], header: Nes2Header) -> Result<Box<dyn Cart>> {
    if header.console_type() != 0 {
        bail!("This ROM is for an unsupported system");
    }

    let mut i = 16;

    let _trainer = if header.trainer() {
        let v = Some(Vec::from(
            bin.get(i..i + 512)
                .ok_or(anyhow!("Failed to get trainer bytes"))?,
        ));
        i += 512;
        v
    } else {
        None
    };

    let prg_rom_size =
        ((header.prg_rom_msb() as usize) << 4 | header.prg_rom_lsb() as usize) * 0x4000;
    let prg_rom = Vec::from(
        bin.get(i..i + prg_rom_size)
            .ok_or_else(|| anyhow!("Failed to read prg rom bytes"))?,
    );
    i += prg_rom_size;

    let chr_rom_size =
        ((header.chr_rom_msb() as usize) << 4 | header.chr_rom_lsb() as usize) * 0x2000;
    let chr_rom = if chr_rom_size == 0 {
        vec![0x00; 0x2000]
    } else {
        Vec::from(
            bin.get(i..i + chr_rom_size)
                .ok_or_else(|| anyhow!("Failed to read chr rom bytes"))?,
        )
    };
    i += prg_rom_size;

    let mirroring = match !header.horizontal_mirroring() {
        true => Mirroring::Horizontal,
        false => Mirroring::Vertical,
    };

    let mapper_number = header.mapper_number_nibble3() << 8
        | header.mapper_number_nibble2() << 4
        | header.mapper_number_nibble1();

    Ok(match mapper_number {
        0 => Box::new(NROM::new(mirroring, prg_rom, chr_rom)),
        1 => Box::new(MMC1::new(prg_rom, chr_rom)),
        2 => Box::new(UxROM::new(mirroring, prg_rom, chr_rom)),
        3 => Box::new(CNROM::new(mirroring, prg_rom, chr_rom)),
        _ => bail!("Mapper number {} is not implemented yet", mapper_number),
    })
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Mirroring {
    Horizontal,
    Vertical,
    Single,
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
