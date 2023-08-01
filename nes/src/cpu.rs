use crate::bus::Bus;

pub struct Cpu {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub pc: u16,
    pub sp: u8,
    pub p: u8,

    pub cyc: u64,

    pub ram: [u8; 0x800],
}

const FLAG_CARRY: u8 = 1 << 0;
const FLAG_ZERO: u8 = 1 << 1;
const FLAG_INTERRUPT_DISABLE: u8 = 1 << 2;
const FLAG_DECIMAL: u8 = 1 << 3;
const FLAG_B: u8 = 1 << 4;
const FLAG_5: u8 = 1 << 5;
const FLAG_OVERFLOW: u8 = 1 << 6;
const FLAG_NEGATIVE: u8 = 1 << 7;

macro_rules! op {
    ($cpu:expr,$bus:expr,$op:ident,$m:expr,$d:expr) => {{
        let a = $cpu.fetch_addr($bus, $m);
        $cpu.$op($bus, a);
        $cpu.cyc += $d;
    }};
    ($cpu:expr,$bus:expr,$op:ident,$d:expr) => {{
        $cpu.$op($bus);
        $cpu.cyc += $d;
    }};
}

impl std::fmt::Debug for Cpu {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Cpu")
            .field("a", &format_args!("{:#04x}", self.a))
            .field("x", &format_args!("{:#04x}", self.x))
            .field("y", &format_args!("{:#04x}", self.y))
            .field("pc", &format_args!("{:#06x}", self.pc))
            .field("sp", &format_args!("{:#04x}", self.sp))
            .field("p", &format_args!("{:#04x}", self.p))
            .field("cyc", &self.cyc)
            .finish()
    }
}

#[derive(Debug, Clone, Copy)]
enum Addr {
    Val(u8),
    A,
    X,
    Y,
    Mem(u16),
    /// Memory with page crossed
    MemPC(u16),
    Rel(i8),
}

#[derive(Debug, Clone, Copy)]
enum AddrMode {
    Acc,
    Imm,
    ZP,
    ZPX,
    ZPY,
    Abs,
    AbsX,
    AbsY,
    Rel,
    Ind,
    IndX,
    IndY,
}

impl Cpu {
    fn read_mem8(&mut self, bus: &mut Bus, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1FFF => self.ram[addr as usize % 0x800],
            0x2000..=0x3FFF => bus.ppu.read_register(addr),
            0x4000..=0x4017 => bus.apu.read_register(addr),
            0x4018..=0x401F => {
                unimplemented!("APU and I/O functionality that is normally disabled.")
            }
            0x4020..=0xFFFF => bus.cart.read8(addr),
        }
    }

    fn read_mem16(&mut self, bus: &mut Bus, addr: u16) -> u16 {
        let l = self.read_mem8(bus, addr);
        let r = self.read_mem8(bus, addr.wrapping_add(1));
        u16::from_le_bytes([l, r])
    }

    fn read_mem16_pw(&mut self, bus: &mut Bus, addr: u16) -> u16 {
        let l = self.read_mem8(bus, addr);
        let r = self.read_mem8(bus, (addr.wrapping_add(1) & 0xFF) | (addr & 0xFF00));
        u16::from_le_bytes([l, r])
    }

    fn write_mem8(&mut self, bus: &mut Bus, addr: u16, val: u8) {
        match addr {
            0x0000..=0x1FFF => {
                self.ram[addr as usize % 0x800] = val;
            }
            0x2000..=0x3FFF => {
                bus.ppu.write_register(addr, val);
            }
            0x4014 => {
                self.oam_dma(bus, val);
            }
            0x4000..=0x4017 => {
                bus.apu.write_register(addr, val);
            }
            0x4018..=0x401F => {
                unimplemented!("APU and I/O functionality that is normally disabled.")
            }
            0x4020..=0xFFFF => {
                bus.cart.write8(addr - 0x4020, val);
            }
        }
    }

    pub fn init(&mut self, bus: &mut Bus) {
        let start_addr = self.read_mem16(bus, 0xFFFC);
        self.pc = start_addr;
        self.cyc = 7;
    }

    pub fn reset(&mut self) {
        self.sp = self.sp.wrapping_sub(3);
        self.p |= FLAG_INTERRUPT_DISABLE;
    }

    pub fn new() -> Self {
        let cpu = Self {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            sp: 0xFD,
            p: 0x24,

            cyc: 7,

            ram: [0x00; 0x800],
        };

        cpu
    }

    fn fetch8(&mut self, bus: &mut Bus) -> u8 {
        let v = self.read_mem8(bus, self.pc);
        self.pc += 1;
        v
    }

    fn fetch16(&mut self, bus: &mut Bus) -> u16 {
        let v = self.read_mem16(bus, self.pc);
        self.pc += 2;
        v
    }

    fn fetch_addr(&mut self, bus: &mut Bus, m: AddrMode) -> Addr {
        use AddrMode::*;
        match m {
            Acc => Addr::A,
            Imm => Addr::Val(self.fetch8(bus)),
            ZP => Addr::Mem(self.fetch8(bus) as u16),
            ZPX => Addr::Mem(self.fetch8(bus).wrapping_add(self.x) as u16),
            ZPY => Addr::Mem(self.fetch8(bus).wrapping_add(self.y) as u16),
            Rel => Addr::Rel(self.fetch8(bus) as i8),
            Abs => Addr::Mem(self.fetch16(bus)),
            AbsX => {
                let abs = self.fetch16(bus);
                let a = abs.wrapping_add(self.x as u16);

                if a & 0xFF < abs & 0xFF {
                    Addr::MemPC(a)
                } else {
                    Addr::Mem(a)
                }
            }
            AbsY => {
                let abs = self.fetch16(bus);
                let a = abs.wrapping_add(self.y as u16);

                if a & 0xFF < abs & 0xFF {
                    Addr::MemPC(a)
                } else {
                    Addr::Mem(a)
                }
            }
            Ind => {
                let a = self.fetch16(bus);
                let a = self.read_mem16_pw(bus, a);
                Addr::Mem(a)
            }
            IndX => {
                let a = self.fetch8(bus).wrapping_add(self.x);
                let a = u16::from_le_bytes([
                    self.read_mem8(bus, a as u16),
                    self.read_mem8(bus, a.wrapping_add(1) as u16),
                ]);
                Addr::Mem(a)
            }
            IndY => {
                let a = self.fetch8(bus) as u16;
                let abs = u16::from_le_bytes([
                    self.read_mem8(bus, a),
                    self.read_mem8(bus, a.wrapping_add(1) & 0xFF),
                ]);
                let a = abs.wrapping_add(self.y as u16);

                if a & 0xFF < abs & 0xFF {
                    Addr::MemPC(a)
                } else {
                    Addr::Mem(a)
                }
            }
        }
    }

    fn read8(&mut self, bus: &mut Bus, addr: Addr) -> u8 {
        match addr {
            Addr::Val(x) => x,
            Addr::A => self.a,
            Addr::X => self.x,
            Addr::Y => self.y,
            Addr::Mem(a) => self.read_mem8(bus, a),
            Addr::MemPC(a) => {
                self.cyc += 1;
                self.read_mem8(bus, a)
            }
            Addr::Rel(_) => unreachable!(),
        }
    }

    fn write8(&mut self, bus: &mut Bus, addr: Addr, v: u8) {
        match addr {
            Addr::A => {
                self.a = v;
            }
            Addr::X => {
                self.x = v;
            }
            Addr::Y => {
                self.y = v;
            }
            Addr::Mem(a) | Addr::MemPC(a) => self.write_mem8(bus, a, v),
            Addr::Val(_) | Addr::Rel(_) => unreachable!(),
        }
    }

    pub fn cycle(&mut self, bus: &mut Bus) -> u64 {
        let opcode = self.fetch8(bus);

        use AddrMode::*;

        let prev_cyc = self.cyc;

        match opcode {
            // ADC
            0x69 => op!(self, bus, adc, Imm, 2),
            0x65 => op!(self, bus, adc, ZP, 3),
            0x75 => op!(self, bus, adc, ZPX, 4),
            0x6D => op!(self, bus, adc, Abs, 4),
            0x7D => op!(self, bus, adc, AbsX, 4),
            0x79 => op!(self, bus, adc, AbsY, 4),
            0x61 => op!(self, bus, adc, IndX, 6),
            0x71 => op!(self, bus, adc, IndY, 5),

            // AND
            0x29 => op!(self, bus, and, Imm, 2),
            0x25 => op!(self, bus, and, ZP, 3),
            0x35 => op!(self, bus, and, ZPX, 4),
            0x2D => op!(self, bus, and, Abs, 4),
            0x3D => op!(self, bus, and, AbsX, 4),
            0x39 => op!(self, bus, and, AbsY, 4),
            0x21 => op!(self, bus, and, IndX, 6),
            0x31 => op!(self, bus, and, IndY, 5),

            // ASL
            0x0A => op!(self, bus, asl, Acc, 2),
            0x06 => op!(self, bus, asl, ZP, 5),
            0x16 => op!(self, bus, asl, ZPX, 6),
            0x0E => op!(self, bus, asl, Abs, 6),
            0x1E => op!(self, bus, asl, AbsX, 7),

            // BCC
            0x90 => op!(self, bus, bcc, Rel, 2),

            // BCS
            0xB0 => op!(self, bus, bcs, Rel, 2),

            // BEQ
            0xF0 => op!(self, bus, beq, Rel, 2),

            // BIT
            0x24 => op!(self, bus, bit, ZP, 3),
            0x2C => op!(self, bus, bit, Abs, 4),

            // BMI
            0x30 => op!(self, bus, bmi, Rel, 2),

            // BNE
            0xD0 => op!(self, bus, bne, Rel, 2),

            // BPL
            0x10 => op!(self, bus, bpl, Rel, 2),

            // BVC
            0x50 => op!(self, bus, bvc, Rel, 2),

            // BVS
            0x70 => op!(self, bus, bvs, Rel, 2),

            // CLC
            0x18 => op!(self, bus, clc, 2),

            // CLD
            0xD8 => op!(self, bus, cld, 2),

            // CLV
            0xB8 => op!(self, bus, clv, 2),

            // CMP
            0xC9 => op!(self, bus, cmp, Imm, 2),
            0xC5 => op!(self, bus, cmp, ZP, 3),
            0xD5 => op!(self, bus, cmp, ZPX, 4),
            0xCD => op!(self, bus, cmp, Abs, 4),
            0xDD => op!(self, bus, cmp, AbsX, 4),
            0xD9 => op!(self, bus, cmp, AbsY, 4),
            0xC1 => op!(self, bus, cmp, IndX, 6),
            0xD1 => op!(self, bus, cmp, IndY, 5),

            // CPX
            0xE0 => op!(self, bus, cpx, Imm, 2),
            0xE4 => op!(self, bus, cpx, ZP, 3),
            0xEC => op!(self, bus, cpx, Abs, 4),

            // CPY
            0xC0 => op!(self, bus, cpy, Imm, 2),
            0xC4 => op!(self, bus, cpy, ZP, 3),
            0xCC => op!(self, bus, cpy, Abs, 4),

            // DEC
            0xC6 => op!(self, bus, dec, ZP, 5),
            0xD6 => op!(self, bus, dec, ZPX, 6),
            0xCE => op!(self, bus, dec, Abs, 6),
            0xDE => op!(self, bus, dec, AbsX, 7),

            // DEX
            0xCA => op!(self, bus, dex, 2),

            // DEY
            0x88 => op!(self, bus, dey, 2),

            // EOR
            0x49 => op!(self, bus, eor, Imm, 2),
            0x45 => op!(self, bus, eor, ZP, 3),
            0x55 => op!(self, bus, eor, ZPX, 4),
            0x4D => op!(self, bus, eor, Abs, 4),
            0x5D => op!(self, bus, eor, AbsX, 4),
            0x59 => op!(self, bus, eor, AbsY, 4),
            0x41 => op!(self, bus, eor, IndX, 6),
            0x51 => op!(self, bus, eor, IndY, 5),

            // INC
            0xE6 => op!(self, bus, inc, ZP, 5),
            0xF6 => op!(self, bus, inc, ZPX, 6),
            0xEE => op!(self, bus, inc, Abs, 6),
            0xFE => op!(self, bus, inc, AbsX, 7),

            // INX
            0xE8 => op!(self, bus, inx, 2),

            // INY
            0xC8 => op!(self, bus, iny, 2),

            // JMP
            0x4C => op!(self, bus, jmp, Abs, 3),
            0x6C => op!(self, bus, jmp, Ind, 5),

            // JSR
            0x20 => op!(self, bus, jsr, Abs, 6),

            // LDA
            0xA9 => op!(self, bus, lda, Imm, 2),
            0xA5 => op!(self, bus, lda, ZP, 3),
            0xB5 => op!(self, bus, lda, ZPX, 4),
            0xAD => op!(self, bus, lda, Abs, 4),
            0xBD => op!(self, bus, lda, AbsX, 4),
            0xB9 => op!(self, bus, lda, AbsY, 4),
            0xA1 => op!(self, bus, lda, IndX, 6),
            0xB1 => op!(self, bus, lda, IndY, 5),

            // LDX
            0xA2 => op!(self, bus, ldx, Imm, 2),
            0xA6 => op!(self, bus, ldx, ZP, 3),
            0xB6 => op!(self, bus, ldx, ZPY, 4),
            0xAE => op!(self, bus, ldx, Abs, 4),
            0xBE => op!(self, bus, ldx, AbsY, 4),

            // LDY
            0xA0 => op!(self, bus, ldy, Imm, 2),
            0xA4 => op!(self, bus, ldy, ZP, 3),
            0xB4 => op!(self, bus, ldy, ZPX, 4),
            0xAC => op!(self, bus, ldy, Abs, 4),
            0xBC => op!(self, bus, ldy, AbsX, 4),

            // LSR
            0x4A => op!(self, bus, lsr, Acc, 2),
            0x46 => op!(self, bus, lsr, ZP, 5),
            0x56 => op!(self, bus, lsr, ZPX, 6),
            0x4E => op!(self, bus, lsr, Abs, 6),
            0x5E => op!(self, bus, lsr, AbsX, 7),

            // NOP
            0xEA => op!(self, bus, nop, 2),

            // ORA
            0x09 => op!(self, bus, ora, Imm, 2),
            0x05 => op!(self, bus, ora, ZP, 3),
            0x15 => op!(self, bus, ora, ZPX, 4),
            0x0D => op!(self, bus, ora, Abs, 4),
            0x1D => op!(self, bus, ora, AbsX, 4),
            0x19 => op!(self, bus, ora, AbsY, 4),
            0x01 => op!(self, bus, ora, IndX, 6),
            0x11 => op!(self, bus, ora, IndY, 5),

            // PHA
            0x48 => op!(self, bus, pha, 3),

            // PHP
            0x08 => op!(self, bus, php, 3),

            // PLA
            0x68 => op!(self, bus, pla, 4),

            // PLP
            0x28 => op!(self, bus, plp, 4),

            // ROL
            0x2A => op!(self, bus, rol, Acc, 2),
            0x26 => op!(self, bus, rol, ZP, 5),
            0x36 => op!(self, bus, rol, ZPX, 6),
            0x2E => op!(self, bus, rol, Abs, 6),
            0x3E => op!(self, bus, rol, AbsX, 7),

            // ROR
            0x6A => op!(self, bus, ror, Acc, 2),
            0x66 => op!(self, bus, ror, ZP, 5),
            0x76 => op!(self, bus, ror, ZPX, 6),
            0x6E => op!(self, bus, ror, Abs, 6),
            0x7E => op!(self, bus, ror, AbsX, 7),

            // RTI
            0x40 => op!(self, bus, rti, 6),

            // RTS
            0x60 => op!(self, bus, rts, 6),

            // SBC
            0xE9 | 0xEB => op!(self, bus, sbc, Imm, 2),
            0xE5 => op!(self, bus, sbc, ZP, 3),
            0xF5 => op!(self, bus, sbc, ZPX, 4),
            0xED => op!(self, bus, sbc, Abs, 4),
            0xFD => op!(self, bus, sbc, AbsX, 4),
            0xF9 => op!(self, bus, sbc, AbsY, 4),
            0xE1 => op!(self, bus, sbc, IndX, 6),
            0xF1 => op!(self, bus, sbc, IndY, 5),

            // SEC
            0x38 => op!(self, bus, sec, 2),

            // SED
            0xF8 => op!(self, bus, sed, 2),

            // SEI
            0x78 => op!(self, bus, sei, 2),

            // STA
            0x85 => op!(self, bus, sta, ZP, 3),
            0x95 => op!(self, bus, sta, ZPX, 4),
            0x8D => op!(self, bus, sta, Abs, 4),
            0x9D => op!(self, bus, sta, AbsX, 5),
            0x99 => op!(self, bus, sta, AbsY, 5),
            0x81 => op!(self, bus, sta, IndX, 6),
            0x91 => op!(self, bus, sta, IndY, 6),

            // STX
            0x86 => op!(self, bus, stx, ZP, 3),
            0x96 => op!(self, bus, stx, ZPY, 4),
            0x8E => op!(self, bus, stx, Abs, 4),

            // STY
            0x84 => op!(self, bus, sty, ZP, 3),
            0x94 => op!(self, bus, sty, ZPX, 4),
            0x8C => op!(self, bus, sty, Abs, 4),

            // TAX
            0xAA => op!(self, bus, tax, 2),

            // TAY
            0xA8 => op!(self, bus, tay, 2),

            // TSX
            0xBA => op!(self, bus, tsx, 2),

            // TXA
            0x8A => op!(self, bus, txa, 2),

            // TXS
            0x9A => op!(self, bus, txs, 2),

            // TYA
            0x98 => op!(self, bus, tya, 2),

            // Illegal opcodes

            // NOP
            0x04 | 0x44 | 0x64 => {
                let _a = self.fetch_addr(bus, ZP);
                self.nop(bus);
                self.cyc += 3;
            }
            0x0c => {
                let _a = self.fetch_addr(bus, Abs);
                self.nop(bus);
                self.cyc += 4;
            }
            0x14 | 0x34 | 0x54 | 0x74 | 0xD4 | 0xF4 => {
                let a = self.fetch_addr(bus, IndX);
                self.read8(bus, a);
                self.nop(bus);
                self.cyc += 4;
            }
            0x1A | 0x3A | 0x5A | 0x7A | 0xDA | 0xFA => op!(self, bus, nop, 2),
            0x80 => {
                let _a = self.fetch_addr(bus, Imm);
                self.nop(bus);
                self.cyc += 2;
            }
            0x1C | 0x3C | 0x5C | 0x7C | 0xDC | 0xFC => {
                let a = self.fetch_addr(bus, AbsX);
                self.read8(bus, a);
                self.nop(bus);
                self.cyc += 4;
            }

            // LAX
            0xA3 => op!(self, bus, lax, IndX, 6),
            0xA7 => op!(self, bus, lax, ZP, 3),
            0xAF => op!(self, bus, lax, Abs, 4),
            0xB3 => op!(self, bus, lax, IndY, 5),
            0xB7 => op!(self, bus, lax, ZPY, 4),
            0xBF => op!(self, bus, lax, AbsY, 4),

            // SAX
            0x83 => op!(self, bus, sax, IndX, 6),
            0x87 => op!(self, bus, sax, ZP, 3),
            0x8F => op!(self, bus, sax, Abs, 4),
            0x97 => op!(self, bus, sax, ZPY, 4),

            // DCP
            0xC3 => op!(self, bus, dcp, IndX, 8),
            0xC7 => op!(self, bus, dcp, ZP, 5),
            0xCF => op!(self, bus, dcp, Abs, 6),
            0xD3 => op!(self, bus, dcp, IndY, 6),
            0xD7 => op!(self, bus, dcp, ZPX, 6),
            0xDB => op!(self, bus, dcp, AbsY, 5),
            0xDF => op!(self, bus, dcp, AbsX, 5),

            // ISC
            0xE3 => op!(self, bus, isc, IndX, 8),
            0xE7 => op!(self, bus, isc, ZP, 5),
            0xEF => op!(self, bus, isc, Abs, 6),
            0xF3 => op!(self, bus, isc, IndY, 6),
            0xF7 => op!(self, bus, isc, ZPX, 6),
            0xFB => op!(self, bus, isc, AbsY, 5),
            0xFF => op!(self, bus, isc, AbsX, 5),

            // SLO
            0x03 => op!(self, bus, slo, IndX, 8),
            0x07 => op!(self, bus, slo, ZP, 5),
            0x0F => op!(self, bus, slo, Abs, 6),
            0x13 => op!(self, bus, slo, IndY, 6),
            0x17 => op!(self, bus, slo, ZPX, 6),
            0x1B => op!(self, bus, slo, AbsY, 5),
            0x1F => op!(self, bus, slo, AbsX, 5),

            // RLA
            0x23 => op!(self, bus, rla, IndX, 8),
            0x27 => op!(self, bus, rla, ZP, 5),
            0x2F => op!(self, bus, rla, Abs, 6),
            0x33 => op!(self, bus, rla, IndY, 6),
            0x37 => op!(self, bus, rla, ZPX, 6),
            0x3B => op!(self, bus, rla, AbsY, 5),
            0x3F => op!(self, bus, rla, AbsX, 5),

            // SRE
            0x43 => op!(self, bus, sre, IndX, 8),
            0x47 => op!(self, bus, sre, ZP, 5),
            0x4F => op!(self, bus, sre, Abs, 6),
            0x53 => op!(self, bus, sre, IndY, 6),
            0x57 => op!(self, bus, sre, ZPX, 6),
            0x5B => op!(self, bus, sre, AbsY, 5),
            0x5F => op!(self, bus, sre, AbsX, 5),

            // RRA
            0x63 => op!(self, bus, rra, IndX, 8),
            0x67 => op!(self, bus, rra, ZP, 5),
            0x6F => op!(self, bus, rra, Abs, 6),
            0x73 => op!(self, bus, rra, IndY, 6),
            0x77 => op!(self, bus, rra, ZPX, 6),
            0x7B => op!(self, bus, rra, AbsY, 5),
            0x7F => op!(self, bus, rra, AbsX, 5),

            _ => {
                #[cfg(debug_assertions)]
                panic!("OPCODE {:#04x} not yet implemented", opcode);
            }
        };

        self.cyc - prev_cyc
    }

    fn get_flag(&self, mask: u8) -> bool {
        self.p & mask != 0
    }

    fn set_flag(&mut self, mask: u8, v: bool) {
        if v {
            self.enable_flag(mask);
        } else {
            self.disable_flag(mask);
        }
    }

    fn enable_flag(&mut self, mask: u8) {
        self.p |= mask;
    }

    fn disable_flag(&mut self, mask: u8) {
        self.p &= !mask;
    }

    fn update_negative(&mut self, v: u8) {
        self.set_flag(FLAG_NEGATIVE, (v as i8).is_negative());
    }

    fn update_zero(&mut self, v: u8) {
        self.set_flag(FLAG_ZERO, v == 0);
    }

    fn relative_jump(&mut self, d: i8) -> u64 {
        let old_pc = self.pc;
        let new_pc = self.pc.wrapping_add_signed(d as i16);
        self.pc = new_pc;

        let old_page = (old_pc / 256) as u8;
        let new_page = (new_pc / 256) as u8;

        if new_page != old_page {
            1
        } else {
            0
        }
    }

    fn compare(&mut self, r: u8, v: u8) {
        self.set_flag(FLAG_CARRY, r >= v);
        self.set_flag(FLAG_ZERO, r == v);
        self.set_flag(FLAG_NEGATIVE, r.wrapping_sub(v) & 0x80 != 0);
    }

    fn push8(&mut self, bus: &mut Bus, v: u8) {
        self.write_mem8(bus, 0x0100 + self.sp as u16, v);
        self.sp = self.sp.wrapping_sub(1);
    }

    fn push16(&mut self, bus: &mut Bus, v: u16) {
        let [l, r] = v.to_le_bytes();
        self.push8(bus, r);
        self.push8(bus, l);
    }

    fn pop8(&mut self, bus: &mut Bus) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        let v = self.read_mem8(bus, 0x0100 + self.sp as u16);
        v
    }

    fn pop16(&mut self, bus: &mut Bus) -> u16 {
        let l = self.pop8(bus);
        let r = self.pop8(bus);
        u16::from_le_bytes([l, r])
    }

    pub fn oam_dma(&mut self, bus: &mut Bus, v: u8) {
        // Either 513 or 514 depending if on a put or write cycle, hard to implement
        self.cyc += 514;
        let mut mem_i = (v as u16) << 8;
        for i in 0..256 {
            bus.ppu.oam[i] = self.read_mem8(bus, mem_i);
            mem_i += 1;
        }
    }

    // Operations

    fn adc(&mut self, bus: &mut Bus, a: Addr) {
        let lhs = self.a as u16;
        let rhs = self.read8(bus, a) as u16;
        let carry = self.get_flag(FLAG_CARRY) as u16;

        let res = lhs + rhs + carry;
        let res8 = res as u8;
        self.a = res8;

        self.update_zero(res8);
        self.update_negative(res8);
        self.set_flag(FLAG_OVERFLOW, !(lhs ^ rhs) & (lhs ^ res) & 0x80 != 0);
        self.set_flag(FLAG_CARRY, res > 0xFF);
    }

    fn and(&mut self, bus: &mut Bus, a: Addr) {
        self.a &= self.read8(bus, a);
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn asl(&mut self, bus: &mut Bus, a: Addr) {
        let v = self.read8(bus, a);
        let new_v = v << 1;

        self.set_flag(FLAG_CARRY, v & 0x80 != 0);
        self.update_zero(new_v);
        self.update_negative(new_v);

        self.write8(bus, a, new_v);
    }

    fn bcc(&mut self, _bus: &mut Bus, a: Addr) {
        self.generic_branch(a, |cpu| !cpu.get_flag(FLAG_CARRY));
    }

    fn bcs(&mut self, _bus: &mut Bus, a: Addr) {
        self.generic_branch(a, |cpu| cpu.get_flag(FLAG_CARRY));
    }

    fn beq(&mut self, _bus: &mut Bus, a: Addr) {
        self.generic_branch(a, |cpu| cpu.get_flag(FLAG_ZERO));
    }

    fn bit(&mut self, bus: &mut Bus, a: Addr) {
        let m = self.read8(bus, a);
        let v = self.a & m;
        self.set_flag(FLAG_OVERFLOW, m & (1 << 6) != 0);
        self.set_flag(FLAG_NEGATIVE, m & (1 << 7) != 0);
        self.set_flag(FLAG_ZERO, v == 0);
    }

    fn bmi(&mut self, _bus: &mut Bus, a: Addr) {
        self.generic_branch(a, |cpu| cpu.get_flag(FLAG_NEGATIVE));
    }

    fn bne(&mut self, _bus: &mut Bus, a: Addr) {
        self.generic_branch(a, |cpu| !cpu.get_flag(FLAG_ZERO));
    }

    fn bpl(&mut self, _bus: &mut Bus, a: Addr) {
        self.generic_branch(a, |cpu| !cpu.get_flag(FLAG_NEGATIVE));
    }

    fn brk(&mut self, bus: &mut Bus) {
        self.push16(bus, self.pc);
        self.push8(bus, self.p | FLAG_5 | FLAG_B);
        self.enable_flag(FLAG_INTERRUPT_DISABLE);
        self.pc = self.read_mem16(bus, 0xFFFE);
    }

    fn bvc(&mut self, _bus: &mut Bus, a: Addr) {
        self.generic_branch(a, |cpu| !cpu.get_flag(FLAG_OVERFLOW));
    }

    fn bvs(&mut self, _bus: &mut Bus, a: Addr) {
        self.generic_branch(a, |cpu| cpu.get_flag(FLAG_OVERFLOW));
    }

    fn clc(&mut self, _bus: &mut Bus) {
        self.disable_flag(FLAG_CARRY);
    }

    fn cld(&mut self, _bus: &mut Bus) {
        self.disable_flag(FLAG_DECIMAL);
    }

    fn cli(&mut self, _bus: &mut Bus) {
        self.disable_flag(FLAG_INTERRUPT_DISABLE);
    }

    fn clv(&mut self, _bus: &mut Bus) {
        self.disable_flag(FLAG_OVERFLOW);
    }

    fn cmp(&mut self, bus: &mut Bus, a: Addr) {
        let v = self.read8(bus, a);
        self.compare(self.a, v);
    }

    fn cpx(&mut self, bus: &mut Bus, a: Addr) {
        let v = self.read8(bus, a);
        self.compare(self.x, v);
    }

    fn cpy(&mut self, bus: &mut Bus, a: Addr) {
        let v = self.read8(bus, a);
        self.compare(self.y, v);
    }

    fn dcp(&mut self, bus: &mut Bus, a: Addr) {
        self.dec(bus, a);
        self.cmp(bus, a);
    }

    fn dec(&mut self, bus: &mut Bus, a: Addr) {
        let v = self.read8(bus, a).wrapping_sub(1);
        self.write8(bus, a, v);
        self.update_zero(v);
        self.update_negative(v);
    }

    fn dex(&mut self, _bus: &mut Bus) {
        self.x = self.x.wrapping_sub(1);
        self.update_zero(self.x);
        self.update_negative(self.x);
    }

    fn dey(&mut self, _bus: &mut Bus) {
        self.y = self.y.wrapping_sub(1);
        self.update_zero(self.y);
        self.update_negative(self.y);
    }

    fn eor(&mut self, bus: &mut Bus, a: Addr) {
        self.a ^= self.read8(bus, a);
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn inc(&mut self, bus: &mut Bus, a: Addr) {
        let v = self.read8(bus, a).wrapping_add(1);
        self.write8(bus, a, v);
        self.update_zero(v);
        self.update_negative(v);
    }

    fn inx(&mut self, _bus: &mut Bus) {
        let v = self.x.wrapping_add(1);
        self.x = v;
        self.update_zero(v);
        self.update_negative(v);
    }

    fn iny(&mut self, _bus: &mut Bus) {
        let v = self.y.wrapping_add(1);
        self.y = v;
        self.update_zero(v);
        self.update_negative(v);
    }

    fn isc(&mut self, bus: &mut Bus, a: Addr) {
        self.inc(bus, a);
        self.sbc(bus, a);
    }

    fn jmp(&mut self, _bus: &mut Bus, a: Addr) {
        if let Addr::Mem(m) = a {
            self.pc = m;
        } else {
            unreachable!();
        }
    }

    fn jsr(&mut self, bus: &mut Bus, a: Addr) {
        if let Addr::Mem(a) = a {
            self.push16(bus, self.pc - 1);
            self.pc = a;
        } else {
            unreachable!();
        }
    }

    fn lax(&mut self, bus: &mut Bus, a: Addr) {
        let v = self.read8(bus, a);
        self.a = v;
        self.x = v;

        self.update_zero(v);
        self.update_negative(v);
    }

    fn lda(&mut self, bus: &mut Bus, a: Addr) {
        self.a = self.read8(bus, a);
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn ldx(&mut self, bus: &mut Bus, a: Addr) {
        self.x = self.read8(bus, a);
        self.update_zero(self.x);
        self.update_negative(self.x);
    }

    fn ldy(&mut self, bus: &mut Bus, a: Addr) {
        self.y = self.read8(bus, a);
        self.update_zero(self.y);
        self.update_negative(self.y);
    }

    fn lsr(&mut self, bus: &mut Bus, a: Addr) {
        let v = self.read8(bus, a);
        let new_v = v >> 1;

        self.set_flag(FLAG_CARRY, v & 0x1 != 0);
        self.update_zero(new_v);
        self.update_negative(new_v);

        self.write8(bus, a, new_v);
    }

    fn nop(&mut self, _bus: &mut Bus) {}

    fn ora(&mut self, bus: &mut Bus, a: Addr) {
        self.a |= self.read8(bus, a);
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn pha(&mut self, bus: &mut Bus) {
        self.push8(bus, self.a);
    }

    fn php(&mut self, bus: &mut Bus) {
        let p = self.p | FLAG_5 | FLAG_B;
        self.push8(bus, p);
    }

    fn pla(&mut self, bus: &mut Bus) {
        self.a = self.pop8(bus);
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn plp(&mut self, bus: &mut Bus) {
        self.p = (self.pop8(bus) & !FLAG_B) | FLAG_5;
    }

    fn rla(&mut self, bus: &mut Bus, a: Addr) {
        self.rol(bus, a);
        self.and(bus, a);
    }

    fn rol(&mut self, bus: &mut Bus, a: Addr) {
        let v = self.read8(bus, a);
        let mut new_v = v << 1;
        if self.get_flag(FLAG_CARRY) {
            new_v |= 0x01;
        }
        self.set_flag(FLAG_CARRY, v & 0x80 != 0);

        self.update_zero(new_v);
        self.update_negative(new_v);

        self.write8(bus, a, new_v);
    }

    fn ror(&mut self, bus: &mut Bus, a: Addr) {
        let v = self.read8(bus, a);
        let mut new_v = v >> 1;
        if self.get_flag(FLAG_CARRY) {
            new_v |= 0x80;
        }
        self.set_flag(FLAG_CARRY, v & 0x01 != 0);

        self.update_zero(new_v);
        self.update_negative(new_v);

        self.write8(bus, a, new_v);
    }

    fn rra(&mut self, bus: &mut Bus, a: Addr) {
        self.ror(bus, a);
        self.adc(bus, a);
    }

    fn rti(&mut self, bus: &mut Bus) {
        self.p = self.pop8(bus) | FLAG_5;
        self.pc = self.pop16(bus);
    }

    fn rts(&mut self, bus: &mut Bus) {
        self.pc = self.pop16(bus) + 1;
    }

    fn sax(&mut self, bus: &mut Bus, a: Addr) {
        let v = self.a & self.x;
        self.write8(bus, a, v);
    }

    fn sbc(&mut self, bus: &mut Bus, a: Addr) {
        let lhs = self.a as i16;
        let rhs = self.read8(bus, a) as i16;
        let borrow = !self.get_flag(FLAG_CARRY) as i16;

        let res = lhs - rhs - borrow;
        let res8 = res as u8;

        self.update_zero(res8);
        self.update_negative(res8);

        self.set_flag(FLAG_CARRY, res >= 0);
        self.set_flag(FLAG_OVERFLOW, (lhs ^ rhs) & (lhs ^ res) & 0x80 != 0);

        self.a = res8;
    }

    fn sec(&mut self, _bus: &mut Bus) {
        self.set_flag(FLAG_CARRY, true);
    }

    fn sed(&mut self, _bus: &mut Bus) {
        self.set_flag(FLAG_DECIMAL, true);
    }

    fn sei(&mut self, _bus: &mut Bus) {
        self.set_flag(FLAG_INTERRUPT_DISABLE, true);
    }

    fn slo(&mut self, bus: &mut Bus, a: Addr) {
        self.asl(bus, a);
        self.ora(bus, a);
    }

    fn sre(&mut self, bus: &mut Bus, a: Addr) {
        self.lsr(bus, a);
        self.eor(bus, a);
    }

    fn sta(&mut self, bus: &mut Bus, a: Addr) {
        self.write8(bus, a, self.a);
    }

    fn stx(&mut self, bus: &mut Bus, a: Addr) {
        self.write8(bus, a, self.x);
    }

    fn sty(&mut self, bus: &mut Bus, a: Addr) {
        self.write8(bus, a, self.y);
    }

    fn tax(&mut self, _bus: &mut Bus) {
        self.x = self.a;
        self.update_zero(self.x);
        self.update_negative(self.x);
    }

    fn tay(&mut self, _bus: &mut Bus) {
        self.y = self.a;
        self.update_zero(self.y);
        self.update_negative(self.y);
    }

    fn tsx(&mut self, _bus: &mut Bus) {
        self.x = self.sp;
        self.update_zero(self.x);
        self.update_negative(self.x);
    }

    fn txa(&mut self, _bus: &mut Bus) {
        self.a = self.x;
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn txs(&mut self, _bus: &mut Bus) {
        self.sp = self.x;
    }

    fn tya(&mut self, _bus: &mut Bus) {
        self.a = self.y;
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn generic_branch<F>(&mut self, addr: Addr, cond: F)
    where
        F: FnOnce(&Cpu) -> bool,
    {
        if let Addr::Rel(d) = addr {
            if cond(self) {
                self.cyc += 1;
                self.cyc += self.relative_jump(d);
            }
        } else {
            unreachable!()
        }
    }
}
