use crate::{bus::Bus, cart::Cart, ppu::Display};

pub struct Cpu {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub pc: u16,
    pub sp: u8,
    pub p: u8,

    pub cyc: u64,

    pub ram: [u8; 0x800],

    pub bus: Bus,
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
    ($cpu:expr,$op:ident,$m:expr,$d:expr) => {{
        let a = $cpu.fetch_addr($m);
        $cpu.$op(a);
        $cpu.cyc += $d;
    }};
    ($cpu:expr,$op:ident,$d:expr) => {{
        $cpu.$op();
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
    fn read_mem8(&mut self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1FFF => self.ram[addr as usize % 0x800],
            0x2000..=0x3FFF => self.bus.ppu.read_register(addr),
            0x4000..=0x4017 => self.bus.apu.read_register(addr),
            0x4018..=0x401F => {
                unimplemented!("APU and I/O functionality that is normally disabled.")
            }
            0x4020..=0xFFFF => self.bus.cart.read8(addr),
        }
    }

    pub fn inspect_mem8(&self, addr: u16) -> Option<u8> {
        match addr {
            0x0000..=0x1FFF => self.ram.get(addr as usize % 0x800).copied(),
            0x2000..=0x3FFF => None,
            0x4000..=0x4017 => None,
            0x4018..=0x401F => {
                unimplemented!("APU and I/O functionality that is normally disabled.")
            }
            0x4020..=0xFFFF => Some(self.bus.cart.read8(addr)),
        }
    }

    fn read_mem16(&mut self, addr: u16) -> u16 {
        let l = self.read_mem8(addr);
        let r = self.read_mem8(addr.wrapping_add(1));
        u16::from_le_bytes([l, r])
    }

    pub fn inspect_mem16(&self, addr: u16) -> Option<u16> {
        let l = self.inspect_mem8(addr);
        let r = self.inspect_mem8(addr.wrapping_add(1));
        if let Some(l) = l
            && let Some(r) = r
        {
            Some(u16::from_le_bytes([l, r]))
        } else {
            None
        }
    }

    fn read_mem16_pw(&mut self, addr: u16) -> u16 {
        let l = self.read_mem8(addr);
        let r = self.read_mem8((addr.wrapping_add(1) & 0xFF) | (addr & 0xFF00));
        u16::from_le_bytes([l, r])
    }

    fn write_mem8(&mut self, addr: u16, val: u8) {
        match addr {
            0x0000..=0x1FFF => {
                self.ram[addr as usize % 0x800] = val;
            }
            0x2000..=0x3FFF => {
                self.bus.ppu.write_register(addr, val);
            }
            0x4014 => {
                self.oam_dma(val);
            }
            0x4000..=0x4017 => {
                self.bus.apu.write_register(addr, val);
            }
            0x4018..=0x401F => {
                unimplemented!("APU and I/O functionality that is normally disabled.")
            }
            0x4020..=0xFFFF => {
                self.bus.cart.write8(addr - 0x4020, val);
            }
        }
    }

    fn init(&mut self) {
        let start_addr = self.read_mem16(0xFFFC);
        self.pc = start_addr;
    }

    pub fn reset(&mut self) {
        self.sp = self.sp.wrapping_sub(3);
        self.p |= FLAG_INTERRUPT_DISABLE;
        self.bus.reset();
    }

    pub fn new(cart: Cart) -> Self {
        let mut cpu = Self {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            sp: 0xFD,
            p: 0x24,

            cyc: 7,

            ram: [0x00; 0x800],

            bus: Bus::new(cart),
        };

        cpu.init();

        cpu
    }

    fn fetch8(&mut self) -> u8 {
        let v = self.read_mem8(self.pc);
        self.pc += 1;
        v
    }

    fn fetch16(&mut self) -> u16 {
        let v = self.read_mem16(self.pc);
        self.pc += 2;
        v
    }

    fn fetch_addr(&mut self, m: AddrMode) -> Addr {
        use AddrMode::*;
        match m {
            Acc => Addr::A,
            Imm => Addr::Val(self.fetch8()),
            ZP => Addr::Mem(self.fetch8() as u16),
            ZPX => Addr::Mem(self.fetch8().wrapping_add(self.x) as u16),
            ZPY => Addr::Mem(self.fetch8().wrapping_add(self.y) as u16),
            Rel => Addr::Rel(self.fetch8() as i8),
            Abs => Addr::Mem(self.fetch16()),
            AbsX => {
                let abs = self.fetch16();
                let a = abs.wrapping_add(self.x as u16);

                if a & 0xFF < abs & 0xFF {
                    Addr::MemPC(a)
                } else {
                    Addr::Mem(a)
                }
            }
            AbsY => {
                let abs = self.fetch16();
                let a = abs.wrapping_add(self.y as u16);

                if a & 0xFF < abs & 0xFF {
                    Addr::MemPC(a)
                } else {
                    Addr::Mem(a)
                }
            }
            Ind => {
                let a = self.fetch16();
                let a = self.read_mem16_pw(a);
                Addr::Mem(a)
            }
            IndX => {
                let a = self.fetch8().wrapping_add(self.x);
                let a = u16::from_le_bytes([
                    self.read_mem8(a as u16),
                    self.read_mem8(a.wrapping_add(1) as u16),
                ]);
                Addr::Mem(a)
            }
            IndY => {
                let a = self.fetch8() as u16;
                let abs = u16::from_le_bytes([
                    self.read_mem8(a),
                    self.read_mem8(a.wrapping_add(1) & 0xFF),
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

    fn read8(&mut self, addr: Addr) -> u8 {
        match addr {
            Addr::Val(x) => x,
            Addr::A => self.a,
            Addr::X => self.x,
            Addr::Y => self.y,
            Addr::Mem(a) => self.read_mem8(a),
            Addr::MemPC(a) => {
                self.cyc += 1;
                self.read_mem8(a)
            }
            Addr::Rel(_) => unreachable!(),
        }
    }

    fn write8(&mut self, addr: Addr, v: u8) {
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
            Addr::Mem(a) | Addr::MemPC(a) => self.write_mem8(a, v),
            Addr::Val(_) | Addr::Rel(_) => unreachable!(),
        }
    }

    pub fn frame(&mut self) -> &Display {
        let wanted = self.bus.ppu.cycle() - (self.bus.ppu.cycle() % (341 * 262)) + 341 * 262;

        while self.bus.ppu.cycle() < wanted {
            self.tick();

            for _ in 0..3 {
                self.bus.ppu.tick();
            }
        }

        self.bus.ppu.display()
    }

    pub fn tick(&mut self) {
        let opcode = self.fetch8();

        use AddrMode::*;

        let prev_cyc = self.cyc;

        match opcode {
            // ADC
            0x69 => op!(self, adc, Imm, 2),
            0x65 => op!(self, adc, ZP, 3),
            0x75 => op!(self, adc, ZPX, 4),
            0x6D => op!(self, adc, Abs, 4),
            0x7D => op!(self, adc, AbsX, 4),
            0x79 => op!(self, adc, AbsY, 4),
            0x61 => op!(self, adc, IndX, 6),
            0x71 => op!(self, adc, IndY, 5),

            // AND
            0x29 => op!(self, and, Imm, 2),
            0x25 => op!(self, and, ZP, 3),
            0x35 => op!(self, and, ZPX, 4),
            0x2D => op!(self, and, Abs, 4),
            0x3D => op!(self, and, AbsX, 4),
            0x39 => op!(self, and, AbsY, 4),
            0x21 => op!(self, and, IndX, 6),
            0x31 => op!(self, and, IndY, 5),

            // ASL
            0x0A => op!(self, asl, Acc, 2),
            0x06 => op!(self, asl, ZP, 5),
            0x16 => op!(self, asl, ZPX, 6),
            0x0E => op!(self, asl, Abs, 6),
            0x1E => op!(self, asl, AbsX, 7),

            // BCC
            0x90 => op!(self, bcc, Rel, 2),

            // BCS
            0xB0 => op!(self, bcs, Rel, 2),

            // BEQ
            0xF0 => op!(self, beq, Rel, 2),

            // BIT
            0x24 => op!(self, bit, ZP, 3),
            0x2C => op!(self, bit, Abs, 4),

            // BMI
            0x30 => op!(self, bmi, Rel, 2),

            // BNE
            0xD0 => op!(self, bne, Rel, 2),

            // BPL
            0x10 => op!(self, bpl, Rel, 2),

            // BVC
            0x50 => op!(self, bvc, Rel, 2),

            // BVS
            0x70 => op!(self, bvs, Rel, 2),

            // CLC
            0x18 => op!(self, clc, 2),

            // CLD
            0xD8 => op!(self, cld, 2),

            // CLV
            0xB8 => op!(self, clv, 2),

            // CMP
            0xC9 => op!(self, cmp, Imm, 2),
            0xC5 => op!(self, cmp, ZP, 3),
            0xD5 => op!(self, cmp, ZPX, 4),
            0xCD => op!(self, cmp, Abs, 4),
            0xDD => op!(self, cmp, AbsX, 4),
            0xD9 => op!(self, cmp, AbsY, 4),
            0xC1 => op!(self, cmp, IndX, 6),
            0xD1 => op!(self, cmp, IndY, 5),

            // CPX
            0xE0 => op!(self, cpx, Imm, 2),
            0xE4 => op!(self, cpx, ZP, 3),
            0xEC => op!(self, cpx, Abs, 4),

            // CPY
            0xC0 => op!(self, cpy, Imm, 2),
            0xC4 => op!(self, cpy, ZP, 3),
            0xCC => op!(self, cpy, Abs, 4),

            // DEC
            0xC6 => op!(self, dec, ZP, 5),
            0xD6 => op!(self, dec, ZPX, 6),
            0xCE => op!(self, dec, Abs, 6),
            0xDE => op!(self, dec, AbsX, 7),

            // DEX
            0xCA => op!(self, dex, 2),

            // DEY
            0x88 => op!(self, dey, 2),

            // EOR
            0x49 => op!(self, eor, Imm, 2),
            0x45 => op!(self, eor, ZP, 3),
            0x55 => op!(self, eor, ZPX, 4),
            0x4D => op!(self, eor, Abs, 4),
            0x5D => op!(self, eor, AbsX, 4),
            0x59 => op!(self, eor, AbsY, 4),
            0x41 => op!(self, eor, IndX, 6),
            0x51 => op!(self, eor, IndY, 5),

            // INC
            0xE6 => op!(self, inc, ZP, 5),
            0xF6 => op!(self, inc, ZPX, 6),
            0xEE => op!(self, inc, Abs, 6),
            0xFE => op!(self, inc, AbsX, 7),

            // INX
            0xE8 => op!(self, inx, 2),

            // INY
            0xC8 => op!(self, iny, 2),

            // JMP
            0x4C => op!(self, jmp, Abs, 3),
            0x6C => op!(self, jmp, Ind, 5),

            // JSR
            0x20 => op!(self, jsr, Abs, 6),

            // LDA
            0xA9 => op!(self, lda, Imm, 2),
            0xA5 => op!(self, lda, ZP, 3),
            0xB5 => op!(self, lda, ZPX, 4),
            0xAD => op!(self, lda, Abs, 4),
            0xBD => op!(self, lda, AbsX, 4),
            0xB9 => op!(self, lda, AbsY, 4),
            0xA1 => op!(self, lda, IndX, 6),
            0xB1 => op!(self, lda, IndY, 5),

            // LDX
            0xA2 => op!(self, ldx, Imm, 2),
            0xA6 => op!(self, ldx, ZP, 3),
            0xB6 => op!(self, ldx, ZPY, 4),
            0xAE => op!(self, ldx, Abs, 4),
            0xBE => op!(self, ldx, AbsY, 4),

            // LDY
            0xA0 => op!(self, ldy, Imm, 2),
            0xA4 => op!(self, ldy, ZP, 3),
            0xB4 => op!(self, ldy, ZPX, 4),
            0xAC => op!(self, ldy, Abs, 4),
            0xBC => op!(self, ldy, AbsX, 4),

            // LSR
            0x4A => op!(self, lsr, Acc, 2),
            0x46 => op!(self, lsr, ZP, 5),
            0x56 => op!(self, lsr, ZPX, 6),
            0x4E => op!(self, lsr, Abs, 6),
            0x5E => op!(self, lsr, AbsX, 7),

            // NOP
            0xEA => op!(self, nop, 2),

            // ORA
            0x09 => op!(self, ora, Imm, 2),
            0x05 => op!(self, ora, ZP, 3),
            0x15 => op!(self, ora, ZPX, 4),
            0x0D => op!(self, ora, Abs, 4),
            0x1D => op!(self, ora, AbsX, 4),
            0x19 => op!(self, ora, AbsY, 4),
            0x01 => op!(self, ora, IndX, 6),
            0x11 => op!(self, ora, IndY, 5),

            // PHA
            0x48 => op!(self, pha, 3),

            // PHP
            0x08 => op!(self, php, 3),

            // PLA
            0x68 => op!(self, pla, 4),

            // PLP
            0x28 => op!(self, plp, 4),

            // ROL
            0x2A => op!(self, rol, Acc, 2),
            0x26 => op!(self, rol, ZP, 5),
            0x36 => op!(self, rol, ZPX, 6),
            0x2E => op!(self, rol, Abs, 6),
            0x3E => op!(self, rol, AbsX, 7),

            // ROR
            0x6A => op!(self, ror, Acc, 2),
            0x66 => op!(self, ror, ZP, 5),
            0x76 => op!(self, ror, ZPX, 6),
            0x6E => op!(self, ror, Abs, 6),
            0x7E => op!(self, ror, AbsX, 7),

            // RTI
            0x40 => op!(self, rti, 6),

            // RTS
            0x60 => op!(self, rts, 6),

            // SBC
            0xE9 | 0xEB => op!(self, sbc, Imm, 2),
            0xE5 => op!(self, sbc, ZP, 3),
            0xF5 => op!(self, sbc, ZPX, 4),
            0xED => op!(self, sbc, Abs, 4),
            0xFD => op!(self, sbc, AbsX, 4),
            0xF9 => op!(self, sbc, AbsY, 4),
            0xE1 => op!(self, sbc, IndX, 6),
            0xF1 => op!(self, sbc, IndY, 5),

            // SEC
            0x38 => op!(self, sec, 2),

            // SED
            0xF8 => op!(self, sed, 2),

            // SEI
            0x78 => op!(self, sei, 2),

            // STA
            0x85 => op!(self, sta, ZP, 3),
            0x95 => op!(self, sta, ZPX, 4),
            0x8D => op!(self, sta, Abs, 4),
            0x9D => op!(self, sta, AbsX, 5),
            0x99 => op!(self, sta, AbsY, 5),
            0x81 => op!(self, sta, IndX, 6),
            0x91 => op!(self, sta, IndY, 6),

            // STX
            0x86 => op!(self, stx, ZP, 3),
            0x96 => op!(self, stx, ZPY, 4),
            0x8E => op!(self, stx, Abs, 4),

            // STY
            0x84 => op!(self, sty, ZP, 3),
            0x94 => op!(self, sty, ZPX, 4),
            0x8C => op!(self, sty, Abs, 4),

            // TAX
            0xAA => op!(self, tax, 2),

            // TAY
            0xA8 => op!(self, tay, 2),

            // TSX
            0xBA => op!(self, tsx, 2),

            // TXA
            0x8A => op!(self, txa, 2),

            // TXS
            0x9A => op!(self, txs, 2),

            // TYA
            0x98 => op!(self, tya, 2),

            // Illegal opcodes

            // NOP
            0x04 | 0x44 | 0x64 => {
                let _a = self.fetch_addr(ZP);
                self.nop();
                self.cyc += 3;
            }
            0x0c => {
                let _a = self.fetch_addr(Abs);
                self.nop();
                self.cyc += 4;
            }
            0x14 | 0x34 | 0x54 | 0x74 | 0xD4 | 0xF4 => {
                let a = self.fetch_addr(IndX);
                self.read8(a);
                self.nop();
                self.cyc += 4;
            }
            0x1A | 0x3A | 0x5A | 0x7A | 0xDA | 0xFA => op!(self, nop, 2),
            0x80 => {
                let _a = self.fetch_addr(Imm);
                self.nop();
                self.cyc += 2;
            }
            0x1C | 0x3C | 0x5C | 0x7C | 0xDC | 0xFC => {
                let a = self.fetch_addr(AbsX);
                self.read8(a);
                self.nop();
                self.cyc += 4;
            }

            // LAX
            0xA3 => op!(self, lax, IndX, 6),
            0xA7 => op!(self, lax, ZP, 3),
            0xAF => op!(self, lax, Abs, 4),
            0xB3 => op!(self, lax, IndY, 5),
            0xB7 => op!(self, lax, ZPY, 4),
            0xBF => op!(self, lax, AbsY, 4),

            // SAX
            0x83 => op!(self, sax, IndX, 6),
            0x87 => op!(self, sax, ZP, 3),
            0x8F => op!(self, sax, Abs, 4),
            0x97 => op!(self, sax, ZPY, 4),

            // DCP
            0xC3 => op!(self, dcp, IndX, 8),
            0xC7 => op!(self, dcp, ZP, 5),
            0xCF => op!(self, dcp, Abs, 6),
            0xD3 => op!(self, dcp, IndY, 6),
            0xD7 => op!(self, dcp, ZPX, 6),
            0xDB => op!(self, dcp, AbsY, 5),
            0xDF => op!(self, dcp, AbsX, 5),

            // ISC
            0xE3 => op!(self, isc, IndX, 8),
            0xE7 => op!(self, isc, ZP, 5),
            0xEF => op!(self, isc, Abs, 6),
            0xF3 => op!(self, isc, IndY, 6),
            0xF7 => op!(self, isc, ZPX, 6),
            0xFB => op!(self, isc, AbsY, 5),
            0xFF => op!(self, isc, AbsX, 5),

            // SLO
            0x03 => op!(self, slo, IndX, 8),
            0x07 => op!(self, slo, ZP, 5),
            0x0F => op!(self, slo, Abs, 6),
            0x13 => op!(self, slo, IndY, 6),
            0x17 => op!(self, slo, ZPX, 6),
            0x1B => op!(self, slo, AbsY, 5),
            0x1F => op!(self, slo, AbsX, 5),

            // RLA
            0x23 => op!(self, rla, IndX, 8),
            0x27 => op!(self, rla, ZP, 5),
            0x2F => op!(self, rla, Abs, 6),
            0x33 => op!(self, rla, IndY, 6),
            0x37 => op!(self, rla, ZPX, 6),
            0x3B => op!(self, rla, AbsY, 5),
            0x3F => op!(self, rla, AbsX, 5),

            // SRE
            0x43 => op!(self, sre, IndX, 8),
            0x47 => op!(self, sre, ZP, 5),
            0x4F => op!(self, sre, Abs, 6),
            0x53 => op!(self, sre, IndY, 6),
            0x57 => op!(self, sre, ZPX, 6),
            0x5B => op!(self, sre, AbsY, 5),
            0x5F => op!(self, sre, AbsX, 5),

            // RRA
            0x63 => op!(self, rra, IndX, 8),
            0x67 => op!(self, rra, ZP, 5),
            0x6F => op!(self, rra, Abs, 6),
            0x73 => op!(self, rra, IndY, 6),
            0x77 => op!(self, rra, ZPX, 6),
            0x7B => op!(self, rra, AbsY, 5),
            0x7F => op!(self, rra, AbsX, 5),

            _ => {
                panic!("OPCODE {:#04x} not yet implemented", opcode);
            }
        };

        for _ in 0..self.cyc - prev_cyc {
            self.bus.ppu.cycle();
        }
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

    fn push8(&mut self, v: u8) {
        self.write_mem8(0x0100 + self.sp as u16, v);
        self.sp = self.sp.wrapping_sub(1);
    }

    fn push16(&mut self, v: u16) {
        let [l, r] = v.to_le_bytes();
        self.push8(r);
        self.push8(l);
    }

    fn pop8(&mut self) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        self.read_mem8(0x0100 + self.sp as u16)
    }

    fn pop16(&mut self) -> u16 {
        let l = self.pop8();
        let r = self.pop8();
        u16::from_le_bytes([l, r])
    }

    pub fn oam_dma(&mut self, v: u8) {
        // Either 513 or 514 depending if on a put or write cycle, hard to implement
        self.cyc += 514;
        let mut mem_i = (v as u16) << 8;
        for i in 0..256 {
            self.bus.ppu.oam[i] = self.read_mem8(mem_i);
            mem_i += 1;
        }
    }

    // Operations

    fn adc(&mut self, a: Addr) {
        let lhs = self.a as u16;
        let rhs = self.read8(a) as u16;
        let carry = self.get_flag(FLAG_CARRY) as u16;

        let res = lhs + rhs + carry;
        let res8 = res as u8;
        self.a = res8;

        self.update_zero(res8);
        self.update_negative(res8);
        self.set_flag(FLAG_OVERFLOW, !(lhs ^ rhs) & (lhs ^ res) & 0x80 != 0);
        self.set_flag(FLAG_CARRY, res > 0xFF);
    }

    fn and(&mut self, a: Addr) {
        self.a &= self.read8(a);
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn asl(&mut self, a: Addr) {
        let v = self.read8(a);
        let new_v = v << 1;

        self.set_flag(FLAG_CARRY, v & 0x80 != 0);
        self.update_zero(new_v);
        self.update_negative(new_v);

        self.write8(a, new_v);
    }

    fn bcc(&mut self, a: Addr) {
        self.generic_branch(a, |cpu| !cpu.get_flag(FLAG_CARRY));
    }

    fn bcs(&mut self, a: Addr) {
        self.generic_branch(a, |cpu| cpu.get_flag(FLAG_CARRY));
    }

    fn beq(&mut self, a: Addr) {
        self.generic_branch(a, |cpu| cpu.get_flag(FLAG_ZERO));
    }

    fn bit(&mut self, a: Addr) {
        let m = self.read8(a);
        let v = self.a & m;
        self.set_flag(FLAG_OVERFLOW, m & (1 << 6) != 0);
        self.set_flag(FLAG_NEGATIVE, m & (1 << 7) != 0);
        self.set_flag(FLAG_ZERO, v == 0);
    }

    fn bmi(&mut self, a: Addr) {
        self.generic_branch(a, |cpu| cpu.get_flag(FLAG_NEGATIVE));
    }

    fn bne(&mut self, a: Addr) {
        self.generic_branch(a, |cpu| !cpu.get_flag(FLAG_ZERO));
    }

    fn bpl(&mut self, a: Addr) {
        self.generic_branch(a, |cpu| !cpu.get_flag(FLAG_NEGATIVE));
    }

    fn brk(&mut self) {
        self.push16(self.pc);
        self.push8(self.p | FLAG_5 | FLAG_B);
        self.enable_flag(FLAG_INTERRUPT_DISABLE);
        self.pc = self.read_mem16(0xFFFE);
    }

    fn bvc(&mut self, a: Addr) {
        self.generic_branch(a, |cpu| !cpu.get_flag(FLAG_OVERFLOW));
    }

    fn bvs(&mut self, a: Addr) {
        self.generic_branch(a, |cpu| cpu.get_flag(FLAG_OVERFLOW));
    }

    fn clc(&mut self) {
        self.disable_flag(FLAG_CARRY);
    }

    fn cld(&mut self) {
        self.disable_flag(FLAG_DECIMAL);
    }

    fn cli(&mut self) {
        self.disable_flag(FLAG_INTERRUPT_DISABLE);
    }

    fn clv(&mut self) {
        self.disable_flag(FLAG_OVERFLOW);
    }

    fn cmp(&mut self, a: Addr) {
        let v = self.read8(a);
        self.compare(self.a, v);
    }

    fn cpx(&mut self, a: Addr) {
        let v = self.read8(a);
        self.compare(self.x, v);
    }

    fn cpy(&mut self, a: Addr) {
        let v = self.read8(a);
        self.compare(self.y, v);
    }

    fn dcp(&mut self, a: Addr) {
        self.dec(a);
        self.cmp(a);
    }

    fn dec(&mut self, a: Addr) {
        let v = self.read8(a).wrapping_sub(1);
        self.write8(a, v);
        self.update_zero(v);
        self.update_negative(v);
    }

    fn dex(&mut self) {
        self.x = self.x.wrapping_sub(1);
        self.update_zero(self.x);
        self.update_negative(self.x);
    }

    fn dey(&mut self) {
        self.y = self.y.wrapping_sub(1);
        self.update_zero(self.y);
        self.update_negative(self.y);
    }

    fn eor(&mut self, a: Addr) {
        self.a ^= self.read8(a);
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn inc(&mut self, a: Addr) {
        let v = self.read8(a).wrapping_add(1);
        self.write8(a, v);
        self.update_zero(v);
        self.update_negative(v);
    }

    fn inx(&mut self) {
        let v = self.x.wrapping_add(1);
        self.x = v;
        self.update_zero(v);
        self.update_negative(v);
    }

    fn iny(&mut self) {
        let v = self.y.wrapping_add(1);
        self.y = v;
        self.update_zero(v);
        self.update_negative(v);
    }

    fn isc(&mut self, a: Addr) {
        self.inc(a);
        self.sbc(a);
    }

    fn jmp(&mut self, a: Addr) {
        if let Addr::Mem(m) = a {
            self.pc = m;
        } else {
            unreachable!();
        }
    }

    fn jsr(&mut self, a: Addr) {
        if let Addr::Mem(a) = a {
            self.push16(self.pc - 1);
            self.pc = a;
        } else {
            unreachable!();
        }
    }

    fn lax(&mut self, a: Addr) {
        let v = self.read8(a);
        self.a = v;
        self.x = v;

        self.update_zero(v);
        self.update_negative(v);
    }

    fn lda(&mut self, a: Addr) {
        self.a = self.read8(a);
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn ldx(&mut self, a: Addr) {
        self.x = self.read8(a);
        self.update_zero(self.x);
        self.update_negative(self.x);
    }

    fn ldy(&mut self, a: Addr) {
        self.y = self.read8(a);
        self.update_zero(self.y);
        self.update_negative(self.y);
    }

    fn lsr(&mut self, a: Addr) {
        let v = self.read8(a);
        let new_v = v >> 1;

        self.set_flag(FLAG_CARRY, v & 0x1 != 0);
        self.update_zero(new_v);
        self.update_negative(new_v);

        self.write8(a, new_v);
    }

    fn nop(&mut self) {}

    fn ora(&mut self, a: Addr) {
        self.a |= self.read8(a);
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn pha(&mut self) {
        self.push8(self.a);
    }

    fn php(&mut self) {
        let p = self.p | FLAG_5 | FLAG_B;
        self.push8(p);
    }

    fn pla(&mut self) {
        self.a = self.pop8();
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn plp(&mut self) {
        self.p = (self.pop8() & !FLAG_B) | FLAG_5;
    }

    fn rla(&mut self, a: Addr) {
        self.rol(a);
        self.and(a);
    }

    fn rol(&mut self, a: Addr) {
        let v = self.read8(a);
        let mut new_v = v << 1;
        if self.get_flag(FLAG_CARRY) {
            new_v |= 0x01;
        }
        self.set_flag(FLAG_CARRY, v & 0x80 != 0);

        self.update_zero(new_v);
        self.update_negative(new_v);

        self.write8(a, new_v);
    }

    fn ror(&mut self, a: Addr) {
        let v = self.read8(a);
        let mut new_v = v >> 1;
        if self.get_flag(FLAG_CARRY) {
            new_v |= 0x80;
        }
        self.set_flag(FLAG_CARRY, v & 0x01 != 0);

        self.update_zero(new_v);
        self.update_negative(new_v);

        self.write8(a, new_v);
    }

    fn rra(&mut self, a: Addr) {
        self.ror(a);
        self.adc(a);
    }

    fn rti(&mut self) {
        self.p = self.pop8() | FLAG_5;
        self.pc = self.pop16();
    }

    fn rts(&mut self) {
        self.pc = self.pop16() + 1;
    }

    fn sax(&mut self, a: Addr) {
        let v = self.a & self.x;
        self.write8(a, v);
    }

    fn sbc(&mut self, a: Addr) {
        let lhs = self.a as i16;
        let rhs = self.read8(a) as i16;
        let borrow = !self.get_flag(FLAG_CARRY) as i16;

        let res = lhs - rhs - borrow;
        let res8 = res as u8;

        self.update_zero(res8);
        self.update_negative(res8);

        self.set_flag(FLAG_CARRY, res >= 0);
        self.set_flag(FLAG_OVERFLOW, (lhs ^ rhs) & (lhs ^ res) & 0x80 != 0);

        self.a = res8;
    }

    fn sec(&mut self) {
        self.set_flag(FLAG_CARRY, true);
    }

    fn sed(&mut self) {
        self.set_flag(FLAG_DECIMAL, true);
    }

    fn sei(&mut self) {
        self.set_flag(FLAG_INTERRUPT_DISABLE, true);
    }

    fn slo(&mut self, a: Addr) {
        self.asl(a);
        self.ora(a);
    }

    fn sre(&mut self, a: Addr) {
        self.lsr(a);
        self.eor(a);
    }

    fn sta(&mut self, a: Addr) {
        self.write8(a, self.a);
    }

    fn stx(&mut self, a: Addr) {
        self.write8(a, self.x);
    }

    fn sty(&mut self, a: Addr) {
        self.write8(a, self.y);
    }

    fn tax(&mut self) {
        self.x = self.a;
        self.update_zero(self.x);
        self.update_negative(self.x);
    }

    fn tay(&mut self) {
        self.y = self.a;
        self.update_zero(self.y);
        self.update_negative(self.y);
    }

    fn tsx(&mut self) {
        self.x = self.sp;
        self.update_zero(self.x);
        self.update_negative(self.x);
    }

    fn txa(&mut self) {
        self.a = self.x;
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn txs(&mut self) {
        self.sp = self.x;
    }

    fn tya(&mut self) {
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

    fn nmi_interrupt(&mut self) {
        self.push16(self.pc);
        let flags = self.p | FLAG_B;
        self.push8(flags);
        self.set_flag(FLAG_INTERRUPT_DISABLE, true);
        self.pc = self.read_mem16(0xfffa);
        // TODO: Might want to do two ppu cycles here?
    }
}
