use crate::rom::Rom;

pub struct Cpu {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub pc: u16,
    pub sp: u8,
    pub p: u8,

    pub cyc: u32,

    pub page: u8,

    pub ram: [u8; 0x800],
    pub ppu_registers: [u8; 0x8],
    pub apu_registers: [u8; 0x18],

    pub rom: Rom,
}

const FLAG_CARRY: u8 = 1 << 0;
const FLAG_ZERO: u8 = 1 << 1;
const FLAG_INTERRUPT_DISABLE: u8 = 1 << 2;
const FLAG_DECIMAL: u8 = 1 << 3;
const FLAG_B: u8 = 1 << 4;
const FLAG_5: u8 = 1 << 5;
const FLAG_OVERFLOW: u8 = 1 << 6;
const FLAG_NEGATIVE: u8 = 1 << 7;

impl std::fmt::Debug for Cpu {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Cpu")
            .field("a", &format_args!("{:#04x}", self.a))
            .field("x", &format_args!("{:#04x}", self.x))
            .field("y", &format_args!("{:#04x}", self.y))
            .field("pc", &format_args!("{:#06x}", self.pc))
            .field("sp", &format_args!("{:#04x}", self.sp))
            .field("p", &format_args!("{:#04x}", self.p))
            .field("page", &self.page)
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
    Rel(i8),
}

impl Cpu {
    fn read_mem8(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1FFF => self.ram[addr as usize % 0x800],
            0x2000..=0x3FFF => self.ppu_registers[(addr as usize - 0x2000) % 0x8],
            0x4000..=0x4017 => self.apu_registers[addr as usize - 0x4000],
            0x4018..=0x401F => {
                unimplemented!("APU and I/O functionality that is normally disabled.")
            }
            0x4020..=0xFFFF => self.rom.read8(addr),
        }
    }

    fn read_mem16(&self, addr: u16) -> u16 {
        let l = self.read_mem8(addr);
        let r = self.read_mem8(addr + 1);
        u16::from_le_bytes([l, r])
    }

    fn write_mem8(&mut self, addr: u16, val: u8) {
        match addr {
            0x0000..=0x1FFF => {
                self.ram[addr as usize % 0x800] = val;
            }
            0x2000..=0x3FFF => {
                self.ppu_registers[(addr as usize - 0x2000) % 0x8] = val;
            }
            0x4000..=0x4017 => {
                self.apu_registers[addr as usize - 0x4000] = val;
            }
            0x4018..=0x401F => {
                unimplemented!("APU and I/O functionality that is normally disabled.")
            }
            0x4020..=0xFFFF => {
                self.rom.write8(addr - 0x4020, val);
            }
        }
    }

    fn write_mem16(&mut self, addr: u16, val: u16) {
        let [l, r] = val.to_le_bytes();
        self.write_mem8(addr, l);
        self.write_mem8(addr.wrapping_add(1), r);
    }

    fn init(&mut self) {
        let start_addr = self.read_mem16(0xFFFC);
        self.pc = start_addr;
    }

    pub fn reset(&mut self) {
        self.sp = self.sp.wrapping_sub(3);
        self.p |= FLAG_INTERRUPT_DISABLE;
        self.apu_registers[0x15] = 0x00;
        // TODO: APU triangle phase is reset to 0
        // TODO: APU DPCM output ANDed with 1 (upper 6 bits cleared)
    }

    pub fn new(rom: Rom) -> Self {
        // TODO: All 15 bits of noise channel LSFR = $0000
        // TODO: APU Frame Counter

        let mut cpu = Self {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            sp: 0xFD,
            p: 0x24,

            cyc: 0,

            page: 0,

            ram: [0x00; 0x800],
            ppu_registers: [0x00; 0x8],
            apu_registers: [0x00; 0x18],

            rom,
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

    fn a_acc(&mut self) -> Addr {
        Addr::A
    }

    fn a_imm(&mut self) -> Addr {
        Addr::Val(self.fetch8())
    }

    fn a_zp(&mut self) -> Addr {
        Addr::Mem(self.fetch8() as u16)
    }

    fn a_zpx(&mut self) -> Addr {
        Addr::Mem(self.fetch8().wrapping_add(self.x) as u16)
    }

    fn a_zpy(&mut self) -> Addr {
        Addr::Mem(self.fetch8().wrapping_add(self.y) as u16)
    }

    fn a_rel(&mut self) -> Addr {
        Addr::Rel(self.fetch8() as i8)
    }

    fn a_abs(&mut self) -> Addr {
        Addr::Mem(self.fetch16())
    }

    fn a_absx(&mut self) -> Addr {
        let addr = self.fetch16().wrapping_add(self.x as u16);
        Addr::Mem(addr)
    }

    fn a_absy(&mut self) -> Addr {
        let addr = self.fetch16().wrapping_add(self.y as u16);
        Addr::Mem(addr)
    }

    fn a_ind(&mut self) -> Addr {
        let a = self.fetch16();
        let a = self.read_mem16(a);
        Addr::Mem(a)
    }

    fn a_indx(&mut self) -> Addr {
        let a = self.fetch8().wrapping_add(self.x);
        let a = u16::from_le_bytes([
            self.read_mem8(a as u16),
            self.read_mem8(a.wrapping_add(1) as u16),
        ]);
        Addr::Mem(a)
    }

    fn a_indy(&mut self) -> Addr {
        let a = self.fetch8() as u16;
        let a = u16::from_le_bytes([
            self.read_mem8(a as u16),
            self.read_mem8(a.wrapping_add(1) as u16),
        ]);
        Addr::Mem(a)
    }

    fn read8(&self, addr: Addr) -> u8 {
        match addr {
            Addr::Val(x) => x,
            Addr::A => self.a,
            Addr::X => self.x,
            Addr::Y => self.y,
            Addr::Mem(a) => self.read_mem8(a),
            Addr::Rel(_) => unreachable!(),
        }
    }

    fn read16(&self, addr: Addr) -> u16 {
        match addr {
            Addr::Mem(a) => self.read_mem16(a),
            Addr::Val(_) | Addr::A | Addr::X | Addr::Y | Addr::Rel(_) => unreachable!(),
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
            Addr::Mem(a) => self.write_mem8(a, v),
            Addr::Val(_) | Addr::Rel(_) => unreachable!(),
        }
    }

    pub fn cycle(&mut self) {
        let opcode = self.fetch8();

        match opcode {
            // ADC
            0x69 => {
                let a = self.a_imm();
                self.op_adc(a);
                self.cyc += 2;
            }
            0x65 => {
                let a = self.a_zp();
                self.op_adc(a);
                self.cyc += 3;
            }
            0x75 => {
                let a = self.a_zpx();
                self.op_adc(a);
                self.cyc += 4;
            }
            0x6D => {
                let a = self.a_abs();
                self.op_adc(a);
                self.cyc += 4;
            }
            0x7D => {
                let a = self.a_absx();
                self.op_adc(a);
                self.cyc += 4;
            }
            0x79 => {
                let a = self.a_absy();
                self.op_adc(a);
                self.cyc += 4;
            }
            0x61 => {
                let m = self.a_indx();
                self.op_adc(m);
                self.cyc += 6;
            }
            0x71 => {
                let a = self.a_indy();
                self.op_adc(a);
                self.cyc += 5;
            }

            // AND
            0x29 => {
                let a = self.a_imm();
                self.op_and(a);
                self.cyc += 2;
            }
            0x25 => {
                let a = self.a_zp();
                self.op_and(a);
                self.cyc += 3;
            }
            0x35 => {
                let a = self.a_zpx();
                self.op_and(a);
                self.cyc += 4;
            }
            0x2D => {
                let m = self.a_abs();
                self.op_and(m);
                self.cyc += 4;
            }
            0x3D => {
                let a = self.a_absx();
                self.op_and(a);
                self.cyc += 4;
            }
            0x39 => {
                let a = self.a_absy();
                self.op_and(a);
                self.cyc += 4;
            }
            0x21 => {
                let a = self.a_indx();
                self.op_and(a);
                self.cyc += 6;
            }
            0x31 => {
                let a = self.a_indy();
                self.op_and(a);
                self.cyc += 5;
            }

            // ASL
            0x0A => {
                let a = self.a_acc();
                self.op_asl(a);
                self.cyc += 2;
            }
            0x06 => {
                let a = self.a_zp();
                self.op_asl(a);
                self.cyc += 5;
            }
            0x16 => {
                let a = self.a_zpx();
                self.op_asl(a);
                self.cyc += 6;
            }
            0x0E => {
                let a = self.a_abs();
                self.op_asl(a);
                self.cyc += 6;
            }
            0x1E => {
                let a = self.a_absx();
                self.op_asl(a);
                self.cyc += 7;
            }

            // BCC
            0x90 => {
                let a = self.a_rel();
                let delay = self.op_bcc(a);
                self.cyc += 2 + delay;
            }

            // BCS
            0xB0 => {
                let a = self.a_rel();
                let delay = self.op_bcs(a);
                self.cyc += 2 + delay;
            }

            // BEQ
            0xF0 => {
                let a = self.a_rel();
                let delay = self.op_beq(a);
                self.cyc += 2 + delay;
            }

            // BIT
            0x24 => {
                let a = self.a_zp();
                self.op_bit(a);
                self.cyc += 3;
            }
            0x2C => {
                let a = self.a_abs();
                self.op_bit(a);
                self.cyc += 4;
            }

            // BMI
            0x30 => {
                let a = self.a_rel();
                self.op_bmi(a);
                self.cyc += 2;
            }

            // BNE
            0xD0 => {
                let a = self.a_rel();
                let delay = self.op_bne(a);
                self.cyc += 2 + delay;
            }

            // BPL
            0x10 => {
                let a = self.a_rel();
                let delay = self.op_bpl(a);
                self.cyc += 2 + delay;
            }

            // BVC
            0x50 => {
                let a = self.a_rel();
                let delay = self.op_bvc(a);
                self.cyc += 2 + delay;
            }

            // BVS
            0x70 => {
                let m = self.a_rel();
                let delay = self.op_bvs(m);
                self.cyc += 2 + delay;
            }

            // CLC
            0x18 => {
                self.op_clc();
                self.cyc += 2;
            }

            // CLD
            0xD8 => {
                self.op_cld();
                self.cyc += 2;
            }

            // CLV
            0xB8 => {
                self.op_clv();
                self.cyc += 2;
            }

            // CMP
            0xC9 => {
                let a = self.a_imm();
                self.op_cmp(a);
                self.cyc += 2;
            }
            0xC5 => {
                let a = self.a_zp();
                self.op_cmp(a);
                self.cyc += 3;
            }
            0xD5 => {
                let a = self.a_zpx();
                self.op_cmp(a);
                self.cyc += 4;
            }
            0xCD => {
                let a = self.a_abs();
                self.op_cmp(a);
                self.cyc += 4;
            }
            0xDD => {
                let a = self.a_absx();
                self.op_cmp(a);
                self.cyc += 4;
            }
            0xD9 => {
                let a = self.a_absy();
                self.op_cmp(a);
                self.cyc += 4;
            }
            0xC1 => {
                let a = self.a_indx();
                self.op_cmp(a);
                self.cyc += 6;
            }
            0xD1 => {
                let a = self.a_indy();
                self.op_cmp(a);
                self.cyc += 5;
            }

            // CPX
            0xE0 => {
                let a = self.a_imm();
                self.op_cpx(a);
                self.cyc += 2;
            }
            0xE4 => {
                let a = self.a_zp();
                self.op_cpx(a);
                self.cyc += 3;
            }
            0xEC => {
                let a = self.a_abs();
                self.op_cpx(a);
                self.cyc += 4;
            }

            // CPY
            0xC0 => {
                let a = self.a_imm();
                self.op_cpy(a);
                self.cyc += 2;
            }
            0xC4 => {
                let a = self.a_zp();
                self.op_cpy(a);
                self.cyc += 3;
            }
            0xCC => {
                let a = self.a_abs();
                self.op_cpy(a);
                self.cyc += 4;
            }

            // DEC
            0xC6 => {
                let a = self.a_zp();
                self.op_dec(a);
                self.cyc += 5;
            }
            0xD6 => {
                let a = self.a_zpx();
                self.op_dec(a);
                self.cyc += 6;
            }
            0xCE => {
                let a = self.a_abs();
                self.op_dec(a);
                self.cyc += 6;
            }
            0xDE => {
                let a = self.a_absx();
                self.op_dec(a);
                self.cyc += 7;
            }

            // DEX
            0xCA => {
                self.op_dex();
                self.cyc += 2;
            }

            // DEY
            0x88 => {
                self.op_dey();
                self.cyc += 2;
            }

            // EOR
            0x49 => {
                let a = self.a_imm();
                self.op_eor(a);
                self.cyc += 2;
            }
            0x45 => {
                let a = self.a_zp();
                self.op_eor(a);
                self.cyc += 3;
            }
            0x55 => {
                let a = self.a_zpx();
                self.op_eor(a);
                self.cyc += 4;
            }
            0x4D => {
                let a = self.a_abs();
                self.op_eor(a);
                self.cyc += 4;
            }
            0x5D => {
                let a = self.a_absx();
                self.op_eor(a);
                self.cyc += 4;
            }
            0x59 => {
                let a = self.a_absy();
                self.op_eor(a);
                self.cyc += 4;
            }
            0x41 => {
                let a = self.a_indx();
                self.op_eor(a);
                self.cyc += 6;
            }
            0x51 => {
                let a = self.a_indy();
                self.op_eor(a);
                self.cyc += 5;
            }

            // INC
            0xE6 => {
                let a = self.a_zp();
                self.op_inc(a);
                self.cyc += 5;
            }
            0xF6 => {
                let a = self.a_zpx();
                self.op_inc(a);
                self.cyc += 6;
            }
            0xEE => {
                let a = self.a_abs();
                self.op_inc(a);
                self.cyc += 6;
            }
            0xFE => {
                let a = self.a_absx();
                self.op_inc(a);
                self.cyc += 6;
            }

            // INX
            0xE8 => {
                self.op_inx();
                self.cyc += 2;
            }

            // INY
            0xC8 => {
                self.op_iny();
                self.cyc += 2;
            }

            // JMP
            0x4C => {
                let a = self.a_abs();
                self.op_jmp(a);
                self.cyc += 3;
            }
            0x6C => {
                let a = self.a_ind();
                self.op_jmp(a);
                self.cyc += 5;
            }

            // JSR
            0x20 => {
                let a = self.a_abs();
                self.op_jsr(a);
                self.cyc += 6;
            }

            // LDA
            0xA9 => {
                let a = self.a_imm();
                self.op_lda(a);
                self.cyc += 2;
            }
            0xA5 => {
                let a = self.a_zp();
                self.op_lda(a);
                self.cyc += 3;
            }
            0xB5 => {
                let a = self.a_zpx();
                self.op_lda(a);
                self.cyc += 4;
            }
            0xAD => {
                let m = self.a_abs();
                self.op_lda(m);
                self.cyc += 4;
            }
            0xBD => {
                let a = self.a_absx();
                self.op_lda(a);
                self.cyc += 4;
            }
            0xB9 => {
                let a = self.a_absy();
                self.op_lda(a);
                self.cyc += 4;
            }
            0xA1 => {
                let a = self.a_indx();
                self.op_lda(a);
                self.cyc += 6;
            }
            0xB1 => {
                let a = self.a_indy();
                self.op_lda(a);
                self.cyc += 5;
            }

            // LDX
            0xA2 => {
                let a = self.a_imm();
                self.op_ldx(a);
                self.cyc += 2;
            }
            0xA6 => {
                let m = self.a_zp();
                self.op_ldx(m);
                self.cyc += 3;
            }
            0xB6 => {
                let m = self.a_zpy();
                self.op_ldx(m);
                self.cyc += 4;
            }
            0xAE => {
                let m = self.a_abs();
                self.op_ldx(m);
                self.cyc += 4;
            }
            0xBE => {
                let a = self.a_absy();
                self.op_ldx(a);
                self.cyc += 4;
            }

            // LDY
            0xA0 => {
                let a = self.a_imm();
                self.op_ldy(a);
                self.cyc += 2;
            }
            0xA4 => {
                let a = self.a_zp();
                self.op_ldy(a);
                self.cyc += 3;
            }
            0xB4 => {
                let a = self.a_zpx();
                self.op_ldy(a);
                self.cyc += 4;
            }
            0xAC => {
                let a = self.a_abs();
                self.op_ldy(a);
                self.cyc += 4;
            }
            0xBC => {
                let a = self.a_absx();
                self.op_ldy(a);
                self.cyc += 4;
            }

            // LSR
            0x4A => {
                let a = self.a_acc();
                self.op_lsr(a);
                self.cyc += 2;
            }
            0x46 => {
                let a = self.a_zp();
                self.op_lsr(a);
                self.cyc += 5;
            }
            0x56 => {
                let a = self.a_zpx();
                self.op_lsr(a);
                self.cyc += 6;
            }
            0x4E => {
                let a = self.a_abs();
                self.op_lsr(a);
                self.cyc += 6;
            }
            0x5E => {
                let a = self.a_absx();
                self.op_lsr(a);
                self.cyc += 7;
            }

            // NOP
            0xEA => {
                self.op_nop();
                self.cyc += 2;
            }

            // ORA
            0x09 => {
                let a = self.a_imm();
                self.op_ora(a);
                self.cyc += 2;
            }
            0x05 => {
                let a = self.a_zp();
                self.op_ora(a);
                self.cyc += 3;
            }
            0x15 => {
                let a = self.a_zpx();
                self.op_ora(a);
                self.cyc += 4;
            }
            0x0D => {
                let a = self.a_zpy();
                self.op_ora(a);
                self.cyc += 4;
            }
            0x1D => {
                let a = self.a_absx();
                self.op_ora(a);
                self.cyc += 4;
            }
            0x19 => {
                let a = self.a_absy();
                self.op_ora(a);
                self.cyc += 4;
            }
            0x01 => {
                let a = self.a_indx();
                self.op_ora(a);
                self.cyc += 6;
            }
            0x11 => {
                let a = self.a_indy();
                self.op_ora(a);
                self.cyc += 5;
            }

            // PHA
            0x48 => {
                self.op_pha();
                self.cyc += 3;
            }

            // PHP
            0x08 => {
                self.op_php();
                self.cyc += 3;
            }

            // PLA
            0x68 => {
                self.op_pla();
                self.cyc += 4;
            }

            // PLP
            0x28 => {
                self.op_plp();
                self.cyc += 4;
            }

            // ROL
            0x2A => {
                let a = self.a_acc();
                self.op_rol(a);
                self.cyc += 2;
            }
            0x26 => {
                let a = self.a_zp();
                self.op_rol(a);
                self.cyc += 5;
            }
            0x36 => {
                let a = self.a_zpx();
                self.op_rol(a);
                self.cyc += 6;
            }
            0x2E => {
                let a = self.a_abs();
                self.op_rol(a);
                self.cyc += 6;
            }
            0x3E => {
                let a = self.a_absx();
                self.op_rol(a);
                self.cyc += 7;
            }

            // ROR
            0x6A => {
                let a = self.a_acc();
                self.op_ror(a);
                self.cyc += 2;
            }
            0x66 => {
                let a = self.a_zp();
                self.op_ror(a);
                self.cyc += 5;
            }
            0x76 => {
                let a = self.a_zpx();
                self.op_ror(a);
                self.cyc += 6;
            }
            0x6E => {
                let a = self.a_abs();
                self.op_ror(a);
                self.cyc += 6;
            }
            0x7E => {
                let a = self.a_abs();
                self.op_ror(a);
                self.cyc += 7;
            }

            // RTI
            0x40 => {
                self.op_rti();
                self.cyc += 6;
            }

            // RTS
            0x60 => {
                self.op_rts();
                self.cyc += 6;
            }

            // SBC
            0xE9 => {
                let a = self.a_imm();
                self.op_sbc(a);
                self.cyc += 2;
            }
            0xE5 => {
                let a = self.a_zp();
                self.op_sbc(a);
                self.cyc += 3;
            }
            0xF5 => {
                let a = self.a_zpx();
                self.op_sbc(a);
                self.cyc += 4;
            }
            0xED => {
                let a = self.a_abs();
                self.op_sbc(a);
                self.cyc += 4;
            }
            0xFD => {
                let a = self.a_absx();
                self.op_sbc(a);
                self.cyc += 4;
            }
            0xF9 => {
                let a = self.a_absy();
                self.op_sbc(a);
                self.cyc += 4;
            }
            0xE1 => {
                let a = self.a_indx();
                self.op_sbc(a);
                self.cyc += 6;
            }
            0xF1 => {
                let a = self.a_indy();
                self.op_sbc(a);
                self.cyc += 5;
            }

            // SEC
            0x38 => {
                self.op_sec();
                self.cyc += 2;
            }

            // SED
            0xF8 => {
                self.op_sed();
                self.cyc += 2;
            }

            // SEI
            0x78 => {
                self.op_sei();
                self.cyc += 2;
            }

            // STA
            0x85 => {
                let a = self.a_zp();
                self.op_sta(a);
                self.cyc += 3;
            }
            0x95 => {
                let a = self.a_zpx();
                self.op_sta(a);
                self.cyc += 4;
            }
            0x8D => {
                let a = self.a_abs();
                self.op_sta(a);
                self.cyc += 4;
            }
            0x9D => {
                let a = self.a_absx();
                self.op_sta(a);
                self.cyc += 5;
            }
            0x99 => {
                let a = self.a_absy();
                self.op_sta(a);
                self.cyc += 5;
            }
            0x81 => {
                let a = self.a_indx();
                self.op_sta(a);
                self.cyc += 6;
            }
            0x91 => {
                let a = self.a_indy();
                self.op_sta(a);
                self.cyc += 6;
            }

            // STX
            0x86 => {
                let a = self.a_zp();
                self.op_stx(a);
                self.cyc += 3;
            }
            0x96 => {
                let a = self.a_zpy();
                self.op_stx(a);
                self.cyc += 4;
            }
            0x8E => {
                let a = self.a_abs();
                self.op_stx(a);
                self.cyc += 4;
            }

            // STY
            0x84 => {
                let a = self.a_zp();
                self.op_sty(a);
                self.cyc += 3;
            }
            0x94 => {
                let a = self.a_zpx();
                self.op_sty(a);
                self.cyc += 4;
            }
            0x8C => {
                let a = self.a_abs();
                self.op_sty(a);
                self.cyc += 4;
            }

            // TAX
            0xAA => {
                self.op_tax();
                self.cyc += 2;
            }

            // TAY
            0xA8 => {
                self.op_tay();
                self.cyc += 2;
            }

            // TSX
            0xBA => {
                self.op_tsx();
                self.cyc += 2;
            }

            // TXA
            0x8A => {
                self.op_txa();
                self.cyc += 2;
            }

            // TXS
            0x9A => {
                self.op_txs();
                self.cyc += 2;
            }

            // TYA
            0x98 => {
                self.op_tya();
                self.cyc += 2;
            }

            _ => {
                #[cfg(debug_assertions)]
                panic!("OPCODE {:#04x} not yet implemented", opcode);
            }
        };
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

    fn relative_jump(&mut self, d: i8) -> u32 {
        let old_pc = self.pc;
        let new_pc = self.pc.wrapping_add_signed(d as i16);
        self.pc = new_pc;

        let old_page = (old_pc / 256) as u8;
        let new_page = (new_pc / 256) as u8;

        if new_page != old_page {
            2
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
        let v = self.read_mem8(0x0100 + self.sp as u16);
        v
    }

    fn pop16(&mut self) -> u16 {
        let l = self.pop8();
        let r = self.pop8();
        u16::from_le_bytes([l, r])
    }

    fn op_adc(&mut self, a: Addr) {
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

    fn op_and(&mut self, a: Addr) {
        self.a &= self.read8(a);
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn op_asl(&mut self, a: Addr) {
        let v = self.read8(a);
        let new_v = v << 1;

        self.set_flag(FLAG_CARRY, v & 0x80 != 0);
        self.update_zero(new_v);
        self.update_negative(new_v);

        self.write8(a, new_v);
    }

    fn op_bcc(&mut self, a: Addr) -> u32 {
        self.generic_branch(a, |cpu| !cpu.get_flag(FLAG_CARRY))
    }

    fn op_bcs(&mut self, a: Addr) -> u32 {
        self.generic_branch(a, |cpu| cpu.get_flag(FLAG_CARRY))
    }

    fn op_beq(&mut self, a: Addr) -> u32 {
        self.generic_branch(a, |cpu| cpu.get_flag(FLAG_ZERO))
    }

    fn op_bit(&mut self, a: Addr) {
        let m = self.read8(a);
        let v = self.a & m;
        self.set_flag(FLAG_OVERFLOW, m & (1 << 6) != 0);
        self.set_flag(FLAG_NEGATIVE, m & (1 << 7) != 0);
        self.set_flag(FLAG_ZERO, v == 0);
    }

    fn op_bmi(&mut self, a: Addr) -> u32 {
        self.generic_branch(a, |cpu| cpu.get_flag(FLAG_NEGATIVE))
    }

    fn op_bne(&mut self, a: Addr) -> u32 {
        self.generic_branch(a, |cpu| !cpu.get_flag(FLAG_ZERO))
    }

    fn op_bpl(&mut self, a: Addr) -> u32 {
        self.generic_branch(a, |cpu| !cpu.get_flag(FLAG_NEGATIVE))
    }

    fn op_brk(&mut self) {
        self.push16(self.pc);
        self.push8(self.p | FLAG_5 | FLAG_B);
        self.enable_flag(FLAG_INTERRUPT_DISABLE);
        self.pc = self.read_mem16(0xFFFE);
    }

    fn op_bvc(&mut self, a: Addr) -> u32 {
        self.generic_branch(a, |cpu| !cpu.get_flag(FLAG_OVERFLOW))
    }

    fn op_bvs(&mut self, a: Addr) -> u32 {
        self.generic_branch(a, |cpu| cpu.get_flag(FLAG_OVERFLOW))
    }

    fn op_clc(&mut self) {
        self.disable_flag(FLAG_CARRY);
    }

    fn op_cld(&mut self) {
        self.disable_flag(FLAG_DECIMAL);
    }

    fn op_cli(&mut self) {
        self.disable_flag(FLAG_INTERRUPT_DISABLE);
    }

    fn op_clv(&mut self) {
        self.disable_flag(FLAG_OVERFLOW);
    }

    fn op_cmp(&mut self, a: Addr) {
        self.compare(self.a, self.read8(a));
    }

    fn op_cpx(&mut self, a: Addr) {
        self.compare(self.x, self.read8(a));
    }

    fn op_cpy(&mut self, a: Addr) {
        self.compare(self.y, self.read8(a));
    }

    fn op_dec(&mut self, a: Addr) {
        let v = self.read8(a).wrapping_sub(1);
        self.write8(a, v);
        self.update_zero(v);
        self.update_negative(v);
    }

    fn op_dex(&mut self) {
        self.x = self.x.wrapping_sub(1);
        self.update_zero(self.x);
        self.update_negative(self.x);
    }

    fn op_dey(&mut self) {
        self.y = self.y.wrapping_sub(1);
        self.update_zero(self.y);
        self.update_negative(self.y);
    }

    fn op_eor(&mut self, a: Addr) {
        self.a ^= self.read8(a);
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn op_inc(&mut self, a: Addr) {
        let v = self.read8(a).wrapping_add(1);
        self.write8(a, v);
        self.update_zero(v);
        self.update_negative(v);
    }

    fn op_inx(&mut self) {
        let v = self.x.wrapping_add(1);
        self.x = v;
        self.update_zero(v);
        self.update_negative(v);
    }

    fn op_iny(&mut self) {
        let v = self.y.wrapping_add(1);
        self.y = v;
        self.update_zero(v);
        self.update_negative(v);
    }

    fn op_jmp(&mut self, a: Addr) {
        if let Addr::Mem(m) = a {
            self.pc = m;
        } else {
            unreachable!();
        }
    }

    fn op_jsr(&mut self, a: Addr) {
        if let Addr::Mem(a) = a {
            self.push16(self.pc - 1);
            self.pc = a;
        } else {
            unreachable!();
        }
    }

    fn op_lda(&mut self, a: Addr) {
        self.a = self.read8(a);
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn op_ldx(&mut self, a: Addr) {
        self.x = self.read8(a);
        self.update_zero(self.x);
        self.update_negative(self.x);
    }

    fn op_ldy(&mut self, a: Addr) {
        self.y = self.read8(a);
        self.update_zero(self.y);
        self.update_negative(self.y);
    }

    fn op_lsr(&mut self, a: Addr) {
        let v = self.read8(a);
        let new_v = v >> 1;

        self.set_flag(FLAG_CARRY, v & 0x1 != 0);
        self.update_zero(new_v);
        self.update_negative(new_v);

        self.write8(a, new_v);
    }

    fn op_nop(&mut self) {}

    fn op_ora(&mut self, a: Addr) {
        self.a |= self.read8(a);
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn op_pha(&mut self) {
        self.push8(self.a);
    }

    fn op_php(&mut self) {
        let p = self.p | FLAG_5 | FLAG_B;
        self.push8(p);
    }

    fn op_pla(&mut self) {
        self.a = self.pop8();
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn op_plp(&mut self) {
        self.p = (self.pop8() & !FLAG_B) | FLAG_5;
    }

    fn op_rol(&mut self, a: Addr) {
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

    fn op_ror(&mut self, a: Addr) {
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

    fn op_rti(&mut self) {
        self.p = self.pop8() | FLAG_5;
        self.pc = self.pop16();
    }

    fn op_rts(&mut self) {
        self.pc = self.pop16() + 1;
    }

    fn op_sbc(&mut self, a: Addr) {
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

    fn op_sec(&mut self) {
        self.set_flag(FLAG_CARRY, true);
    }

    fn op_sed(&mut self) {
        self.set_flag(FLAG_DECIMAL, true);
    }

    fn op_sei(&mut self) {
        self.set_flag(FLAG_INTERRUPT_DISABLE, true);
    }

    fn op_sta(&mut self, a: Addr) {
        self.write8(a, self.a);
    }

    fn op_stx(&mut self, a: Addr) {
        self.write8(a, self.x);
    }

    fn op_sty(&mut self, a: Addr) {
        self.write8(a, self.y);
    }

    fn op_tax(&mut self) {
        self.x = self.a;
        self.update_zero(self.x);
        self.update_negative(self.x);
    }

    fn op_tay(&mut self) {
        self.y = self.a;
        self.update_zero(self.y);
        self.update_negative(self.y);
    }

    fn op_tsx(&mut self) {
        self.x = self.sp;
        self.update_zero(self.x);
        self.update_negative(self.x);
    }

    fn op_txa(&mut self) {
        self.a = self.x;
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn op_txs(&mut self) {
        self.sp = self.x;
    }

    fn op_tya(&mut self) {
        self.a = self.y;
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn generic_branch<F>(&mut self, addr: Addr, cond: F) -> u32
    where
        F: FnOnce(&Cpu) -> bool,
    {
        if let Addr::Rel(d) = addr {
            if cond(self) {
                self.relative_jump(d) + 1
            } else {
                0
            }
        } else {
            unreachable!()
        }
    }
}
