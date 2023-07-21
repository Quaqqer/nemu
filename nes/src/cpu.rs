use crate::rom::Rom;

pub struct Cpu {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub pc: u16,
    pub sp: u8,
    pub p: u8,

    pub ram: [u8; 0x800],
    pub ppu_registers: [u8; 0x8],
    pub apu_registers: [u8; 0x18],

    pub rom: Rom,
}

const FLAG_CARRY: u8 = 1 << 0;
const FLAG_ZERO: u8 = 1 << 1;
const FLAG_INTERRUPT_DISABLE: u8 = 1 << 2;
const FLAG_DECIMAL: u8 = 1 << 3;
const FLAG_B: u8 = 0b11 << 4;
const FLAG_OVERFLOW: u8 = 1 << 6;
const FLAG_NEGATIVE: u8 = 1 << 7;

impl Cpu {
    fn read_memory(&self, addr: u16) -> u8 {
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

    fn read_memory_16(&self, addr: u16) -> u16 {
        let l = self.read_memory(addr);
        let r = self.read_memory(addr + 1);
        u16::from_le_bytes([l, r])
    }

    fn write_memory(&mut self, addr: u16, val: u8) {
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

    fn init(&mut self) {
        let start_addr = self.read_memory_16(0xFFFC);
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

            ram: [0x00; 0x800],
            ppu_registers: [0x00; 0x8],
            apu_registers: [0x00; 0x18],

            rom,
        };

        cpu.init();

        cpu
    }

    fn fetch8(&mut self) -> u8 {
        let v = self.read_memory(self.pc);
        self.pc += 1;
        v
    }

    fn fetch16(&mut self) -> u16 {
        let v = self.read_memory_16(self.pc);
        self.pc += 2;
        v
    }

    fn a_imm(&mut self) -> u8 {
        self.fetch8()
    }

    fn a_zp(&mut self) -> u8 {
        let m = self.fetch8() as u16;
        self.read_memory(m)
    }

    fn a_zpx(&mut self) -> u8 {
        let m = self.fetch8().wrapping_add(self.x) as u16;
        self.read_memory(m)
    }

    fn a_zpy(&mut self) -> u8 {
        let m = self.fetch8().wrapping_add(self.y) as u16;
        self.read_memory(m)
    }

    fn a_rel(&mut self) -> i8 {
        let m = self.fetch8() as u16;
        self.read_memory(m) as i8
    }

    fn a_abs(&mut self) -> u8 {
        let m = self.fetch16();
        self.read_memory(m)
    }

    fn a_absx(&mut self) -> (u8, u32) {
        let m = self.fetch16().wrapping_add(self.x as u16);
        let pc = if m > 255 { 1 } else { 0 };
        let v = self.read_memory(m);
        (v, pc)
    }

    fn a_absy(&mut self) -> (u8, u32) {
        let m = self.fetch16().wrapping_add(self.y as u16);
        let pc = if m > 255 { 1 } else { 0 };
        let v = self.read_memory(m);
        (v, pc)
    }

    fn a_ind(&mut self) -> u16 {
        let m = self.fetch16();
        self.read_memory_16(m)
    }

    fn a_indx(&mut self) -> u8 {
        let m = self.fetch8().wrapping_add(self.x);
        self.read_memory(m as u16)
    }

    fn a_indy(&mut self) -> (u8, u32) {
        let m = self.fetch8();
        let m = self.read_memory(m as u16);
        let m = u16::from_le_bytes([m, self.y]);
        let pc = if m > 255 { 1 } else { 0 };
        (self.read_memory(m), pc)
    }

    pub fn cycle(&mut self) -> u32 {
        let opcode = self.fetch8();

        match opcode {
            // ADC
            0x69 => {
                let m = self.a_imm();
                self.op_adc(m);
                2
            }
            0x65 => {
                let m = self.a_zp();
                self.op_adc(m);
                3
            }
            0x75 => {
                let m = self.a_zpx();
                self.op_adc(m);
                4
            }
            0x6D => {
                let m = self.a_abs();
                self.op_adc(m);
                4
            }
            0x7D => {
                let (m, pc) = self.a_absx();
                self.op_adc(m);
                4 + pc
            }
            0x79 => {
                let (m, pc) = self.a_absy();
                self.op_adc(m);
                4 + pc
            }
            0x61 => {
                let m = self.a_indx();
                self.op_adc(m);
                6
            }
            0x71 => {
                let (m, pc) = self.a_indy();
                self.op_adc(m);
                5 + pc
            }

            // AND
            0x29 => {
                let m = self.a_imm();
                self.op_and(m);
                2
            }
            0x25 => {
                let m = self.a_zp();
                self.op_and(m);
                3
            }
            0x35 => {
                let m = self.a_zpx();
                self.op_and(m);
                4
            }
            0x2D => {
                let m = self.a_abs();
                self.op_and(m);
                4
            }
            0x3D => {
                let (m, pc) = self.a_absx();
                self.op_and(m);
                4 + pc
            }
            0x39 => {
                let (m, pc) = self.a_absy();
                self.op_and(m);
                4 + pc
            }
            0x21 => {
                let m = self.a_indx();
                self.op_and(m);
                6
            }
            0x31 => {
                let (m, pc) = self.a_indy();
                self.op_and(m);
                5 + pc
            }

            // JMP
            0x4C => {
                let m = self.fetch16();
                self.op_jmp(m);
                3
            }
            0x6C => {
                let m = self.a_ind();
                self.op_jmp(m);
                5
            }

            // AND
            _ => todo!("OPCODE {:#04x} not yet implemented", opcode),
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

    fn relative_jump(&mut self, d: i8) {
        self.pc = self.pc.wrapping_add_signed(d as i16);
    }

    fn compare(&mut self, v: i8) {
        self.set_flag(FLAG_CARRY, self.a as i8 >= v);
        self.set_flag(FLAG_ZERO, self.a as i8 == v);
        self.set_flag(FLAG_NEGATIVE, (self.a as i8).wrapping_sub(v).is_negative());
    }

    fn push8(&mut self, v: u8) {
        self.write_memory(0x0100 + self.sp as u16, v);
        self.sp = self.sp.wrapping_add(1);
    }

    fn pop8(&mut self) -> u8 {
        let v = self.read_memory(0x0100 + self.sp as u16);
        self.sp = self.sp.wrapping_sub(1);
        v
    }

    fn push16(&mut self, v: u16) {
        let [l, r] = v.to_le_bytes();
        self.push8(l);
        self.push8(r);
    }

    fn pop16(&mut self) -> u16 {
        let r = self.pop8();
        let l = self.pop8();
        u16::from_le_bytes([l, r])
    }

    fn op_adc(&mut self, v: u8) {
        let a = self.a;

        let was_positive = (self.a as i8).is_positive();

        let (a, of1) = a.overflowing_add(if self.get_flag(FLAG_CARRY) { 1 } else { 0 });
        let (a, of2) = a.overflowing_add(v);

        self.a = a;
        self.set_flag(FLAG_CARRY, of1 || of2);
        self.set_flag(FLAG_OVERFLOW, was_positive && (self.a as i8).is_negative());
    }

    fn op_and(&mut self, v: u8) {
        self.a &= v;
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn op_asl(&mut self, v: Option<u8>) {
        let v = if let Some(x) = v { x } else { self.a };
        self.a = v << 1;
        self.set_flag(FLAG_CARRY, v & (1 << 7) != 0);
        self.update_negative(self.a);
    }

    fn op_bcc(&mut self, d: i8) {
        if !self.get_flag(FLAG_CARRY) {
            self.relative_jump(d);
        }
    }

    fn op_bcs(&mut self, d: i8) {
        if self.get_flag(FLAG_CARRY) {
            self.relative_jump(d);
        }
    }

    fn op_beq(&mut self, d: i8) {
        if self.get_flag(FLAG_ZERO) {
            self.relative_jump(d);
        }
    }

    fn op_bit(&mut self, m: u8) {
        let v = self.a & m;
        self.set_flag(FLAG_OVERFLOW, v & (1 << 6) != 0);
        self.set_flag(FLAG_NEGATIVE, v & (1 << 7) != 0);
        self.set_flag(FLAG_ZERO, v == 0);
    }

    fn op_bmi(&mut self, d: i8) {
        if self.get_flag(FLAG_NEGATIVE) {
            self.relative_jump(d);
        }
    }

    fn op_bne(&mut self, d: i8) {
        if !self.get_flag(FLAG_ZERO) {
            self.relative_jump(d);
        }
    }

    fn op_bpl(&mut self, d: i8) {
        if !self.get_flag(FLAG_NEGATIVE) {
            self.relative_jump(d);
        }
    }

    fn op_brk(&mut self) {
        // TODO, what should this do? Nothing?
    }

    fn op_bvc(&mut self, d: i8) {
        if !self.get_flag(FLAG_OVERFLOW) {
            self.relative_jump(d);
        }
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

    fn op_cmp(&mut self, m: i8) {
        self.compare(m);
    }

    fn op_cpx(&mut self) {
        self.compare(self.x as i8);
    }

    fn op_cpy(&mut self) {
        self.compare(self.y as i8);
    }

    fn op_dec(&mut self, addr: u16) {
        let v = self.read_memory(addr).wrapping_sub(1);
        self.write_memory(addr, v);
        self.update_zero(v);
        self.update_negative(v);
    }

    fn op_dex(&mut self) {
        let v = self.x.wrapping_sub(1);
        self.x = v;
        self.update_zero(v);
        self.update_negative(v);
    }

    fn op_dey(&mut self) {
        let v = self.y.wrapping_sub(1);
        self.y = v;
        self.update_zero(v);
        self.update_negative(v);
    }

    fn op_eor(&mut self, m: u8) {
        self.a ^= m;
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn op_inc(&mut self, addr: u16) {
        let v = self.read_memory(addr).wrapping_add(1);
        self.write_memory(addr, v);
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

    fn op_jmp(&mut self, addr: u16) {
        self.pc = addr;
    }

    fn op_jsr(&mut self, addr: u16) {
        self.push16(self.pc - 1);
        self.pc = addr;
    }

    fn op_lda(&mut self, m: u8) {
        self.a = m;
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn op_ldx(&mut self) {
        self.a = self.x;
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn op_ldy(&mut self) {
        self.a = self.y;
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn op_lsr(&mut self, v: Option<u8>) {
        let v = if let Some(x) = v { x } else { self.a };
        self.a = v >> 1;
        self.set_flag(FLAG_CARRY, v & (1 << 0) != 0);
        self.update_negative(self.a);
    }

    fn op_nop(&mut self) {}

    fn op_ora(&mut self, m: u8) {
        self.a |= m;
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn op_pha(&mut self) {
        self.push8(self.a);
    }

    fn op_php(&mut self) {
        self.push8(self.p);
    }

    fn op_pla(&mut self) {
        self.a = self.pop8();
    }

    fn op_plp(&mut self) {
        self.p = self.pop8();
    }

    fn op_rol(&mut self, v: u8) -> u8 {
        let new_v = v << 1 | if self.get_flag(FLAG_CARRY) { 1 } else { 0 };
        self.set_flag(FLAG_CARRY, v & (1 << 7) != 0);
        new_v
    }

    fn op_ror(&mut self, v: u8) -> u8 {
        let new_v = v >> 1 | if self.get_flag(FLAG_CARRY) { 1 << 7 } else { 0 };
        self.set_flag(FLAG_CARRY, v & 1 != 0);
        new_v
    }

    fn op_rti(&mut self) {
        self.p = self.pop8();
        self.pc = self.pop16();
    }

    fn op_rts(&mut self) {
        self.pc = self.pop16();
    }

    fn op_sbc(&mut self) {
        todo!()
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

    fn op_sta(&mut self, addr: u16) {
        self.write_memory(addr, self.a);
    }

    fn op_stx(&mut self, addr: u16) {
        self.write_memory(addr, self.x);
    }

    fn op_sty(&mut self, addr: u16) {
        self.write_memory(addr, self.y);
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
        self.a = self.x;
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn op_tya(&mut self) {
        self.a = self.y;
        self.update_zero(self.a);
        self.update_negative(self.a);
    }
}
