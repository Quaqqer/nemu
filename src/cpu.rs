use crate::apu::Apu;
use crate::op::{AddrMode, Op, OPCODE_MATRIX};
use crate::ppu::Ppu;
use crate::{cart::Cart, ppu::Display};

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

pub struct CpuBus<'a> {
    pub apu: &'a mut Apu,
    pub ppu: &'a mut Ppu,
    pub cart: &'a mut Cart,
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
            .field("cyc", &self.cyc)
            .finish()
    }
}

#[derive(Debug, Clone, Copy)]
enum Addr {
    A,
    Mem(u16),
    /// Memory with page crossed
    MemPC(u16),
    Rel(i8),
    None,
}

impl Cpu {
    fn read_mem8(&mut self, bus: &mut CpuBus, addr: u16) -> u8 {
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

    pub fn inspect_mem8(&self, bus: &mut CpuBus, addr: u16) -> Option<u8> {
        match addr {
            0x0000..=0x1FFF => self.ram.get(addr as usize % 0x800).copied(),
            0x2000..=0x3FFF => None,
            0x4000..=0x4017 => None,
            0x4018..=0x401F => {
                unimplemented!("APU and I/O functionality that is normally disabled.")
            }
            0x4020..=0xFFFF => Some(bus.cart.read8(addr)),
        }
    }

    fn read_mem16(&mut self, bus: &mut CpuBus, addr: u16) -> u16 {
        let l = self.read_mem8(bus, addr);
        let r = self.read_mem8(bus, addr.wrapping_add(1));
        u16::from_le_bytes([l, r])
    }

    pub fn inspect_mem16(&self, bus: &mut CpuBus, addr: u16) -> Option<u16> {
        let l = self.inspect_mem8(bus, addr);
        let r = self.inspect_mem8(bus, addr.wrapping_add(1));
        if let Some(l) = l
            && let Some(r) = r
        {
            Some(u16::from_le_bytes([l, r]))
        } else {
            None
        }
    }

    /// Read 16 bytes from memory, with page wrapping.
    ///
    /// If the adress is at the end of a page the second byte wraps around to the start of the same
    /// page.
    ///
    /// * `addr`: The memory address
    fn read_mem16_pw(&mut self, bus: &mut CpuBus, addr: u16) -> u16 {
        let l = self.read_mem8(bus, addr);
        let r = self.read_mem8(bus, (addr.wrapping_add(1) & 0xFF) | (addr & 0xFF00));
        u16::from_le_bytes([l, r])
    }

    fn write_mem8(&mut self, bus: &mut CpuBus, addr: u16, val: u8) {
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

    pub(crate) fn init(&mut self, bus: &mut CpuBus) {
        let start_addr = self.read_mem16(bus, 0xFFFC);
        self.pc = start_addr;
    }

    pub(crate) fn reset(&mut self) {
        self.sp = self.sp.wrapping_sub(3);
        self.p |= FLAG_INTERRUPT_DISABLE;
    }

    pub fn new() -> Self {
        let mut cpu = Self {
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

    fn fetch8(&mut self, bus: &mut CpuBus) -> u8 {
        let v = self.read_mem8(bus, self.pc);
        self.pc += 1;
        v
    }

    fn fetch16(&mut self, bus: &mut CpuBus) -> u16 {
        let v = self.read_mem16(bus, self.pc);
        self.pc += 2;
        v
    }

    fn fetch_addr(&mut self, bus: &mut CpuBus, m: AddrMode, pc_add: bool) -> Addr {
        use AddrMode::*;
        match m {
            Acc => Addr::A,
            Imm => {
                let v = Addr::Mem(self.pc);
                self.pc = self.pc.wrapping_add(1);
                v
            }
            Zp0 => Addr::Mem(self.fetch8(bus) as u16),
            ZpX => Addr::Mem(self.fetch8(bus).wrapping_add(self.x) as u16),
            ZpY => Addr::Mem(self.fetch8(bus).wrapping_add(self.y) as u16),
            Rel => Addr::Rel(self.fetch8(bus) as i8),
            Abs => Addr::Mem(self.fetch16(bus)),
            AbX => {
                let abs = self.fetch16(bus);
                let a = abs.wrapping_add(self.x as u16);

                if pc_add && a & 0xFF < abs & 0xFF {
                    Addr::MemPC(a)
                } else {
                    Addr::Mem(a)
                }
            }
            AbY => {
                let abs = self.fetch16(bus);
                let a = abs.wrapping_add(self.y as u16);

                if pc_add && a & 0xFF < abs & 0xFF {
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
            IdX => {
                let a = self.fetch8(bus).wrapping_add(self.x);
                let a = self.read_mem16_pw(bus, a as u16);
                Addr::Mem(a)
            }
            IdY => {
                let a = self.fetch8(bus);
                let abs = self.read_mem16_pw(bus, a as u16);
                let a = abs.wrapping_add(self.y as u16);

                if pc_add && a & 0xFF < abs & 0xFF {
                    Addr::MemPC(a)
                } else {
                    Addr::Mem(a)
                }
            }
        }
    }

    fn read8(&mut self, bus: &mut CpuBus, addr: Addr) -> u8 {
        match addr {
            Addr::A => self.a,
            Addr::Mem(a) => self.read_mem8(bus, a),
            Addr::MemPC(a) => {
                self.cyc += 1;
                self.read_mem8(bus, a)
            }
            Addr::Rel(_) | Addr::None => unreachable!(),
        }
    }

    fn write8(&mut self, bus: &mut CpuBus, addr: Addr, v: u8) {
        match addr {
            Addr::A => {
                self.a = v;
            }
            Addr::Mem(a) | Addr::MemPC(a) => self.write_mem8(bus, a, v),
            Addr::None | Addr::Rel(_) => unreachable!(),
        }
    }

    pub fn tick(&mut self, bus: &mut CpuBus) -> u64 {
        let start_cycle = self.cyc;

        let opcode = self.fetch8(bus);

        let (op, addr_mode, cycles, pc_add) = OPCODE_MATRIX[opcode as usize];
        let addr = self.fetch_addr(bus, addr_mode, pc_add);

        self.cyc += cycles as u64;

        match op {
            Op::Adc => self.adc(bus, addr),
            Op::And => self.and(bus, addr),
            Op::Asl => self.asl(bus, addr),
            Op::Bcc => self.bcc(bus, addr),
            Op::Bcs => self.bcs(bus, addr),
            Op::Beq => self.beq(bus, addr),
            Op::Bit => self.bit(bus, addr),
            Op::Bmi => self.bmi(bus, addr),
            Op::Bne => self.bne(bus, addr),
            Op::Bpl => self.bpl(bus, addr),
            Op::Brk => self.brk(bus),
            Op::Bvc => self.bvc(bus, addr),
            Op::Bvs => self.bvs(bus, addr),
            Op::Clc => self.clc(bus),
            Op::Cld => self.cld(bus),
            Op::Cli => self.cli(bus),
            Op::Clv => self.clv(bus),
            Op::Cmp => self.cmp(bus, addr),
            Op::Cpx => self.cpx(bus, addr),
            Op::Cpy => self.cpy(bus, addr),
            Op::Dec => self.dec(bus, addr),
            Op::Dex => self.dex(bus),
            Op::Dey => self.dey(bus),
            Op::Eor => self.eor(bus, addr),
            Op::Inc => self.inc(bus, addr),
            Op::Inx => self.inx(bus),
            Op::Iny => self.iny(bus),
            Op::Jmp => self.jmp(bus, addr),
            Op::Jsr => self.jsr(bus, addr),
            Op::Lda => self.lda(bus, addr),
            Op::Ldx => self.ldx(bus, addr),
            Op::Ldy => self.ldy(bus, addr),
            Op::Lsr => self.lsr(bus, addr),
            Op::Nop => self.nop(bus, addr),
            Op::Ora => self.ora(bus, addr),
            Op::Pha => self.pha(bus),
            Op::Php => self.php(bus),
            Op::Pla => self.pla(bus),
            Op::Plp => self.plp(bus),
            Op::Rol => self.rol(bus, addr),
            Op::Ror => self.ror(bus, addr),
            Op::Rti => self.rti(bus),
            Op::Rts => self.rts(bus),
            Op::Sbc => self.sbc(bus, addr),
            Op::Sec => self.sec(bus),
            Op::Sed => self.sed(bus),
            Op::Sei => self.sei(bus),
            Op::Sta => self.sta(bus, addr),
            Op::Stx => self.stx(bus, addr),
            Op::Sty => self.sty(bus, addr),
            Op::Tax => self.tax(bus),
            Op::Tay => self.tay(bus),
            Op::Tsx => self.tsx(bus),
            Op::Txa => self.txa(bus),
            Op::Txs => self.txs(bus),
            Op::Tya => self.tya(bus),

            // Illegal opcodes
            Op::Ahx => self.ahx(bus),
            Op::Alr => self.alr(bus),
            Op::Anc => self.anc(bus),
            Op::Arr => self.arr(bus),
            Op::Axs => self.axs(bus),
            Op::Dcp => self.dcp(bus, addr),
            Op::Kil => self.kil(bus),
            Op::Isc => self.isc(bus, addr),
            Op::Las => self.las(bus),
            Op::Lax => self.lax(bus, addr),
            Op::Rla => self.rla(bus, addr),
            Op::Rra => self.rra(bus, addr),
            Op::Sax => self.sax(bus, addr),
            Op::Shx => self.shx(bus),
            Op::Shy => self.shy(bus),
            Op::Slo => self.slo(bus, addr),
            Op::Sre => self.sre(bus, addr),
            Op::Tas => self.tas(bus),
            Op::Xaa => self.xaa(bus),
        }

        self.cyc - start_cycle
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

    fn push8(&mut self, bus: &mut CpuBus, v: u8) {
        self.write_mem8(bus, 0x0100 + self.sp as u16, v);
        self.sp = self.sp.wrapping_sub(1);
    }

    fn push16(&mut self, bus: &mut CpuBus, v: u16) {
        let [l, r] = v.to_le_bytes();
        self.push8(bus, r);
        self.push8(bus, l);
    }

    fn pop8(&mut self, bus: &mut CpuBus) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        self.read_mem8(bus, 0x0100 + self.sp as u16)
    }

    fn pop16(&mut self, bus: &mut CpuBus) -> u16 {
        let l = self.pop8(bus);
        let r = self.pop8(bus);
        u16::from_le_bytes([l, r])
    }

    pub fn oam_dma(&mut self, bus: &mut CpuBus, v: u8) {
        // Either 513 or 514 depending if on a put or write cycle, hard to implement
        self.cyc += 514;
        let mut mem_i = (v as u16) << 8;
        for i in 0..256 {
            bus.ppu.oam[i] = self.read_mem8(bus, mem_i);
            mem_i += 1;
        }
    }

    // Operations

    fn adc(&mut self, bus: &mut CpuBus, a: Addr) {
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

    fn and(&mut self, bus: &mut CpuBus, a: Addr) {
        self.a &= self.read8(bus, a);
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn asl(&mut self, bus: &mut CpuBus, a: Addr) {
        let v = self.read8(bus, a);
        let new_v = v << 1;

        self.set_flag(FLAG_CARRY, v & 0x80 != 0);
        self.update_zero(new_v);
        self.update_negative(new_v);

        self.write8(bus, a, new_v);
    }

    fn bcc(&mut self, bus: &mut CpuBus, a: Addr) {
        self.generic_branch(a, |cpu| !cpu.get_flag(FLAG_CARRY));
    }

    fn bcs(&mut self, bus: &mut CpuBus, a: Addr) {
        self.generic_branch(a, |cpu| cpu.get_flag(FLAG_CARRY));
    }

    fn beq(&mut self, bus: &mut CpuBus, a: Addr) {
        self.generic_branch(a, |cpu| cpu.get_flag(FLAG_ZERO));
    }

    fn bit(&mut self, bus: &mut CpuBus, a: Addr) {
        let m = self.read8(bus, a);
        let v = self.a & m;
        self.set_flag(FLAG_OVERFLOW, m & (1 << 6) != 0);
        self.set_flag(FLAG_NEGATIVE, m & (1 << 7) != 0);
        self.set_flag(FLAG_ZERO, v == 0);
    }

    fn bmi(&mut self, bus: &mut CpuBus, a: Addr) {
        self.generic_branch(a, |cpu| cpu.get_flag(FLAG_NEGATIVE));
    }

    fn bne(&mut self, bus: &mut CpuBus, a: Addr) {
        self.generic_branch(a, |cpu| !cpu.get_flag(FLAG_ZERO));
    }

    fn bpl(&mut self, bus: &mut CpuBus, a: Addr) {
        self.generic_branch(a, |cpu| !cpu.get_flag(FLAG_NEGATIVE));
    }

    fn brk(&mut self, bus: &mut CpuBus) {
        self.push16(bus, self.pc);
        self.push8(bus, self.p | FLAG_5 | FLAG_B);
        self.enable_flag(FLAG_INTERRUPT_DISABLE);
        self.pc = self.read_mem16(bus, 0xFFFE);
    }

    fn bvc(&mut self, bus: &mut CpuBus, a: Addr) {
        self.generic_branch(a, |cpu| !cpu.get_flag(FLAG_OVERFLOW));
    }

    fn bvs(&mut self, bus: &mut CpuBus, a: Addr) {
        self.generic_branch(a, |cpu| cpu.get_flag(FLAG_OVERFLOW));
    }

    fn clc(&mut self, bus: &mut CpuBus) {
        self.disable_flag(FLAG_CARRY);
    }

    fn cld(&mut self, bus: &mut CpuBus) {
        self.disable_flag(FLAG_DECIMAL);
    }

    fn cli(&mut self, bus: &mut CpuBus) {
        self.disable_flag(FLAG_INTERRUPT_DISABLE);
    }

    fn clv(&mut self, bus: &mut CpuBus) {
        self.disable_flag(FLAG_OVERFLOW);
    }

    fn cmp(&mut self, bus: &mut CpuBus, a: Addr) {
        let v = self.read8(bus, a);
        self.compare(self.a, v);
    }

    fn cpx(&mut self, bus: &mut CpuBus, a: Addr) {
        let v = self.read8(bus, a);
        self.compare(self.x, v);
    }

    fn cpy(&mut self, bus: &mut CpuBus, a: Addr) {
        let v = self.read8(bus, a);
        self.compare(self.y, v);
    }

    fn dcp(&mut self, bus: &mut CpuBus, a: Addr) {
        let v = self.read8(bus, a).wrapping_sub(1);
        self.write8(bus, a, v);
        self.update_zero(v);
        self.update_negative(v);
        self.compare(self.a, v);
    }

    fn dec(&mut self, bus: &mut CpuBus, a: Addr) {
        let v = self.read8(bus, a).wrapping_sub(1);
        self.write8(bus, a, v);
        self.update_zero(v);
        self.update_negative(v);
    }

    fn dex(&mut self, bus: &mut CpuBus) {
        self.x = self.x.wrapping_sub(1);
        self.update_zero(self.x);
        self.update_negative(self.x);
    }

    fn dey(&mut self, bus: &mut CpuBus) {
        self.y = self.y.wrapping_sub(1);
        self.update_zero(self.y);
        self.update_negative(self.y);
    }

    fn eor(&mut self, bus: &mut CpuBus, a: Addr) {
        self.a ^= self.read8(bus, a);
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn inc(&mut self, bus: &mut CpuBus, a: Addr) {
        let v = self.read8(bus, a).wrapping_add(1);
        self.write8(bus, a, v);
        self.update_zero(v);
        self.update_negative(v);
    }

    fn inx(&mut self, bus: &mut CpuBus) {
        let v = self.x.wrapping_add(1);
        self.x = v;
        self.update_zero(v);
        self.update_negative(v);
    }

    fn iny(&mut self, bus: &mut CpuBus) {
        let v = self.y.wrapping_add(1);
        self.y = v;
        self.update_zero(v);
        self.update_negative(v);
    }

    fn isc(&mut self, bus: &mut CpuBus, a: Addr) {
        self.inc(bus, a);
        self.sbc(bus, a);
    }

    fn jmp(&mut self, bus: &mut CpuBus, a: Addr) {
        if let Addr::Mem(m) = a {
            self.pc = m;
        } else {
            unreachable!();
        }
    }

    fn jsr(&mut self, bus: &mut CpuBus, a: Addr) {
        if let Addr::Mem(a) = a {
            self.push16(bus, self.pc - 1);
            self.pc = a;
        } else {
            unreachable!();
        }
    }

    fn lax(&mut self, bus: &mut CpuBus, a: Addr) {
        let v = self.read8(bus, a);
        self.a = v;
        self.x = v;

        self.update_zero(v);
        self.update_negative(v);
    }

    fn lda(&mut self, bus: &mut CpuBus, a: Addr) {
        self.a = self.read8(bus, a);
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn ldx(&mut self, bus: &mut CpuBus, a: Addr) {
        self.x = self.read8(bus, a);
        self.update_zero(self.x);
        self.update_negative(self.x);
    }

    fn ldy(&mut self, bus: &mut CpuBus, a: Addr) {
        self.y = self.read8(bus, a);
        self.update_zero(self.y);
        self.update_negative(self.y);
    }

    fn lsr(&mut self, bus: &mut CpuBus, a: Addr) {
        let v = self.read8(bus, a);
        let new_v = v >> 1;

        self.set_flag(FLAG_CARRY, v & 0x1 != 0);
        self.update_zero(new_v);
        self.update_negative(new_v);

        self.write8(bus, a, new_v);
    }

    fn nop(&mut self, bus: &mut CpuBus, a: Addr) {
        self.read8(bus, a);
    }

    fn ora(&mut self, bus: &mut CpuBus, a: Addr) {
        self.a |= self.read8(bus, a);
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn pha(&mut self, bus: &mut CpuBus) {
        self.push8(bus, self.a);
    }

    fn php(&mut self, bus: &mut CpuBus) {
        let p = self.p | FLAG_5 | FLAG_B;
        self.push8(bus, p);
    }

    fn pla(&mut self, bus: &mut CpuBus) {
        self.a = self.pop8(bus);
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn plp(&mut self, bus: &mut CpuBus) {
        self.p = (self.pop8(bus) & !FLAG_B) | FLAG_5;
    }

    fn rla(&mut self, bus: &mut CpuBus, a: Addr) {
        self.rol(bus, a);
        self.and(bus, a);
    }

    fn rol(&mut self, bus: &mut CpuBus, a: Addr) {
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

    fn ror(&mut self, bus: &mut CpuBus, a: Addr) {
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

    fn rra(&mut self, bus: &mut CpuBus, a: Addr) {
        self.ror(bus, a);
        self.adc(bus, a);
    }

    fn rti(&mut self, bus: &mut CpuBus) {
        self.p = self.pop8(bus) | FLAG_5;
        self.pc = self.pop16(bus);
    }

    fn rts(&mut self, bus: &mut CpuBus) {
        self.pc = self.pop16(bus) + 1;
    }

    fn sax(&mut self, bus: &mut CpuBus, a: Addr) {
        let v = self.a & self.x;
        self.write8(bus, a, v);
    }

    fn sbc(&mut self, bus: &mut CpuBus, a: Addr) {
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

    fn sec(&mut self, bus: &mut CpuBus) {
        self.set_flag(FLAG_CARRY, true);
    }

    fn sed(&mut self, bus: &mut CpuBus) {
        self.set_flag(FLAG_DECIMAL, true);
    }

    fn sei(&mut self, bus: &mut CpuBus) {
        self.set_flag(FLAG_INTERRUPT_DISABLE, true);
    }

    fn slo(&mut self, bus: &mut CpuBus, a: Addr) {
        self.asl(bus, a);
        self.ora(bus, a);
    }

    fn sre(&mut self, bus: &mut CpuBus, a: Addr) {
        self.lsr(bus, a);
        self.eor(bus, a);
    }

    fn sta(&mut self, bus: &mut CpuBus, a: Addr) {
        self.write8(bus, a, self.a);
    }

    fn stx(&mut self, bus: &mut CpuBus, a: Addr) {
        self.write8(bus, a, self.x);
    }

    fn sty(&mut self, bus: &mut CpuBus, a: Addr) {
        self.write8(bus, a, self.y);
    }

    fn tax(&mut self, bus: &mut CpuBus) {
        self.x = self.a;
        self.update_zero(self.x);
        self.update_negative(self.x);
    }

    fn tay(&mut self, bus: &mut CpuBus) {
        self.y = self.a;
        self.update_zero(self.y);
        self.update_negative(self.y);
    }

    fn tsx(&mut self, bus: &mut CpuBus) {
        self.x = self.sp;
        self.update_zero(self.x);
        self.update_negative(self.x);
    }

    fn txa(&mut self, bus: &mut CpuBus) {
        self.a = self.x;
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn txs(&mut self, bus: &mut CpuBus) {
        self.sp = self.x;
    }

    fn tya(&mut self, bus: &mut CpuBus) {
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

    #[allow(unused)]
    fn nmi_interrupt(&mut self, bus: &mut CpuBus) {
        self.push16(bus, self.pc);
        let flags = self.p | FLAG_B;
        self.push8(bus, flags);
        self.set_flag(FLAG_INTERRUPT_DISABLE, true);
        self.pc = self.read_mem16(bus, 0xfffa);
        // TODO: Might want to do two ppu cycles here?
    }

    fn tas(&self, bus: &mut CpuBus) {
        // Illegal opcode not implemented
    }

    fn xaa(&self, bus: &mut CpuBus) {
        // Illegal opcode not implemented
    }

    fn shy(&self, bus: &mut CpuBus) {
        // Illegal opcode not implemented
    }

    fn shx(&self, bus: &mut CpuBus) {
        // Illegal opcode not implemented
    }

    fn las(&self, bus: &mut CpuBus) {
        // Illegal opcode not implemented
    }

    fn axs(&self, bus: &mut CpuBus) {
        // Illegal opcode not implemented
    }

    fn kil(&self, bus: &mut CpuBus) {
        // Illegal opcode not implemented
    }

    fn arr(&self, bus: &mut CpuBus) {
        // Illegal opcode not implemented
    }

    fn anc(&self, bus: &mut CpuBus) {
        // Illegal opcode not implemented
    }

    fn alr(&self, bus: &mut CpuBus) {
        // Illegal opcode not implemented
    }

    fn ahx(&self, bus: &mut CpuBus) {
        // Illegal opcode not implemented
    }
}
