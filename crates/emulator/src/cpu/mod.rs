pub mod op;

use bitflags::bitflags;

use crate::cpu::op::{AddrMode, Op, OPCODE_MATRIX};

/// The status register
#[derive(Clone, Copy)]
pub struct P(u8);

bitflags! {
    impl P: u8 {
        const CARRY             = 0b00000001;
        const ZERO              = 0b00000010;
        const INTERRUPT_DISABLE = 0b00000100;
        const DECIMAL           = 0b00001000;
        const B                 = 0b00010000;
        const _5                = 0b00100000;
        const OVERFLOW          = 0b01000000;
        const NEGATIVE          = 0b10000000;
    }
}

impl std::fmt::Display for P {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        crate::util::fmt_bitflags_u8(self.bits(), ['n', 'o', '5', 'b', 'd', 'i', 'z', 'c'], f)
    }
}

#[derive(Clone)]
pub struct Cpu {
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub pc: u16,
    pub sp: u8,
    pub p: P,

    pub cyc: u64,

    pub history: [u16; Self::HISTORY_LEN],
    pub history_i: usize,
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

pub trait CpuMemory {
    fn read(&mut self, cpu: &mut Cpu, addr: u16) -> u8;

    fn inspect(&self, cpu: &Cpu, addr: u16) -> Option<u8>;

    fn write(&mut self, cpu: &mut Cpu, addr: u16, v: u8);
}

#[derive(Debug, Clone, Copy)]
enum Addr {
    A,
    Mem(u16),
    /// Memory with page crossed
    MemPC(u16),
    Rel(i8),
}

impl Cpu {
    pub const HISTORY_LEN: usize = 64;

    fn read_mem16<Mem: CpuMemory>(&mut self, mem: &mut Mem, addr: u16) -> u16 {
        let l = mem.read(self, addr);
        let r = mem.read(self, addr.wrapping_add(1));
        u16::from_le_bytes([l, r])
    }

    pub fn inspect_mem16<Mem: CpuMemory>(&self, mem: &Mem, addr: u16) -> Option<u16> {
        let l = mem.inspect(self, addr);
        let r = mem.inspect(self, addr.wrapping_add(1));
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
    fn read_mem16_pw<Mem: CpuMemory>(&mut self, mem: &mut Mem, addr: u16) -> u16 {
        let l = mem.read(self, addr);
        let r = mem.read(self, (addr.wrapping_add(1) & 0xFF) | (addr & 0xFF00));
        u16::from_le_bytes([l, r])
    }

    pub(crate) fn init<Mem: CpuMemory>(&mut self, mem: &mut Mem) {
        let start_addr = self.read_mem16(mem, 0xFFFC);
        self.pc = start_addr;
    }

    pub(crate) fn reset(&mut self) {
        self.sp = self.sp.wrapping_sub(3);
        self.p |= P::INTERRUPT_DISABLE;
    }

    pub fn new() -> Self {
        Self {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            sp: 0xFD,
            p: P::INTERRUPT_DISABLE | P::_5,

            cyc: 7,

            history: [0x0; 64],
            history_i: Self::HISTORY_LEN - 1,
        }
    }

    fn fetch8<Mem: CpuMemory>(&mut self, mem: &mut Mem) -> u8 {
        let v = mem.read(self, self.pc);
        self.pc += 1;
        v
    }

    fn fetch16<Mem: CpuMemory>(&mut self, mem: &mut Mem) -> u16 {
        let v = self.read_mem16(mem, self.pc);
        self.pc += 2;
        v
    }

    fn fetch_addr<Mem: CpuMemory>(&mut self, mem: &mut Mem, m: AddrMode, pc_add: bool) -> Addr {
        use AddrMode::*;
        match m {
            Acc => Addr::A,
            Imm => {
                let v = Addr::Mem(self.pc);
                self.pc = self.pc.wrapping_add(1);
                v
            }
            Zp0 => Addr::Mem(self.fetch8(mem) as u16),
            ZpX => Addr::Mem(self.fetch8(mem).wrapping_add(self.x) as u16),
            ZpY => Addr::Mem(self.fetch8(mem).wrapping_add(self.y) as u16),
            Rel => Addr::Rel(self.fetch8(mem) as i8),
            Abs => Addr::Mem(self.fetch16(mem)),
            AbX => {
                let abs = self.fetch16(mem);
                let a = abs.wrapping_add(self.x as u16);

                if pc_add && a & 0xFF < abs & 0xFF {
                    Addr::MemPC(a)
                } else {
                    Addr::Mem(a)
                }
            }
            AbY => {
                let abs = self.fetch16(mem);
                let a = abs.wrapping_add(self.y as u16);

                if pc_add && a & 0xFF < abs & 0xFF {
                    Addr::MemPC(a)
                } else {
                    Addr::Mem(a)
                }
            }
            Ind => {
                let a = self.fetch16(mem);
                let a = self.read_mem16_pw(mem, a);
                Addr::Mem(a)
            }
            IdX => {
                let a = self.fetch8(mem).wrapping_add(self.x);
                let a = self.read_mem16_pw(mem, a as u16);
                Addr::Mem(a)
            }
            IdY => {
                let a = self.fetch8(mem);
                let abs = self.read_mem16_pw(mem, a as u16);
                let a = abs.wrapping_add(self.y as u16);

                if pc_add && a & 0xFF < abs & 0xFF {
                    Addr::MemPC(a)
                } else {
                    Addr::Mem(a)
                }
            }
        }
    }

    fn read8<Mem: CpuMemory>(&mut self, mem: &mut Mem, addr: Addr) -> u8 {
        match addr {
            Addr::A => self.a,
            Addr::Mem(a) => mem.read(self, a),
            Addr::MemPC(a) => {
                self.cyc += 1;
                mem.read(self, a)
            }
            Addr::Rel(_) => unreachable!(),
        }
    }

    fn write8<Mem: CpuMemory>(&mut self, mem: &mut Mem, addr: Addr, v: u8) {
        match addr {
            Addr::A => {
                self.a = v;
            }
            Addr::Mem(a) | Addr::MemPC(a) => mem.write(self, a, v),
            Addr::Rel(_) => unreachable!(),
        }
    }

    pub fn tick<Mem: CpuMemory>(&mut self, mem: &mut Mem) -> u64 {
        self.history_i = (self.history_i + 1) % Self::HISTORY_LEN;
        self.history[self.history_i] = self.pc;

        let start_cycle = self.cyc;

        let opcode = self.fetch8(mem);

        let (op, addr_mode, cycles, pc_add) = OPCODE_MATRIX[opcode as usize];
        let addr = self.fetch_addr(mem, addr_mode, pc_add);

        self.cyc += cycles as u64;

        match op {
            Op::Adc => self.adc(mem, addr),
            Op::And => self.and(mem, addr),
            Op::Asl => self.asl(mem, addr),
            Op::Bcc => self.bcc(mem, addr),
            Op::Bcs => self.bcs(mem, addr),
            Op::Beq => self.beq(mem, addr),
            Op::Bit => self.bit(mem, addr),
            Op::Bmi => self.bmi(mem, addr),
            Op::Bne => self.bne(mem, addr),
            Op::Bpl => self.bpl(mem, addr),
            Op::Brk => self.brk(mem),
            Op::Bvc => self.bvc(mem, addr),
            Op::Bvs => self.bvs(mem, addr),
            Op::Clc => self.clc(mem),
            Op::Cld => self.cld(mem),
            Op::Cli => self.cli(mem),
            Op::Clv => self.clv(mem),
            Op::Cmp => self.cmp(mem, addr),
            Op::Cpx => self.cpx(mem, addr),
            Op::Cpy => self.cpy(mem, addr),
            Op::Dec => self.dec(mem, addr),
            Op::Dex => self.dex(mem),
            Op::Dey => self.dey(mem),
            Op::Eor => self.eor(mem, addr),
            Op::Inc => self.inc(mem, addr),
            Op::Inx => self.inx(mem),
            Op::Iny => self.iny(mem),
            Op::Jmp => self.jmp(mem, addr),
            Op::Jsr => self.jsr(mem, addr),
            Op::Lda => self.lda(mem, addr),
            Op::Ldx => self.ldx(mem, addr),
            Op::Ldy => self.ldy(mem, addr),
            Op::Lsr => self.lsr(mem, addr),
            Op::Nop => self.nop(mem, addr),
            Op::Ora => self.ora(mem, addr),
            Op::Pha => self.pha(mem),
            Op::Php => self.php(mem),
            Op::Pla => self.pla(mem),
            Op::Plp => self.plp(mem),
            Op::Rol => self.rol(mem, addr),
            Op::Ror => self.ror(mem, addr),
            Op::Rti => self.rti(mem),
            Op::Rts => self.rts(mem),
            Op::Sbc => self.sbc(mem, addr),
            Op::Sec => self.sec(mem),
            Op::Sed => self.sed(mem),
            Op::Sei => self.sei(mem),
            Op::Sta => self.sta(mem, addr),
            Op::Stx => self.stx(mem, addr),
            Op::Sty => self.sty(mem, addr),
            Op::Tax => self.tax(mem),
            Op::Tay => self.tay(mem),
            Op::Tsx => self.tsx(mem),
            Op::Txa => self.txa(mem),
            Op::Txs => self.txs(mem),
            Op::Tya => self.tya(mem),

            // Illegal opcodes
            Op::Ahx => self.ahx(mem),
            Op::Alr => self.alr(mem),
            Op::Anc => self.anc(mem),
            Op::Arr => self.arr(mem),
            Op::Axs => self.axs(mem),
            Op::Dcp => self.dcp(mem, addr),
            Op::Kil => self.kil(mem),
            Op::Isc => self.isc(mem, addr),
            Op::Las => self.las(mem),
            Op::Lax => self.lax(mem, addr),
            Op::Rla => self.rla(mem, addr),
            Op::Rra => self.rra(mem, addr),
            Op::Sax => self.sax(mem, addr),
            Op::Shx => self.shx(mem),
            Op::Shy => self.shy(mem),
            Op::Slo => self.slo(mem, addr),
            Op::Sre => self.sre(mem, addr),
            Op::Tas => self.tas(mem),
            Op::Xaa => self.xaa(mem),
        }

        self.cyc - start_cycle
    }

    fn update_negative(&mut self, v: u8) {
        self.p.set(P::NEGATIVE, (v as i8).is_negative());
    }

    fn update_zero(&mut self, v: u8) {
        self.p.set(P::ZERO, v == 0);
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
        self.p.set(P::CARRY, r >= v);
        self.p.set(P::ZERO, r == v);
        self.p.set(P::NEGATIVE, r.wrapping_sub(v) & 0x80 != 0);
    }

    fn push8<Mem: CpuMemory>(&mut self, mem: &mut Mem, v: u8) {
        mem.write(self, 0x0100 + self.sp as u16, v);
        self.sp = self.sp.wrapping_sub(1);
    }

    fn push16<Mem: CpuMemory>(&mut self, mem: &mut Mem, v: u16) {
        let [l, r] = v.to_le_bytes();
        self.push8(mem, r);
        self.push8(mem, l);
    }

    fn pop8<Mem: CpuMemory>(&mut self, mem: &mut Mem) -> u8 {
        self.sp = self.sp.wrapping_add(1);
        mem.read(self, 0x0100 + self.sp as u16)
    }

    fn pop16<Mem: CpuMemory>(&mut self, mem: &mut Mem) -> u16 {
        let l = self.pop8(mem);
        let r = self.pop8(mem);
        u16::from_le_bytes([l, r])
    }

    // Operations

    fn adc<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        let lhs = self.a as u16;
        let rhs = self.read8(mem, a) as u16;
        let carry = self.p.intersects(P::CARRY) as u16;

        let res = lhs + rhs + carry;
        let res8 = res as u8;
        self.a = res8;

        self.update_zero(res8);
        self.update_negative(res8);
        self.p
            .set(P::OVERFLOW, !(lhs ^ rhs) & (lhs ^ res) & 0x80 != 0);
        self.p.set(P::CARRY, res > 0xFF);
    }

    fn and<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        self.a &= self.read8(mem, a);
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn asl<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        let v = self.read8(mem, a);
        let new_v = v << 1;

        self.p.set(P::CARRY, v & 0x80 != 0);
        self.update_zero(new_v);
        self.update_negative(new_v);

        self.write8(mem, a, new_v);
    }

    fn bcc<Mem: CpuMemory>(&mut self, _mem: &mut Mem, a: Addr) {
        self.generic_branch(a, |cpu| !cpu.p.intersects(P::CARRY));
    }

    fn bcs<Mem: CpuMemory>(&mut self, _mem: &mut Mem, a: Addr) {
        self.generic_branch(a, |cpu| cpu.p.intersects(P::CARRY));
    }

    fn beq<Mem: CpuMemory>(&mut self, _mem: &mut Mem, a: Addr) {
        self.generic_branch(a, |cpu| cpu.p.intersects(P::ZERO));
    }

    fn bit<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        let m = self.read8(mem, a);
        let v = self.a & m;
        self.p.set(P::OVERFLOW, m & (1 << 6) != 0);
        self.p.set(P::NEGATIVE, m & (1 << 7) != 0);
        self.p.set(P::ZERO, m & v == 0);
    }

    fn bmi<Mem: CpuMemory>(&mut self, _mem: &mut Mem, a: Addr) {
        self.generic_branch(a, |cpu| cpu.p.intersects(P::NEGATIVE));
    }

    fn bne<Mem: CpuMemory>(&mut self, _mem: &mut Mem, a: Addr) {
        self.generic_branch(a, |cpu| !cpu.p.intersects(P::ZERO));
    }

    fn bpl<Mem: CpuMemory>(&mut self, _mem: &mut Mem, a: Addr) {
        self.generic_branch(a, |cpu| !cpu.p.intersects(P::NEGATIVE));
    }

    fn brk<Mem: CpuMemory>(&mut self, mem: &mut Mem) {
        self.push16(mem, self.pc);
        self.push8(mem, (self.p | P::_5 | P::B).bits());
        self.p |= P::INTERRUPT_DISABLE;
        self.pc = self.read_mem16(mem, 0xFFFE);
    }

    fn bvc<Mem: CpuMemory>(&mut self, _mem: &mut Mem, a: Addr) {
        self.generic_branch(a, |cpu| !cpu.p.intersects(P::OVERFLOW));
    }

    fn bvs<Mem: CpuMemory>(&mut self, _mem: &mut Mem, a: Addr) {
        self.generic_branch(a, |cpu| cpu.p.intersects(P::OVERFLOW));
    }

    fn clc<Mem: CpuMemory>(&mut self, _mem: &mut Mem) {
        self.p -= P::CARRY;
    }

    fn cld<Mem: CpuMemory>(&mut self, _mem: &mut Mem) {
        self.p -= P::DECIMAL;
    }

    fn cli<Mem: CpuMemory>(&mut self, _mem: &mut Mem) {
        self.p -= P::INTERRUPT_DISABLE;
    }

    fn clv<Mem: CpuMemory>(&mut self, _mem: &mut Mem) {
        self.p -= P::OVERFLOW;
    }

    fn cmp<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        let v = self.read8(mem, a);
        self.compare(self.a, v);
    }

    fn cpx<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        let v = self.read8(mem, a);
        self.compare(self.x, v);
    }

    fn cpy<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        let v = self.read8(mem, a);
        self.compare(self.y, v);
    }

    fn dcp<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        let v = self.read8(mem, a).wrapping_sub(1);
        self.write8(mem, a, v);
        self.update_zero(v);
        self.update_negative(v);
        self.compare(self.a, v);
    }

    fn dec<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        let v = self.read8(mem, a).wrapping_sub(1);
        self.write8(mem, a, v);
        self.update_zero(v);
        self.update_negative(v);
    }

    fn dex<Mem: CpuMemory>(&mut self, _mem: &mut Mem) {
        self.x = self.x.wrapping_sub(1);
        self.update_zero(self.x);
        self.update_negative(self.x);
    }

    fn dey<Mem: CpuMemory>(&mut self, _mem: &mut Mem) {
        self.y = self.y.wrapping_sub(1);
        self.update_zero(self.y);
        self.update_negative(self.y);
    }

    fn eor<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        self.a ^= self.read8(mem, a);
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn inc<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        let v = self.read8(mem, a).wrapping_add(1);
        self.write8(mem, a, v);
        self.update_zero(v);
        self.update_negative(v);
    }

    fn inx<Mem: CpuMemory>(&mut self, _mem: &mut Mem) {
        let v = self.x.wrapping_add(1);
        self.x = v;
        self.update_zero(v);
        self.update_negative(v);
    }

    fn iny<Mem: CpuMemory>(&mut self, _mem: &mut Mem) {
        let v = self.y.wrapping_add(1);
        self.y = v;
        self.update_zero(v);
        self.update_negative(v);
    }

    fn isc<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        self.inc(mem, a);
        self.sbc(mem, a);
    }

    fn jmp<Mem: CpuMemory>(&mut self, _mem: &mut Mem, a: Addr) {
        if let Addr::Mem(m) = a {
            self.pc = m;
        } else {
            unreachable!();
        }
    }

    fn jsr<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        if let Addr::Mem(a) = a {
            self.push16(mem, self.pc - 1);
            self.pc = a;
        } else {
            unreachable!();
        }
    }

    fn lax<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        let v = self.read8(mem, a);
        self.a = v;
        self.x = v;

        self.update_zero(v);
        self.update_negative(v);
    }

    fn lda<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        self.a = self.read8(mem, a);
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn ldx<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        self.x = self.read8(mem, a);
        self.update_zero(self.x);
        self.update_negative(self.x);
    }

    fn ldy<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        self.y = self.read8(mem, a);
        self.update_zero(self.y);
        self.update_negative(self.y);
    }

    fn lsr<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        let v = self.read8(mem, a);
        let new_v = v >> 1;

        self.p.set(P::CARRY, v & 1 != 0);
        self.update_zero(new_v);
        self.update_negative(new_v);

        self.write8(mem, a, new_v);
    }

    fn nop<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        self.read8(mem, a);
    }

    fn ora<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        self.a |= self.read8(mem, a);
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn pha<Mem: CpuMemory>(&mut self, mem: &mut Mem) {
        self.push8(mem, self.a);
    }

    fn php<Mem: CpuMemory>(&mut self, mem: &mut Mem) {
        let p = self.p | P::_5 | P::B;
        self.push8(mem, p.bits());
    }

    fn pla<Mem: CpuMemory>(&mut self, mem: &mut Mem) {
        self.a = self.pop8(mem);
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn plp<Mem: CpuMemory>(&mut self, mem: &mut Mem) {
        self.p = (P::from_bits(self.pop8(mem)).unwrap() - P::B) | P::_5;
    }

    fn rla<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        self.rol(mem, a);
        self.and(mem, a);
    }

    fn rol<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        let v = self.read8(mem, a);
        let mut new_v = v << 1;
        if self.p.intersects(P::CARRY) {
            new_v |= 0x01;
        }
        self.p.set(P::CARRY, v & 0x80 != 0);

        self.update_zero(new_v);
        self.update_negative(new_v);

        self.write8(mem, a, new_v);
    }

    fn ror<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        let v = self.read8(mem, a);
        let mut new_v = v >> 1;
        if self.p.intersects(P::CARRY) {
            new_v |= 0x80;
        }
        self.p.set(P::CARRY, v & 1 != 0);

        self.update_zero(new_v);
        self.update_negative(new_v);

        self.write8(mem, a, new_v);
    }

    fn rra<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        self.ror(mem, a);
        self.adc(mem, a);
    }

    fn rti<Mem: CpuMemory>(&mut self, mem: &mut Mem) {
        self.p = P::from_bits(self.pop8(mem)).unwrap() | P::_5;
        self.pc = self.pop16(mem);
    }

    fn rts<Mem: CpuMemory>(&mut self, mem: &mut Mem) {
        self.pc = self.pop16(mem) + 1;
    }

    fn sax<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        let v = self.a & self.x;
        self.write8(mem, a, v);
    }

    fn sbc<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        let lhs = self.a as i16;
        let rhs = self.read8(mem, a) as i16;
        let borrow = !self.p.intersects(P::CARRY) as i16;

        let res = lhs - rhs - borrow;
        let res8 = res as u8;

        self.update_zero(res8);
        self.update_negative(res8);

        self.p.set(P::CARRY, res >= 0);
        self.p
            .set(P::OVERFLOW, (lhs ^ rhs) & (lhs ^ res) & 0x80 != 0);

        self.a = res8;
    }

    fn sec<Mem: CpuMemory>(&mut self, _mem: &mut Mem) {
        self.p |= P::CARRY;
    }

    fn sed<Mem: CpuMemory>(&mut self, _mem: &mut Mem) {
        self.p |= P::DECIMAL;
    }

    fn sei<Mem: CpuMemory>(&mut self, _mem: &mut Mem) {
        self.p |= P::INTERRUPT_DISABLE;
    }

    fn slo<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        self.asl(mem, a);
        self.ora(mem, a);
    }

    fn sre<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        self.lsr(mem, a);
        self.eor(mem, a);
    }

    fn sta<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        self.write8(mem, a, self.a);
    }

    fn stx<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        self.write8(mem, a, self.x);
    }

    fn sty<Mem: CpuMemory>(&mut self, mem: &mut Mem, a: Addr) {
        self.write8(mem, a, self.y);
    }

    fn tax<Mem: CpuMemory>(&mut self, _mem: &mut Mem) {
        self.x = self.a;
        self.update_zero(self.x);
        self.update_negative(self.x);
    }

    fn tay<Mem: CpuMemory>(&mut self, _mem: &mut Mem) {
        self.y = self.a;
        self.update_zero(self.y);
        self.update_negative(self.y);
    }

    fn tsx<Mem: CpuMemory>(&mut self, _mem: &mut Mem) {
        self.x = self.sp;
        self.update_zero(self.x);
        self.update_negative(self.x);
    }

    fn txa<Mem: CpuMemory>(&mut self, _mem: &mut Mem) {
        self.a = self.x;
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn txs<Mem: CpuMemory>(&mut self, _mem: &mut Mem) {
        self.sp = self.x;
    }

    fn tya<Mem: CpuMemory>(&mut self, _mem: &mut Mem) {
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

    pub fn nmi_interrupt<Mem: CpuMemory>(&mut self, mem: &mut Mem) {
        self.push16(mem, self.pc);
        self.push8(mem, self.p.bits());
        self.p |= P::INTERRUPT_DISABLE;
        self.pc = self.read_mem16(mem, 0xfffa);
    }

    fn tas<Mem: CpuMemory>(&self, _mem: &mut Mem) {
        // Illegal opcode not implemented
    }

    fn xaa<Mem: CpuMemory>(&self, _mem: &mut Mem) {
        // Illegal opcode not implemented
    }

    fn shy<Mem: CpuMemory>(&self, _mem: &mut Mem) {
        // Illegal opcode not implemented
    }

    fn shx<Mem: CpuMemory>(&self, _mem: &mut Mem) {
        // Illegal opcode not implemented
    }

    fn las<Mem: CpuMemory>(&self, _mem: &mut Mem) {
        // Illegal opcode not implemented
    }

    fn axs<Mem: CpuMemory>(&self, _mem: &mut Mem) {
        // Illegal opcode not implemented
    }

    fn kil<Mem: CpuMemory>(&self, _mem: &mut Mem) {
        // Illegal opcode not implemented
    }

    fn arr<Mem: CpuMemory>(&self, _mem: &mut Mem) {
        // Illegal opcode not implemented
    }

    fn anc<Mem: CpuMemory>(&self, _mem: &mut Mem) {
        // Illegal opcode not implemented
    }

    fn alr<Mem: CpuMemory>(&self, _mem: &mut Mem) {
        // Illegal opcode not implemented
    }

    fn ahx<Mem: CpuMemory>(&self, _mem: &mut Mem) {
        // Illegal opcode not implemented
    }

    pub fn print_op<Mem: CpuMemory>(&self, mem: &Mem, addr: u16) -> String {
        let Some(opcode) = mem.inspect(self, addr) else {
            return "?".to_string();
        };
        macro_rules! read_8(($base:expr, $offset:expr) => {
            mem.inspect(self, $base.wrapping_add($offset)).map(|v| format!("${:02x}", v)).unwrap_or("$??".to_string())
        });

        macro_rules! read_16(($base:expr, $offset:expr) => {
            self.inspect_mem16(mem, $base.wrapping_add($offset)).map(|v| format!("${:04x}", v)).unwrap_or("$????".to_string())
        });

        let (op, addr_mode, _, _) = OPCODE_MATRIX[opcode as usize];
        let addr_format = match addr_mode {
            AddrMode::Acc => "A {{ACC}}".to_string(),
            AddrMode::Imm => {
                format!("#{} {{IMM}}", read_8!(addr, 1))
            }
            AddrMode::Zp0 => format!("{} {{ZP}}", read_8!(addr, 1)),
            AddrMode::ZpX => {
                format!("{},x {{ZPX}}", read_8!(addr, 1))
            }
            AddrMode::ZpY => {
                format!("{},y {{ZPY}}", read_8!(addr, 1))
            }
            AddrMode::Abs => {
                format!("{} {{ABS}}", read_16!(addr, 1))
            }
            AddrMode::AbX => {
                format!("{},x {{ABX}}", read_16!(addr, 1))
            }
            AddrMode::AbY => {
                format!("{},y {{ABY}}", read_16!(addr, 1))
            }
            AddrMode::Rel => {
                format!("{} {{REL}}", read_8!(addr, 1))
            }
            AddrMode::Ind => {
                format!("{} {{IND}}", read_16!(addr, 1))
            }
            AddrMode::IdX => {
                format!("{},x {{IDX}}", read_16!(addr, 1))
            }
            AddrMode::IdY => {
                format!("{},y {{IDY}}", read_16!(addr, 1))
            }
        };

        format!("${:#04x}: {} {}", addr, op, addr_format)
    }
}
