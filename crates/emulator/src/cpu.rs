use crate::apu::Apu;
use crate::cart::Cart;
use crate::controller::NesController;
use crate::op::{AddrMode, Op, OPCODE_MATRIX};
use crate::ppu::Ppu;
use bitflags::bitflags;

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

pub struct CpuBus<'a> {
    pub ram: &'a mut [u8; 0x800],
    pub apu: &'a mut Apu,
    pub ppu: &'a mut Ppu,
    pub cart: &'a mut Cart,
    pub controllers: &'a [NesController; 2],
    pub controller_shifters: &'a mut [u8; 2],
}

impl<'a> CpuMemory for CpuBus<'a> {
    fn read(&mut self, _cpu: &mut Cpu, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1FFF => self.ram[addr as usize % 0x800],
            0x2000..=0x3FFF => self.ppu.cpu_read_register(self.cart, addr),
            0x4000..=0x4015 => self.apu.read_register(addr),
            0x4016 => {
                let v = (self.controller_shifters[0] & 0x80 != 0) as u8;
                self.controller_shifters[0] <<= 1;
                v
            }
            0x4017 => {
                let v = (self.controller_shifters[0] & 0x80 != 0) as u8;
                self.controller_shifters[0] <<= 1;
                v
            }
            0x4018..=0x401F => {
                unimplemented!("APU and I/O functionality that is normally disabled.")
            }
            0x4020..=0xFFFF => self.cart.read8(addr),
        }
    }

    fn inspect(&self, _cpu: &Cpu, addr: u16) -> Option<u8> {
        match addr {
            0x0000..=0x1FFF => self.ram.get(addr as usize % 0x800).copied(),
            0x2000..=0x3FFF => None,
            0x4000..=0x4017 => None,
            0x4018..=0x401F => {
                unimplemented!("APU and I/O functionality that is normally disabled.")
            }
            0x4020..=0xFFFF => Some(self.cart.read8(addr)),
        }
    }

    fn write(&mut self, cpu: &mut Cpu, addr: u16, val: u8) {
        match addr {
            0x0000..=0x1FFF => {
                self.ram[addr as usize % 0x800] = val;
            }
            0x2000..=0x3FFF => {
                self.ppu.cpu_write_register(self.cart, addr, val);
            }
            0x4014 => {
                // Perform OAM DMA

                // Either 513 or 514 depending if on a put or write cycle, hard to implement
                cpu.cyc += 514;
                let mut mem_i = (val as u16) << 8;
                for i in 0..256 {
                    self.ppu.oam[i] = self.read(cpu, mem_i);
                    mem_i += 1;
                }
            }
            0x4000..=0x4015 => {
                self.apu.write_register(addr, val);
            }
            0x4016 => {
                self.controller_shifters[0] = self.controllers[0].bits();
            }
            0x4017 => {
                self.controller_shifters[0] = self.controllers[0].bits();
            }
            0x4018..=0x401F => {
                unimplemented!("APU and I/O functionality that is normally disabled.")
            }
            0x4020..=0xFFFF => {
                self.cart.write8(addr - 0x4020, val);
            }
        }
    }
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

    pub(crate) fn init(&mut self, bus: &mut CpuBus) {
        let start_addr = self.read_mem16(bus, 0xFFFC);
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

    pub fn tick(&mut self, bus: &mut CpuBus) -> u64 {
        self.history_i = (self.history_i + 1) % Self::HISTORY_LEN;
        self.history[self.history_i] = self.pc;

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

    fn adc(&mut self, bus: &mut CpuBus, a: Addr) {
        let lhs = self.a as u16;
        let rhs = self.read8(bus, a) as u16;
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

    fn and(&mut self, bus: &mut CpuBus, a: Addr) {
        self.a &= self.read8(bus, a);
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn asl(&mut self, bus: &mut CpuBus, a: Addr) {
        let v = self.read8(bus, a);
        let new_v = v << 1;

        self.p.set(P::CARRY, v & 0x80 != 0);
        self.update_zero(new_v);
        self.update_negative(new_v);

        self.write8(bus, a, new_v);
    }

    fn bcc(&mut self, _bus: &mut CpuBus, a: Addr) {
        self.generic_branch(a, |cpu| !cpu.p.intersects(P::CARRY));
    }

    fn bcs(&mut self, _bus: &mut CpuBus, a: Addr) {
        self.generic_branch(a, |cpu| cpu.p.intersects(P::CARRY));
    }

    fn beq(&mut self, _bus: &mut CpuBus, a: Addr) {
        self.generic_branch(a, |cpu| cpu.p.intersects(P::ZERO));
    }

    fn bit(&mut self, bus: &mut CpuBus, a: Addr) {
        let m = self.read8(bus, a);
        let v = self.a & m;
        self.p.set(P::OVERFLOW, m & (1 << 6) != 0);
        self.p.set(P::NEGATIVE, m & (1 << 7) != 0);
        self.p.set(P::ZERO, m & v == 0);
    }

    fn bmi(&mut self, _bus: &mut CpuBus, a: Addr) {
        self.generic_branch(a, |cpu| cpu.p.intersects(P::NEGATIVE));
    }

    fn bne(&mut self, _bus: &mut CpuBus, a: Addr) {
        self.generic_branch(a, |cpu| !cpu.p.intersects(P::ZERO));
    }

    fn bpl(&mut self, _bus: &mut CpuBus, a: Addr) {
        self.generic_branch(a, |cpu| !cpu.p.intersects(P::NEGATIVE));
    }

    fn brk(&mut self, bus: &mut CpuBus) {
        self.push16(bus, self.pc);
        self.push8(bus, (self.p | P::_5 | P::B).bits());
        self.p |= P::INTERRUPT_DISABLE;
        self.pc = self.read_mem16(bus, 0xFFFE);
    }

    fn bvc(&mut self, _bus: &mut CpuBus, a: Addr) {
        self.generic_branch(a, |cpu| !cpu.p.intersects(P::OVERFLOW));
    }

    fn bvs(&mut self, _bus: &mut CpuBus, a: Addr) {
        self.generic_branch(a, |cpu| cpu.p.intersects(P::OVERFLOW));
    }

    fn clc(&mut self, _bus: &mut CpuBus) {
        self.p -= P::CARRY;
    }

    fn cld(&mut self, _bus: &mut CpuBus) {
        self.p -= P::DECIMAL;
    }

    fn cli(&mut self, _bus: &mut CpuBus) {
        self.p -= P::INTERRUPT_DISABLE;
    }

    fn clv(&mut self, _bus: &mut CpuBus) {
        self.p -= P::OVERFLOW;
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

    fn dex(&mut self, _bus: &mut CpuBus) {
        self.x = self.x.wrapping_sub(1);
        self.update_zero(self.x);
        self.update_negative(self.x);
    }

    fn dey(&mut self, _bus: &mut CpuBus) {
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

    fn inx(&mut self, _bus: &mut CpuBus) {
        let v = self.x.wrapping_add(1);
        self.x = v;
        self.update_zero(v);
        self.update_negative(v);
    }

    fn iny(&mut self, _bus: &mut CpuBus) {
        let v = self.y.wrapping_add(1);
        self.y = v;
        self.update_zero(v);
        self.update_negative(v);
    }

    fn isc(&mut self, bus: &mut CpuBus, a: Addr) {
        self.inc(bus, a);
        self.sbc(bus, a);
    }

    fn jmp(&mut self, _bus: &mut CpuBus, a: Addr) {
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

        self.p.set(P::CARRY, v & 1 != 0);
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
        let p = self.p | P::_5 | P::B;
        self.push8(bus, p.bits());
    }

    fn pla(&mut self, bus: &mut CpuBus) {
        self.a = self.pop8(bus);
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn plp(&mut self, bus: &mut CpuBus) {
        self.p = (P::from_bits(self.pop8(bus)).unwrap() - P::B) | P::_5;
    }

    fn rla(&mut self, bus: &mut CpuBus, a: Addr) {
        self.rol(bus, a);
        self.and(bus, a);
    }

    fn rol(&mut self, bus: &mut CpuBus, a: Addr) {
        let v = self.read8(bus, a);
        let mut new_v = v << 1;
        if self.p.intersects(P::CARRY) {
            new_v |= 0x01;
        }
        self.p.set(P::CARRY, v & 0x80 != 0);

        self.update_zero(new_v);
        self.update_negative(new_v);

        self.write8(bus, a, new_v);
    }

    fn ror(&mut self, bus: &mut CpuBus, a: Addr) {
        let v = self.read8(bus, a);
        let mut new_v = v >> 1;
        if self.p.intersects(P::CARRY) {
            new_v |= 0x80;
        }
        self.p.set(P::CARRY, v & 1 != 0);

        self.update_zero(new_v);
        self.update_negative(new_v);

        self.write8(bus, a, new_v);
    }

    fn rra(&mut self, bus: &mut CpuBus, a: Addr) {
        self.ror(bus, a);
        self.adc(bus, a);
    }

    fn rti(&mut self, bus: &mut CpuBus) {
        self.p = P::from_bits(self.pop8(bus)).unwrap() | P::_5;
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

    fn sec(&mut self, _bus: &mut CpuBus) {
        self.p |= P::CARRY;
    }

    fn sed(&mut self, _bus: &mut CpuBus) {
        self.p |= P::DECIMAL;
    }

    fn sei(&mut self, _bus: &mut CpuBus) {
        self.p |= P::INTERRUPT_DISABLE;
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

    fn tax(&mut self, _bus: &mut CpuBus) {
        self.x = self.a;
        self.update_zero(self.x);
        self.update_negative(self.x);
    }

    fn tay(&mut self, _bus: &mut CpuBus) {
        self.y = self.a;
        self.update_zero(self.y);
        self.update_negative(self.y);
    }

    fn tsx(&mut self, _bus: &mut CpuBus) {
        self.x = self.sp;
        self.update_zero(self.x);
        self.update_negative(self.x);
    }

    fn txa(&mut self, _bus: &mut CpuBus) {
        self.a = self.x;
        self.update_zero(self.a);
        self.update_negative(self.a);
    }

    fn txs(&mut self, _bus: &mut CpuBus) {
        self.sp = self.x;
    }

    fn tya(&mut self, _bus: &mut CpuBus) {
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

    pub fn nmi_interrupt(&mut self, bus: &mut CpuBus) {
        self.push16(bus, self.pc);
        self.push8(bus, self.p.bits());
        self.p |= P::INTERRUPT_DISABLE;
        self.pc = self.read_mem16(bus, 0xfffa);
    }

    fn tas(&self, _bus: &mut CpuBus) {
        // Illegal opcode not implemented
    }

    fn xaa(&self, _bus: &mut CpuBus) {
        // Illegal opcode not implemented
    }

    fn shy(&self, _bus: &mut CpuBus) {
        // Illegal opcode not implemented
    }

    fn shx(&self, _bus: &mut CpuBus) {
        // Illegal opcode not implemented
    }

    fn las(&self, _bus: &mut CpuBus) {
        // Illegal opcode not implemented
    }

    fn axs(&self, _bus: &mut CpuBus) {
        // Illegal opcode not implemented
    }

    fn kil(&self, _bus: &mut CpuBus) {
        // Illegal opcode not implemented
    }

    fn arr(&self, _bus: &mut CpuBus) {
        // Illegal opcode not implemented
    }

    fn anc(&self, _bus: &mut CpuBus) {
        // Illegal opcode not implemented
    }

    fn alr(&self, _bus: &mut CpuBus) {
        // Illegal opcode not implemented
    }

    fn ahx(&self, _bus: &mut CpuBus) {
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
