use crate::op::{AddrMode, Op, OPCODE_MATRIX};
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

    /// Read 16 bytes from memory, with page wrapping.
    ///
    /// If the adress is at the end of a page the second byte wraps around to the start of the same
    /// page.
    ///
    /// * `addr`: The memory address
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

    fn fetch_addr(&mut self, m: AddrMode, pc_add: bool) -> Addr {
        use AddrMode::*;
        match m {
            Acc => Addr::A,
            Imm => {
                let v = Addr::Mem(self.pc);
                self.pc = self.pc.wrapping_add(1);
                v
            }
            Zp0 => Addr::Mem(self.fetch8() as u16),
            ZpX => Addr::Mem(self.fetch8().wrapping_add(self.x) as u16),
            ZpY => Addr::Mem(self.fetch8().wrapping_add(self.y) as u16),
            Rel => Addr::Rel(self.fetch8() as i8),
            Abs => Addr::Mem(self.fetch16()),
            AbX => {
                let abs = self.fetch16();
                let a = abs.wrapping_add(self.x as u16);

                if pc_add && a & 0xFF < abs & 0xFF {
                    Addr::MemPC(a)
                } else {
                    Addr::Mem(a)
                }
            }
            AbY => {
                let abs = self.fetch16();
                let a = abs.wrapping_add(self.y as u16);

                if pc_add && a & 0xFF < abs & 0xFF {
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
            IdX => {
                let a = self.fetch8().wrapping_add(self.x);
                let a = self.read_mem16_pw(a as u16);
                Addr::Mem(a)
            }
            IdY => {
                let a = self.fetch8();
                let abs = self.read_mem16_pw(a as u16);
                let a = abs.wrapping_add(self.y as u16);

                if pc_add && a & 0xFF < abs & 0xFF {
                    Addr::MemPC(a)
                } else {
                    Addr::Mem(a)
                }
            }
        }
    }

    fn read8(&mut self, addr: Addr) -> u8 {
        match addr {
            Addr::A => self.a,
            Addr::Mem(a) => self.read_mem8(a),
            Addr::MemPC(a) => {
                self.cyc += 1;
                self.read_mem8(a)
            }
            Addr::Rel(_) | Addr::None => unreachable!(),
        }
    }

    fn write8(&mut self, addr: Addr, v: u8) {
        match addr {
            Addr::A => {
                self.a = v;
            }
            Addr::Mem(a) | Addr::MemPC(a) => self.write_mem8(a, v),
            Addr::None | Addr::Rel(_) => unreachable!(),
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
        let start_cycle = self.cyc;

        let opcode = self.fetch8();

        let (op, addr_mode, cycles, pc_add) = OPCODE_MATRIX[opcode as usize];
        let addr = self.fetch_addr(addr_mode, pc_add);

        self.cyc += cycles as u64;

        match op {
            Op::Adc => self.adc(addr),
            Op::And => self.and(addr),
            Op::Asl => self.asl(addr),
            Op::Bcc => self.bcc(addr),
            Op::Bcs => self.bcs(addr),
            Op::Beq => self.beq(addr),
            Op::Bit => self.bit(addr),
            Op::Bmi => self.bmi(addr),
            Op::Bne => self.bne(addr),
            Op::Bpl => self.bpl(addr),
            Op::Brk => self.brk(),
            Op::Bvc => self.bvc(addr),
            Op::Bvs => self.bvs(addr),
            Op::Clc => self.clc(),
            Op::Cld => self.cld(),
            Op::Cli => self.cli(),
            Op::Clv => self.clv(),
            Op::Cmp => self.cmp(addr),
            Op::Cpx => self.cpx(addr),
            Op::Cpy => self.cpy(addr),
            Op::Dec => self.dec(addr),
            Op::Dex => self.dex(),
            Op::Dey => self.dey(),
            Op::Eor => self.eor(addr),
            Op::Inc => self.inc(addr),
            Op::Inx => self.inx(),
            Op::Iny => self.iny(),
            Op::Jmp => self.jmp(addr),
            Op::Jsr => self.jsr(addr),
            Op::Lda => self.lda(addr),
            Op::Ldx => self.ldx(addr),
            Op::Ldy => self.ldy(addr),
            Op::Lsr => self.lsr(addr),
            Op::Nop => self.nop(addr),
            Op::Ora => self.ora(addr),
            Op::Pha => self.pha(),
            Op::Php => self.php(),
            Op::Pla => self.pla(),
            Op::Plp => self.plp(),
            Op::Rol => self.rol(addr),
            Op::Ror => self.ror(addr),
            Op::Rti => self.rti(),
            Op::Rts => self.rts(),
            Op::Sbc => self.sbc(addr),
            Op::Sec => self.sec(),
            Op::Sed => self.sed(),
            Op::Sei => self.sei(),
            Op::Sta => self.sta(addr),
            Op::Stx => self.stx(addr),
            Op::Sty => self.sty(addr),
            Op::Tax => self.tax(),
            Op::Tay => self.tay(),
            Op::Tsx => self.tsx(),
            Op::Txa => self.txa(),
            Op::Txs => self.txs(),
            Op::Tya => self.tya(),

            // Illegal opcodes
            Op::Ahx => self.ahx(),
            Op::Alr => self.alr(),
            Op::Anc => self.anc(),
            Op::Arr => self.arr(),
            Op::Axs => self.axs(),
            Op::Dcp => self.dcp(addr),
            Op::Kil => self.kil(),
            Op::Isc => self.isc(addr),
            Op::Las => self.las(),
            Op::Lax => self.lax(addr),
            Op::Rla => self.rla(addr),
            Op::Rra => self.rra(addr),
            Op::Sax => self.sax(addr),
            Op::Shx => self.shx(),
            Op::Shy => self.shy(),
            Op::Slo => self.slo(addr),
            Op::Sre => self.sre(addr),
            Op::Tas => self.tas(),
            Op::Xaa => self.xaa(),
        }

        for _ in 0..self.cyc - start_cycle {
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
        let v = self.read8(a).wrapping_sub(1);
        self.write8(a, v);
        self.update_zero(v);
        self.update_negative(v);
        self.compare(self.a, v);
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

    fn nop(&mut self, a: Addr) {
        self.read8(a);
    }

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

    #[allow(unused)]
    fn nmi_interrupt(&mut self) {
        self.push16(self.pc);
        let flags = self.p | FLAG_B;
        self.push8(flags);
        self.set_flag(FLAG_INTERRUPT_DISABLE, true);
        self.pc = self.read_mem16(0xfffa);
        // TODO: Might want to do two ppu cycles here?
    }

    fn tas(&self) {
        // Illegal opcode not implemented
    }

    fn xaa(&self) {
        // Illegal opcode not implemented
    }

    fn shy(&self) {
        // Illegal opcode not implemented
    }

    fn shx(&self) {
        // Illegal opcode not implemented
    }

    fn las(&self) {
        // Illegal opcode not implemented
    }

    fn axs(&self) {
        // Illegal opcode not implemented
    }

    fn kil(&self) {
        // Illegal opcode not implemented
    }

    fn arr(&self) {
        // Illegal opcode not implemented
    }

    fn anc(&self) {
        // Illegal opcode not implemented
    }

    fn alr(&self) {
        // Illegal opcode not implemented
    }

    fn ahx(&self) {
        // Illegal opcode not implemented
    }
}
