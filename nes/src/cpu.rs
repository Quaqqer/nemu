use crate::rom::Rom;

pub struct Cpu {
    a: u8,
    x: u8,
    y: u8,
    pc: u16,
    s: u8,
    p: u8,

    ram: [u8; 0x800],
    ppu_registers: [u8; 0x8],
    apu_registers: [u8; 0x18],

    rom: Rom,
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
            0x4020..=0xFFFF => self.rom.read8(addr - 0x4020),
        }
    }

    fn read_memory_16(&self, addr: u16) -> u16 {
        let l = self.read_memory(addr);
        let r = self.read_memory(addr + 1);
        (r as u16) << 8 + (l as u16)
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

    fn stack_ptr(&self) -> u16 {
        self.s as u16 + 0x0100
    }

    pub fn reset(&mut self) {
        self.s = self.s.wrapping_sub(3);
        self.p |= FLAG_INTERRUPT_DISABLE;
        self.apu_registers[0x15] = 0x00;
        // TODO: APU triangle phase is reset to 0
        // TODO: APU DPCM output ANDed with 1 (upper 6 bits cleared)
    }

    fn new(rom: Rom) -> Self {
        // TODO: All 15 bits of noise channel LSFR = $0000
        // TODO: APU Frame Counter

        Self {
            a: 0,
            x: 0,
            y: 0,
            pc: 0,
            s: 0xFD,
            p: 0x34,

            ram: [0x00; 0x800],
            ppu_registers: [0x00; 0x8],
            apu_registers: [0x00; 0x18],

            rom,
        }
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

    fn addr_absolute(&mut self) -> u16 {
        self.fetch16()
    }

    fn addr_indirect(&mut self) -> u16 {
        let m = self.fetch16();
        self.read_memory_16(m)
    }

    fn cycle(&mut self) {
        let opcode = self.fetch8();

        match opcode {
            0x69 => {
                let m = self.fetch8();
                self.op_adc(m);
                panic!()
            }

            0x4c => {
                let m = self.addr_absolute();
                // TODO: Fix NB
                self.pc = m;
            }

            0x6c => {
                let m = self.addr_indirect();
                // TODO: Fix NB
                self.pc = m;
            }
            _ => todo!(),
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

    fn update_negative(&mut self) {
        self.set_flag(FLAG_NEGATIVE, (self.a as i8).is_negative());
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
}
