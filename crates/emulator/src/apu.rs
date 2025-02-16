#[derive(Clone, bincode::Encode, bincode::Decode)]
pub struct Apu {
    registers: [u8; 0x18],
}

impl Apu {
    pub fn new() -> Self {
        Apu {
            registers: [0x00; 0x18],
        }
    }

    pub fn reset(&mut self) {
        self.registers[0x15] = 0x00;
        // TODO: APU triangle phase is reset to 0
        // TODO: APU DPCM output ANDed with 1 (upper 6 bits cleared)
        // TODO: All 15 bits of noise channel LSFR = $0000
        // TODO: APU Frame Counter
    }

    pub fn write_register(&mut self, addr: u16, v: u8) {
        self.registers[addr as usize - 0x4000] = v;
    }

    pub fn read_register(&self, addr: u16) -> u8 {
        self.registers[addr as usize - 0x4000]
    }
}
