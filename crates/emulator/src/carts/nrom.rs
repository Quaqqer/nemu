use super::{Cart, MapperTrait, Mirroring};

pub struct NROM {}

impl NROM {
    pub fn new() -> Self {
        Self {}
    }
}

impl MapperTrait for NROM {
    fn cpu_read(&self, cart: &mut Cart, addr: u16) -> u8 {
        self.cpu_inspect(cart, addr)
    }

    fn cpu_inspect(&self, cart: &Cart, addr: u16) -> u8 {
        match addr {
            ..0x6000 => 0,
            0x6000..0x8000 => cart.prg_ram[(addr as usize - 0x6000) % cart.prg_ram.len()],
            0x8000.. => cart.prg_rom[(addr as usize - 0x8000) % cart.prg_rom.len()],
        }
    }

    fn cpu_write(&self, cart: &mut Cart, addr: u16, v: u8) {
        match addr {
            ..0x6000 => {}
            0x6000..0x8000 => {
                let i = (addr as usize - 0x6000) % cart.prg_ram.len();
                cart.prg_ram[i] = v
            }
            0x8000.. => {}
        }
    }

    fn ppu_read(&self, cart: &mut Cart, addr: u16) -> u8 {
        let i = addr as usize % cart.chr_rom.len();
        cart.chr_rom[i]
    }

    fn ppu_write(&self, _cart: &mut Cart, _addr: u16, _v: u8) {}

    fn reset(&self, _cart: &mut Cart) {
        todo!()
    }
}
