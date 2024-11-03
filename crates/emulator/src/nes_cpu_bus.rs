use crate::{
    apu::Apu,
    carts::Cart,
    controller::NesController,
    cpu::{Cpu, CpuMemory},
    ppu::Ppu,
};

pub struct NesCpuBus<'a> {
    pub ram: &'a mut [u8; 0x800],
    pub apu: &'a mut Apu,
    pub ppu: &'a mut Ppu,
    pub cart: &'a mut dyn Cart,
    pub controllers: &'a [NesController; 2],
    pub controller_shifters: &'a mut [u8; 2],
}

impl<'a> CpuMemory for NesCpuBus<'a> {
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
                let v = (self.controller_shifters[1] & 0x80 != 0) as u8;
                self.controller_shifters[1] <<= 1;
                v
            }
            0x4018..=0x401F => {
                unimplemented!("APU and I/O functionality that is normally disabled.")
            }
            0x4020..=0xFFFF => self.cart.cpu_read(addr),
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
            0x4020..=0xFFFF => Some(self.cart.cpu_inspect(addr)),
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
                self.controller_shifters[1] = self.controllers[1].bits();
            }
            0x4018..=0x401F => {
                unimplemented!("APU and I/O functionality that is normally disabled.")
            }
            0x4020..=0xFFFF => {
                self.cart.cpu_write(addr, val);
            }
        }
    }
}
