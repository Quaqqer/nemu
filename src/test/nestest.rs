#[cfg(test)]
mod tests {
    use crate::controller::NesController;
    use crate::{apu::Apu, cpu::CpuBus, ppu::Ppu};
    use crate::{cart::Cart, cpu::Cpu};

    #[derive(PartialEq)]
    struct LogEntry {
        pc: u16,
        a: u8,
        x: u8,
        y: u8,
        p: u8,
        sp: u8,
        cyc: u64,
    }

    impl std::fmt::Debug for LogEntry {
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

    fn parse_log(lines: &[String]) -> Vec<LogEntry> {
        lines
            .iter()
            .map(|line| LogEntry {
                pc: u16::from_str_radix(&line[0..4], 16).unwrap(),
                a: u8::from_str_radix(&line[50..52], 16).unwrap(),
                x: u8::from_str_radix(&line[55..57], 16).unwrap(),
                y: u8::from_str_radix(&line[60..62], 16).unwrap(),
                p: u8::from_str_radix(&line[65..67], 16).unwrap(),
                sp: u8::from_str_radix(&line[71..73], 16).unwrap(),
                cyc: line[90..].parse().unwrap(),
            })
            .collect()
    }

    #[test]
    fn run_nestest() {
        let log_lines = std::fs::read_to_string("roms/nestest/nestest.log")
            .unwrap()
            .lines()
            .map(|s| s.to_string())
            .collect::<Vec<String>>();

        let cart = &mut Cart::read_ines1_0("roms/nestest/nestest.nes");
        let mut cpu = Cpu::new();
        let cpu_bus = &mut CpuBus {
            apu: &mut Apu::new(),
            ppu: &mut Ppu::new(),
            cart,
            controllers: &[NesController::empty(); 2],
            controller_shifters: &mut [0x0; 2],
        };

        cpu.pc = 0xC000;

        for (i, expected) in parse_log(&log_lines).iter().enumerate() {
            let got = &LogEntry {
                pc: cpu.pc,
                a: cpu.a,
                x: cpu.x,
                y: cpu.y,
                p: cpu.p.bits(),
                sp: cpu.sp,
                cyc: cpu.cyc,
            };

            if expected != got {
                let lines = ((i as i32 - 27).max(0)..(i as i32 + 3).min(log_lines.len() as i32))
                    .map(|i| (i, &log_lines[i as usize]))
                    .map(|(j, line)| {
                        if j as usize == i {
                            "-> ".to_string() + line
                        } else {
                            "   ".to_string() + line
                        }
                    })
                    .collect::<Vec<_>>()
                    .join("\n");

                panic!(
                "\nUnexpected cpu state on line {}:\n\tExpected: {:?}\n\tGot:      {:?}\n\nFull cpu state: {:?}\n\nLines:\n{}\n",
                i + 1,
                expected,
                got,
                cpu,
                lines,
            );
            }

            cpu.tick(cpu_bus);
        }
    }
}
