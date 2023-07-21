use crate::{cpu::Cpu, rom::Rom};

#[derive(PartialEq)]
struct LogLine {
    op: String,
    pc: u16,
    a: u8,
    x: u8,
    y: u8,
    p: u8,
    sp: u8,
    cyc: u32,
}

impl std::fmt::Debug for LogLine {
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

fn parse_log(log: &str) -> Vec<LogLine> {
    log.lines()
        .map(|line| LogLine {
            op: line[16..48].trim_end().to_string(),
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
    let log = parse_log(
        std::fs::read_to_string("../roms/nestest/nestest.log")
            .unwrap()
            .as_str(),
    );

    let mut cpu = Cpu::new(Rom::read_ines1_0("../roms/nestest/nestest.nes"));
    cpu.pc = 0xC000;

    let mut cycles = 7;

    for (i, line) in log.iter().enumerate() {
        let created_line = &LogLine {
            op: line.op.clone(),
            pc: cpu.pc,
            a: cpu.a,
            x: cpu.x,
            y: cpu.y,
            p: cpu.p,
            sp: cpu.sp,
            cyc: cycles,
        };

        cycles += cpu.cycle();

        assert_eq!(line, created_line, "Line {}", i + 1);
    }

    println!("{:?}", log);

    panic!()
}
