use crate::{cpu::Cpu, rom::Rom};

#[derive(Debug)]
struct LogLine {
    op: String,
    pc: u16,
    a: u8,
    x: u8,
    y: u8,
    p: u8,
    s: u8,
    cyc: u32,
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
            s: u8::from_str_radix(&line[71..73], 16).unwrap(),
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

    let mut cycles = 0;

    for line in &log {
        assert_eq!(cpu.pc, line.pc);

        cycles += cpu.cycle();

        assert_eq!(cpu.pc, line.pc);
        assert_eq!(cpu.a, line.a);
        assert_eq!(cpu.x, line.x);
        assert_eq!(cpu.y, line.y);
        assert_eq!(cpu.p, line.p);
        assert_eq!(cpu.s, line.s);
        assert_eq!(cycles, line.cyc);
    }

    println!("{:?}", log);

    panic!()
}
