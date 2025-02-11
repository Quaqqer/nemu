use std::{collections::BTreeMap, path::PathBuf};

use serde::Deserialize;

use crate::cpu::{self, op::Op, Cpu, CpuMemory};

#[derive(serde::Deserialize)]
struct CpuState {
    pc: u16,
    s: u8,
    a: u8,
    x: u8,
    y: u8,
    p: u8,
    ram: Vec<(u16, u8)>,
}

#[derive(Deserialize)]
struct SingleStepTest {
    name: String,
    initial: CpuState,
    #[serde(rename = "final")]
    final_: CpuState,
    #[allow(dead_code)]
    cycles: Vec<(u16, u8, String)>,
}

#[derive(Deserialize)]
struct SingleStepTestFile(Vec<SingleStepTest>);

#[derive(Deserialize)]
struct SingleStepMem {
    ram: BTreeMap<u16, u8>,
}

impl CpuMemory for SingleStepMem {
    fn read(&mut self, _cpu: &mut Cpu, addr: u16) -> u8 {
        self.ram.get(&addr).copied().unwrap_or(0)
    }

    fn inspect(&self, _cpu: &Cpu, addr: u16) -> Option<u8> {
        Some(self.ram.get(&addr).copied().unwrap_or(0))
    }

    fn write(&mut self, _cpu: &mut Cpu, addr: u16, v: u8) {
        self.ram.insert(addr, v);
    }
}

fn run_test(sst: SingleStepTest) {
    let initial = sst.initial;
    let mut cpu = Cpu {
        a: initial.a,
        x: initial.x,
        y: initial.y,
        pc: initial.pc,
        sp: initial.s,
        p: cpu::P::from_bits(initial.p).unwrap(),
        cyc: 0,
        history: [0x00; Cpu::HISTORY_LEN],
        history_i: 0,
    };
    let mut ram = BTreeMap::new();
    for (k, v) in initial.ram {
        ram.insert(k, v);
    }
    let mut mem = SingleStepMem { ram };

    let (op, _, _, _) = cpu::op::OPCODE_MATRIX[mem.inspect(&cpu, cpu.pc).unwrap() as usize];

    match op {
        Op::Ahx
        | Op::Alr
        | Op::Anc
        | Op::Arr
        | Op::Axs
        | Op::Dcp
        | Op::Kil
        | Op::Isc
        | Op::Las
        | Op::Lax
        | Op::Rla
        | Op::Rra
        | Op::Sax
        | Op::Shx
        | Op::Shy
        | Op::Slo
        | Op::Sre
        | Op::Tas
        | Op::Xaa => {
            println!("Illegal opcode, skipping test {}", sst.name);
            return;
        }
        _ => {}
    };

    // Run the instruction
    let op_str = cpu.print_op(&mem, cpu.pc);
    cpu.tick(&mut mem);

    let final_ = sst.final_;
    let final_p = cpu::P::from_bits(final_.p).unwrap();
    let mut final_ram = BTreeMap::new();
    for (k, v) in final_.ram {
        final_ram.insert(k, v);
    }

    let cpu_state_differ = cpu.a != final_.a
        || cpu.x != final_.x
        || cpu.y != final_.y
        || cpu.p != final_p
        || cpu.pc != final_.pc
        || cpu.sp != final_.s;
    let mem_differ = mem.ram != final_ram;
    let fail = cpu_state_differ || mem_differ;

    if fail {
        let fail_line = std::format!("Failed test {} with op {}", sst.name, op_str);
        eprintln!("{}", fail_line);
        eprintln!("{}", "=".repeat(fail_line.len()));
        eprintln!();

        if cpu_state_differ {
            eprintln!("Cpu state differs:");
            eprintln!(
                "Got:      a: {:#02x}, x: {:#02x}, y: {:#02x}, pc: {:#04x}, s: {:#04x}, p: {}",
                cpu.a, cpu.x, cpu.y, cpu.pc, cpu.sp, cpu.p
            );
            eprintln!(
                "Expected: a: {:#02x}, x: {:#02x}, y: {:#02x}, pc: {:#04x}, s: {:#04x}, p: {}",
                final_.a,
                final_.x,
                final_.y,
                final_.pc,
                final_.s,
                cpu::P::from_bits(final_.p).unwrap()
            );
        }
        eprintln!();

        if mem_differ {
            eprintln!("Memory differs");
            eprintln!("Got:      {:?}", mem.ram);
            eprintln!("Expected: {:?}", final_ram);
        }
        eprintln!();
        panic!();
    }

    // TODO: Check cycle accuracy
}

#[test]
fn single_step_tests() {
    let mut path = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    path.push("res/single_step_tests/nes6502/v1");

    let test_files = std::fs::read_dir(path).unwrap();
    let test_files = test_files.map(Result::unwrap);

    for test_file in test_files {
        println!(
            "Running test file {}",
            test_file.file_name().to_str().unwrap()
        );

        let content = std::fs::read_to_string(test_file.path()).unwrap();
        let data = serde_json::from_str::<SingleStepTestFile>(&content).unwrap();

        for sst in data.0 {
            run_test(sst);
        }
    }
}
