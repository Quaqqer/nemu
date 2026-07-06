use bitfield_struct::bitfield;

const DUTY_CYCLE_SEQUENCES: [u8; 4] = [0b0100_0000, 0b0110_0000, 0b0011_1100, 0b1001_1111];

#[bitfield(u8)]
struct Reg0 {
    #[bits(2)]
    pub duty: u8,
    envelope_loop: bool,
    constant_volume: bool,
    #[bits(4)]
    volume: u8,
}

#[bitfield(u8)]
struct Reg1 {
    sweep_enabled: bool,
    #[bits(3)]
    period: u8,
    negate: bool,
    #[bits(3)]
    shift: u8,
}

#[bitfield(u8)]
struct Reg2 {
    timer_low: u8,
}

#[bitfield(u8)]
struct Reg3 {
    #[bits(4)]
    length_counter_load: u8,
    #[bits(4)]
    timer_high: u8,
}

struct PulseCh {
    reg0: Reg0,
    reg1: Reg1,
    reg2: Reg2,
    reg3: Reg3,
}

impl PulseCh {}
