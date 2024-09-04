use bitflags::bitflags;

bitflags! {
    #[derive(Clone, Copy)]
    pub struct NesController: u8 {
        const RIGHT = 0x1;
        const LEFT = 0x2;
        const DOWN = 0x4;
        const UP = 0x8;
        const START = 0x10;
        const SELECT = 0x20;
        const B = 0x40;
        const A = 0x80;
    }
}
