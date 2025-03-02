use crate::{carts::Cart, ppu::Ppu};

pub fn get_sprite(cart: &dyn Cart, pattern_table: usize, tile_x: usize, tile_y: usize) -> [u8; 16] {
    debug_assert!(tile_x < 16);
    debug_assert!(tile_y < 16);

    get_sprite_i(cart, pattern_table, (tile_y << 4) | tile_x)
}

pub fn get_sprite_i(cart: &dyn Cart, pattern_table: usize, tile_i: usize) -> [u8; 16] {
    debug_assert!(pattern_table < 2);
    let addr = (pattern_table << 10) | (tile_i << 4);

    let mut buf = [0; 16];

    #[allow(clippy::needless_range_loop)]
    for i in 0..16 {
        buf[i] = cart.ppu_inspect(addr as u16 + i as u16);
    }

    buf
}

pub fn get_sprite_px(sprite: &[u8; 16], x: usize, y: usize) -> u8 {
    debug_assert!(x < 8);
    debug_assert!(y < 8);

    let lo_row = sprite[y];
    let hi_row = sprite[0x8 | y];

    let lo_px = (lo_row >> (7 - x)) & 1;
    let hi_px = (hi_row >> (7 - x)) & 1;

    (hi_px << 1) | lo_px
}

pub fn get_nametable_entry(ppu: &Ppu, cart: &dyn Cart, nametable: usize, x: usize, y: usize) -> u8 {
    debug_assert!(nametable < 4);
    debug_assert!(x < 32);
    debug_assert!(y < 30);

    let addr = (nametable << 10) | (y << 5) | x;

    ppu.inspect_mem(cart, 0x2000 + addr as u16)
}

pub fn get_attribute_table_entry(_ppu: &Ppu, _nametable: usize, _x: usize, _y: usize) -> u8 {
    todo!()
}
