use std::fmt::Write;

pub fn fmt_bitflags_u8(
    bits: u8,
    chars: [char; 8],
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    #[allow(clippy::needless_range_loop)]
    for bit_i in 0..8 {
        let c = chars[bit_i];
        let bit = bits >> (7 - bit_i) != 0;

        f.write_char(if bit {
            c.to_uppercase().next().unwrap()
        } else {
            c
        })?;
    }
    Ok(())
}
