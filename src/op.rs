#[derive(Debug, Clone, Copy)]
pub enum Op {
    Adc,
    And,
    Asl,
    Bcc,
    Bcs,
    Beq,
    Bit,
    Bmi,
    Bne,
    Bpl,
    Brk,
    Bvc,
    Bvs,
    Clc,
    Cld,
    Cli,
    Clv,
    Cmp,
    Cpx,
    Cpy,
    Dec,
    Dex,
    Dey,
    Eor,
    Inc,
    Inx,
    Iny,
    Jmp,
    Jsr,
    Lda,
    Ldx,
    Ldy,
    Lsr,
    Nop,
    Ora,
    Pha,
    Php,
    Pla,
    Plp,
    Rol,
    Ror,
    Rti,
    Rts,
    Sbc,
    Sec,
    Sed,
    Sei,
    Sta,
    Stx,
    Sty,
    Tax,
    Tay,
    Tsx,
    Txa,
    Txs,
    Tya,
    // Illegal opcodes
    Ahx,
    Alr,
    Anc,
    Arr,
    Axs,
    Dcp,
    Kil,
    Isc,
    Las,
    Lax,
    Rla,
    Rra,
    Sax,
    Shx,
    Shy,
    Slo,
    Sre,
    Tas,
    Xaa,
}

impl std::fmt::Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Op::Adc => "ADC",
                Op::And => "AND",
                Op::Asl => "ASL",
                Op::Bcc => "BCC",
                Op::Bcs => "BCS",
                Op::Beq => "BEQ",
                Op::Bit => "BIT",
                Op::Bmi => "BMI",
                Op::Bne => "BNE",
                Op::Bpl => "BPL",
                Op::Brk => "BRK",
                Op::Bvc => "BVC",
                Op::Bvs => "BVS",
                Op::Clc => "CLC",
                Op::Cld => "CLD",
                Op::Cli => "CLI",
                Op::Clv => "CLV",
                Op::Cmp => "CMP",
                Op::Cpx => "CPX",
                Op::Cpy => "CPY",
                Op::Dec => "DEC",
                Op::Dex => "DEX",
                Op::Dey => "DEY",
                Op::Eor => "EOR",
                Op::Inc => "INC",
                Op::Inx => "INX",
                Op::Iny => "INY",
                Op::Jmp => "JMP",
                Op::Jsr => "JSR",
                Op::Lda => "LDA",
                Op::Ldx => "LDX",
                Op::Ldy => "LDY",
                Op::Lsr => "LSR",
                Op::Nop => "NOP",
                Op::Ora => "ORA",
                Op::Pha => "PHA",
                Op::Php => "PHP",
                Op::Pla => "PLA",
                Op::Plp => "PLP",
                Op::Rol => "ROL",
                Op::Ror => "ROR",
                Op::Rti => "RTI",
                Op::Rts => "RTS",
                Op::Sbc => "SBC",
                Op::Sec => "SEC",
                Op::Sed => "SED",
                Op::Sei => "SEI",
                Op::Sta => "STA",
                Op::Stx => "STX",
                Op::Sty => "STY",
                Op::Tax => "TAX",
                Op::Tay => "TAY",
                Op::Tsx => "TSX",
                Op::Txa => "TXA",
                Op::Txs => "TXS",
                Op::Tya => "TYA",
                Op::Ahx => "AHX",
                Op::Alr => "ALR",
                Op::Anc => "ANC",
                Op::Arr => "ARR",
                Op::Axs => "AXS",
                Op::Dcp => "DCP",
                Op::Kil => "KIL",
                Op::Isc => "ISC",
                Op::Las => "LAS",
                Op::Lax => "LAX",
                Op::Rla => "RLA",
                Op::Rra => "RRA",
                Op::Sax => "SAX",
                Op::Shx => "SHX",
                Op::Shy => "SHY",
                Op::Slo => "SLO",
                Op::Sre => "SRE",
                Op::Tas => "TAS",
                Op::Xaa => "XAA",
            }
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub enum AddrMode {
    /// No argument (implicit)
    Imp,
    /// Accumulator
    Acc,
    /// Adress `pc + 1`
    Imm,
    /// Adress `fetch8()`
    Zp0,
    /// Adress `(fetch8() + x) & 0xFF`
    ZpX,
    /// Adress `(fetch8() + y) & 0xFF`
    ZpY,
    /// Adress `fetch16()`
    Abs,
    /// Adress `fetch16() + x`
    AbX,
    /// Adress `fetch16() + y`
    AbY,
    /// Adress `(i8)fetch8() + pc`
    Rel,
    /// Adress `mem16_pw(fetch16())`
    ///
    /// The page wraps at the end, see `Cpu::read_mem16_pw`
    Ind,
    /// Address `mem16_pw((fetch8() + x) & 0xFF)`
    ///
    /// The page wraps at the end, see `Cpu::read_mem16_pw`.
    IdX,
    /// Address `mem16_pw(fetch8())`
    ///
    /// The page wraps at the end, see `Cpu::read_mem16_pw`.
    IdY,
}

impl AddrMode {
    pub fn fetched_bytes(&self) -> u8 {
        match self {
            AddrMode::Imp => 0,
            AddrMode::Acc => 0,
            AddrMode::Imm => 1,
            AddrMode::Zp0 => 1,
            AddrMode::ZpX => 1,
            AddrMode::ZpY => 1,
            AddrMode::Abs => 2,
            AddrMode::AbX => 2,
            AddrMode::AbY => 2,
            AddrMode::Rel => 2,
            AddrMode::Ind => 2,
            AddrMode::IdX => 1,
            AddrMode::IdY => 1,
        }
    }
}

pub const OPCODE_MATRIX: [(Op, AddrMode, u8); 256] = {
    use AddrMode::*;
    use Op::*;

    #[rustfmt::skip]
    let matrix: [(Op, AddrMode, u8); 256] = [
            (Brk, Imp, 7), (Ora, IdX, 6), (Kil, Imp, 0), (Slo, IdX, 8), (Nop, Zp0, 3), (Ora, Zp0, 3), (Asl, Zp0, 5), (Slo, Zp0, 5), (Php, Imp, 3), (Ora, Imm, 2), (Asl, Imp, 2), (Anc, Imm, 2), (Nop, Abs, 4), (Ora, Abs, 4), (Asl, Abs, 6), (Slo, Abs, 6),
            (Bpl, Rel, 2), (Ora, IdY, 5), (Kil, Imp, 0), (Slo, IdY, 8), (Nop, ZpX, 4), (Ora, ZpX, 4), (Asl, ZpX, 6), (Slo, ZpX, 6), (Clc, Imp, 2), (Ora, AbY, 4), (Nop, Imp, 2), (Slo, AbY, 7), (Nop, AbX, 4), (Ora, AbX, 4), (Asl, AbX, 7), (Slo, AbX, 7),
            (Jsr, Abs, 6), (And, IdX, 6), (Kil, Imp, 0), (Rla, IdX, 8), (Bit, Zp0, 3), (And, Zp0, 3), (Rol, Zp0, 5), (Rla, Zp0, 5), (Plp, Imp, 4), (And, Imm, 2), (Rol, Imp, 2), (Anc, Imm, 2), (Bit, Abs, 4), (And, Abs, 4), (Rol, Abs, 6), (Rla, Abs, 6),
            (Bmi, Rel, 2), (And, IdY, 5), (Kil, Imp, 0), (Rla, IdY, 8), (Nop, ZpX, 4), (And, ZpX, 4), (Rol, ZpX, 6), (Rla, ZpX, 6), (Sec, Imp, 2), (And, AbY, 4), (Nop, Imp, 2), (Rla, AbY, 7), (Nop, AbX, 4), (And, AbX, 4), (Rol, AbX, 7), (Rla, AbX, 7),
            (Rti, Imp, 6), (Eor, IdX, 6), (Kil, Imp, 0), (Sre, IdX, 8), (Nop, Zp0, 3), (Eor, Zp0, 3), (Lsr, Zp0, 5), (Sre, Zp0, 5), (Pha, Imp, 3), (Eor, Imm, 2), (Lsr, Imp, 2), (Alr, Imm, 2), (Jmp, Abs, 3), (Eor, Abs, 4), (Lsr, Abs, 6), (Sre, Abs, 6),
            (Bvc, Rel, 2), (Eor, IdY, 5), (Kil, Imp, 0), (Sre, IdY, 8), (Nop, ZpX, 4), (Eor, ZpX, 4), (Lsr, ZpX, 6), (Sre, ZpX, 6), (Cli, Imp, 2), (Eor, AbY, 4), (Nop, Imp, 2), (Sre, AbY, 7), (Nop, AbX, 4), (Eor, AbX, 4), (Lsr, AbX, 7), (Sre, AbX, 7),
            (Rts, Imp, 6), (Adc, IdX, 6), (Kil, Imp, 0), (Rra, IdX, 8), (Nop, Zp0, 3), (Adc, Zp0, 3), (Ror, Zp0, 5), (Rra, Zp0, 5), (Pla, Imp, 4), (Adc, Imm, 2), (Ror, Imp, 2), (Arr, Imm, 2), (Jmp, Ind, 5), (Adc, Abs, 4), (Ror, Abs, 6), (Rra, Abs, 6),
            (Bvs, Rel, 2), (Adc, IdY, 5), (Kil, Imp, 0), (Rra, IdY, 8), (Nop, ZpX, 4), (Adc, ZpX, 4), (Ror, ZpX, 6), (Rra, ZpX, 6), (Sei, Imp, 2), (Adc, AbY, 4), (Nop, Imp, 2), (Rra, AbY, 7), (Nop, AbX, 4), (Adc, AbX, 4), (Ror, AbX, 7), (Rra, AbX, 7),
            (Nop, Imm, 2), (Sta, IdX, 6), (Nop, Imm, 2), (Sax, IdX, 6), (Sty, Zp0, 3), (Sta, Zp0, 3), (Stx, Zp0, 3), (Sax, Zp0, 3), (Dey, Imp, 2), (Nop, Imm, 2), (Txa, Imp, 2), (Xaa, Imm, 2), (Sty, Abs, 4), (Sta, Abs, 4), (Stx, Abs, 4), (Sax, Abs, 4),
            (Bcc, Rel, 2), (Sta, IdY, 6), (Kil, Imp, 0), (Ahx, IdY, 6), (Sty, ZpX, 4), (Sta, ZpX, 4), (Stx, ZpY, 4), (Sax, ZpY, 4), (Tya, Imp, 2), (Sta, AbY, 5), (Txs, Imp, 2), (Tas, AbY, 5), (Shy, AbX, 5), (Sta, AbX, 5), (Shx, AbY, 5), (Ahx, AbY, 5),
            (Ldy, Imm, 2), (Lda, IdX, 6), (Ldx, Imm, 2), (Lax, IdX, 6), (Ldy, Zp0, 3), (Lda, Zp0, 3), (Ldx, Zp0, 3), (Lax, Zp0, 3), (Tay, Imp, 2), (Lda, Imm, 2), (Tax, Imp, 2), (Lax, Imm, 2), (Ldy, Abs, 4), (Lda, Abs, 4), (Ldx, Abs, 4), (Lax, Abs, 4),
            (Bcs, Rel, 2), (Lda, IdY, 5), (Kil, Imp, 0), (Lax, IdY, 5), (Ldy, ZpX, 4), (Lda, ZpX, 4), (Ldx, ZpY, 4), (Lax, ZpY, 4), (Clv, Imp, 2), (Lda, AbY, 4), (Tsx, Imp, 2), (Las, AbY, 4), (Ldy, AbX, 4), (Lda, AbX, 4), (Ldx, AbY, 4), (Lax, AbY, 4),
            (Cpy, Imm, 2), (Cmp, IdX, 6), (Nop, Imm, 2), (Dcp, IdX, 8), (Cpy, Zp0, 3), (Cmp, Zp0, 3), (Dec, Zp0, 5), (Dcp, Zp0, 5), (Iny, Imp, 2), (Cmp, Imm, 2), (Dex, Imp, 2), (Axs, Imm, 2), (Cpy, Abs, 4), (Cmp, Abs, 4), (Dec, Abs, 6), (Dcp, Abs, 6),
            (Bne, Rel, 2), (Cmp, IdY, 5), (Kil, Imp, 0), (Dcp, IdY, 8), (Nop, ZpX, 4), (Cmp, ZpX, 4), (Dec, ZpX, 6), (Dcp, ZpX, 6), (Cld, Imp, 2), (Cmp, AbY, 4), (Nop, Imp, 2), (Dcp, AbY, 7), (Nop, AbX, 4), (Cmp, AbX, 4), (Dec, AbX, 7), (Dcp, AbX, 7),
            (Cpx, Imm, 2), (Sbc, IdX, 6), (Nop, Imm, 2), (Isc, IdX, 8), (Cpx, Zp0, 3), (Sbc, Zp0, 3), (Inc, Zp0, 5), (Isc, Zp0, 5), (Inx, Imp, 2), (Sbc, Imm, 2), (Nop, Imp, 2), (Sbc, Imm, 2), (Cpx, Abs, 4), (Sbc, Abs, 4), (Inc, Abs, 6), (Isc, Abs, 6),
            (Beq, Rel, 2), (Sbc, IdY, 5), (Kil, Imp, 0), (Isc, IdY, 8), (Nop, ZpX, 4), (Sbc, ZpX, 4), (Inc, ZpX, 6), (Isc, ZpX, 6), (Sed, Imp, 2), (Sbc, AbY, 4), (Nop, Imp, 2), (Isc, AbY, 7), (Nop, AbX, 4), (Sbc, AbX, 4), (Inc, AbX, 7), (Isc, AbX, 7),

    ];
    matrix
};
