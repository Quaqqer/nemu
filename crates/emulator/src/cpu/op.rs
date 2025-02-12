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

/// The opcode matrix, containing the operation, addressing mode, the cycles the instruction takes
/// and if the instruction adds one clock cycle if a page boundary is crossed
pub const OPCODE_MATRIX: [(Op, AddrMode, u8, bool); 256] = {
    use AddrMode::*;
    use Op::*;

    let t = true;
    let f = false;
    #[rustfmt::skip]
    let matrix: [(Op, AddrMode, u8, bool); 256] = [
    /*        x0                x1                x2                x3                x4                x5                x6                x7                x8                x9                xA                 xB               xC                xD                xE                xF        */
    /* 0x */ (Brk, Acc, 7, f), (Ora, IdX, 6, f), (Kil, Acc, 0, f), (Slo, IdX, 8, f), (Nop, Zp0, 3, f), (Ora, Zp0, 3, f), (Asl, Zp0, 5, f), (Slo, Zp0, 5, f), (Php, Acc, 3, f), (Ora, Imm, 2, f), (Asl, Acc, 2, f), (Anc, Imm, 2, f), (Nop, Abs, 4, f), (Ora, Abs, 4, f), (Asl, Abs, 6, f), (Slo, Abs, 6, f),
    /* 1x */ (Bpl, Rel, 2, t), (Ora, IdY, 5, t), (Kil, Acc, 0, f), (Slo, IdY, 8, f), (Nop, ZpX, 4, f), (Ora, ZpX, 4, f), (Asl, ZpX, 6, f), (Slo, ZpX, 6, f), (Clc, Acc, 2, f), (Ora, AbY, 4, t), (Nop, Acc, 2, f), (Slo, AbY, 7, f), (Nop, AbX, 4, t), (Ora, AbX, 4, t), (Asl, AbX, 7, f), (Slo, AbX, 7, f),
    /* 2x */ (Jsr, Abs, 6, f), (And, IdX, 6, f), (Kil, Acc, 0, f), (Rla, IdX, 8, f), (Bit, Zp0, 3, f), (And, Zp0, 3, f), (Rol, Zp0, 5, f), (Rla, Zp0, 5, f), (Plp, Acc, 4, f), (And, Imm, 2, f), (Rol, Acc, 2, f), (Anc, Imm, 2, f), (Bit, Abs, 4, f), (And, Abs, 4, f), (Rol, Abs, 6, f), (Rla, Abs, 6, f),
    /* 3x */ (Bmi, Rel, 2, t), (And, IdY, 5, t), (Kil, Acc, 0, f), (Rla, IdY, 8, f), (Nop, ZpX, 4, f), (And, ZpX, 4, f), (Rol, ZpX, 6, f), (Rla, ZpX, 6, f), (Sec, Acc, 2, f), (And, AbY, 4, t), (Nop, Acc, 2, f), (Rla, AbY, 7, f), (Nop, AbX, 4, t), (And, AbX, 4, t), (Rol, AbX, 7, f), (Rla, AbX, 7, f),
    /* 4x */ (Rti, Acc, 6, f), (Eor, IdX, 6, f), (Kil, Acc, 0, f), (Sre, IdX, 8, f), (Nop, Zp0, 3, f), (Eor, Zp0, 3, f), (Lsr, Zp0, 5, f), (Sre, Zp0, 5, f), (Pha, Acc, 3, f), (Eor, Imm, 2, f), (Lsr, Acc, 2, f), (Alr, Imm, 2, f), (Jmp, Abs, 3, f), (Eor, Abs, 4, f), (Lsr, Abs, 6, f), (Sre, Abs, 6, f),
    /* 5x */ (Bvc, Rel, 2, t), (Eor, IdY, 5, t), (Kil, Acc, 0, f), (Sre, IdY, 8, f), (Nop, ZpX, 4, f), (Eor, ZpX, 4, f), (Lsr, ZpX, 6, f), (Sre, ZpX, 6, f), (Cli, Acc, 2, f), (Eor, AbY, 4, t), (Nop, Acc, 2, f), (Sre, AbY, 7, f), (Nop, AbX, 4, t), (Eor, AbX, 4, t), (Lsr, AbX, 7, f), (Sre, AbX, 7, f),
    /* 6x */ (Rts, Acc, 6, f), (Adc, IdX, 6, f), (Kil, Acc, 0, f), (Rra, IdX, 8, f), (Nop, Zp0, 3, f), (Adc, Zp0, 3, f), (Ror, Zp0, 5, f), (Rra, Zp0, 5, f), (Pla, Acc, 4, f), (Adc, Imm, 2, f), (Ror, Acc, 2, f), (Arr, Imm, 2, f), (Jmp, Ind, 5, f), (Adc, Abs, 4, f), (Ror, Abs, 6, f), (Rra, Abs, 6, f),
    /* 7x */ (Bvs, Rel, 2, t), (Adc, IdY, 5, t), (Kil, Acc, 0, f), (Rra, IdY, 8, f), (Nop, ZpX, 4, f), (Adc, ZpX, 4, f), (Ror, ZpX, 6, f), (Rra, ZpX, 6, f), (Sei, Acc, 2, f), (Adc, AbY, 4, t), (Nop, Acc, 2, f), (Rra, AbY, 7, f), (Nop, AbX, 4, t), (Adc, AbX, 4, t), (Ror, AbX, 7, f), (Rra, AbX, 7, f),
    /* 8x */ (Nop, Imm, 2, f), (Sta, IdX, 6, f), (Nop, Imm, 2, f), (Sax, IdX, 6, f), (Sty, Zp0, 3, f), (Sta, Zp0, 3, f), (Stx, Zp0, 3, f), (Sax, Zp0, 3, f), (Dey, Acc, 2, f), (Nop, Imm, 2, f), (Txa, Acc, 2, f), (Xaa, Imm, 2, f), (Sty, Abs, 4, f), (Sta, Abs, 4, f), (Stx, Abs, 4, f), (Sax, Abs, 4, f),
    /* 9x */ (Bcc, Rel, 2, t), (Sta, IdY, 6, t), (Kil, Acc, 0, f), (Ahx, IdY, 6, t), (Sty, ZpX, 4, f), (Sta, ZpX, 4, f), (Stx, ZpY, 4, f), (Sax, ZpY, 4, f), (Tya, Acc, 2, f), (Sta, AbY, 5, t), (Txs, Acc, 2, f), (Tas, AbY, 5, t), (Shy, AbX, 5, t), (Sta, AbX, 5, t), (Shx, AbY, 5, t), (Ahx, AbY, 5, t),
    /* Ax */ (Ldy, Imm, 2, f), (Lda, IdX, 6, f), (Ldx, Imm, 2, f), (Lax, IdX, 6, f), (Ldy, Zp0, 3, f), (Lda, Zp0, 3, f), (Ldx, Zp0, 3, f), (Lax, Zp0, 3, f), (Tay, Acc, 2, f), (Lda, Imm, 2, f), (Tax, Acc, 2, f), (Lax, Imm, 2, f), (Ldy, Abs, 4, f), (Lda, Abs, 4, f), (Ldx, Abs, 4, f), (Lax, Abs, 4, f),
    /* Bx */ (Bcs, Rel, 2, t), (Lda, IdY, 5, t), (Kil, Acc, 0, f), (Lax, IdY, 5, t), (Ldy, ZpX, 4, f), (Lda, ZpX, 4, f), (Ldx, ZpY, 4, f), (Lax, ZpY, 4, f), (Clv, Acc, 2, f), (Lda, AbY, 4, t), (Tsx, Acc, 2, f), (Las, AbY, 4, t), (Ldy, AbX, 4, t), (Lda, AbX, 4, t), (Ldx, AbY, 4, t), (Lax, AbY, 4, t),
    /* Cx */ (Cpy, Imm, 2, f), (Cmp, IdX, 6, f), (Nop, Imm, 2, f), (Dcp, IdX, 8, f), (Cpy, Zp0, 3, f), (Cmp, Zp0, 3, f), (Dec, Zp0, 5, f), (Dcp, Zp0, 5, f), (Iny, Acc, 2, f), (Cmp, Imm, 2, f), (Dex, Acc, 2, f), (Axs, Imm, 2, f), (Cpy, Abs, 4, f), (Cmp, Abs, 4, f), (Dec, Abs, 6, f), (Dcp, Abs, 6, f),
    /* Dx */ (Bne, Rel, 2, t), (Cmp, IdY, 5, t), (Kil, Acc, 0, f), (Dcp, IdY, 8, f), (Nop, ZpX, 4, f), (Cmp, ZpX, 4, f), (Dec, ZpX, 6, f), (Dcp, ZpX, 6, f), (Cld, Acc, 2, f), (Cmp, AbY, 4, t), (Nop, Acc, 2, f), (Dcp, AbY, 7, f), (Nop, AbX, 4, t), (Cmp, AbX, 4, t), (Dec, AbX, 7, f), (Dcp, AbX, 7, f),
    /* Ex */ (Cpx, Imm, 2, f), (Sbc, IdX, 6, f), (Nop, Imm, 2, f), (Isc, IdX, 8, f), (Cpx, Zp0, 3, f), (Sbc, Zp0, 3, f), (Inc, Zp0, 5, f), (Isc, Zp0, 5, f), (Inx, Acc, 2, f), (Sbc, Imm, 2, f), (Nop, Acc, 2, f), (Sbc, Imm, 2, f), (Cpx, Abs, 4, f), (Sbc, Abs, 4, f), (Inc, Abs, 6, f), (Isc, Abs, 6, f),
    /* Fx */ (Beq, Rel, 2, t), (Sbc, IdY, 5, t), (Kil, Acc, 0, f), (Isc, IdY, 8, f), (Nop, ZpX, 4, f), (Sbc, ZpX, 4, f), (Inc, ZpX, 6, f), (Isc, ZpX, 6, f), (Sed, Acc, 2, f), (Sbc, AbY, 4, t), (Nop, Acc, 2, f), (Isc, AbY, 7, f), (Nop, AbX, 4, t), (Sbc, AbX, 4, t), (Inc, AbX, 7, f), (Isc, AbX, 7, f),

    ];
    matrix
};
