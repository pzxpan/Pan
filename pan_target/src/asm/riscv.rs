use super::{InlineAsmArch, InlineAsmType};
use crate::spec::Target;
use pan_macros::HashStable_Generic;
use std::fmt;

def_reg_class! {
    RiscV RiscVInlineAsmRegClass {
        reg,
        freg,
    }
}

impl RiscVInlineAsmRegClass {
    pub fn valid_modifiers(self, _arch: super::InlineAsmArch) -> &'static [char] {
        &[]
    }

    pub fn suggest_class(self, _arch: InlineAsmArch, _ty: InlineAsmType) -> Option<Self> {
        None
    }

    pub fn suggest_modifier(
        self,
        _arch: InlineAsmArch,
        _ty: InlineAsmType,
    ) -> Option<(char, &'static str)> {
        None
    }

    pub fn default_modifier(self, _arch: InlineAsmArch) -> Option<(char, &'static str)> {
        None
    }

    pub fn supported_types(
        self,
        arch: InlineAsmArch,
    ) -> &'static [(InlineAsmType, Option<&'static str>)] {
        match self {
            Self::reg => {
                if arch == InlineAsmArch::RiscV64 {
                    types! { _: I8, I16, I32, I64, F32, F64; }
                } else {
                    types! { _: I8, I16, I32, F32; }
                }
            }
            Self::freg => types! { "f": F32; "d": F64; },
        }
    }
}

fn not_e(
    _arch: InlineAsmArch,
    mut has_feature: impl FnMut(&str) -> bool,
    _target: &Target,
    _allocating: bool,
) -> Result<(), &'static str> {
    if has_feature("e") {
        Err("register can't be used with the `e` target feature")
    } else {
        Ok(())
    }
}

def_regs! {
    RiscV RiscVInlineAsmReg RiscVInlineAsmRegClass {
        x1: reg = ["x1", "ra"],
        x5: reg = ["x5", "t0"],
        x6: reg = ["x6", "t1"],
        x7: reg = ["x7", "t2"],
        x9: reg = ["x9", "s1"],
        x10: reg = ["x10", "a0"],
        x11: reg = ["x11", "a1"],
        x12: reg = ["x12", "a2"],
        x13: reg = ["x13", "a3"],
        x14: reg = ["x14", "a4"],
        x15: reg = ["x15", "a5"],
        x16: reg = ["x16", "a6"] % not_e,
        x17: reg = ["x17", "a7"] % not_e,
        x18: reg = ["x18", "s2"] % not_e,
        x19: reg = ["x19", "s3"] % not_e,
        x20: reg = ["x20", "s4"] % not_e,
        x21: reg = ["x21", "s5"] % not_e,
        x22: reg = ["x22", "s6"] % not_e,
        x23: reg = ["x23", "s7"] % not_e,
        x24: reg = ["x24", "s8"] % not_e,
        x25: reg = ["x25", "s9"] % not_e,
        x26: reg = ["x26", "s10"] % not_e,
        x27: reg = ["x27", "s11"] % not_e,
        x28: reg = ["x28", "t3"] % not_e,
        x29: reg = ["x29", "t4"] % not_e,
        x30: reg = ["x30", "t5"] % not_e,
        x31: reg = ["x31", "t6"] % not_e,
        f0: freg = ["f0", "ft0"],
        f1: freg = ["f1", "ft1"],
        f2: freg = ["f2", "ft2"],
        f3: freg = ["f3", "ft3"],
        f4: freg = ["f4", "ft4"],
        f5: freg = ["f5", "ft5"],
        f6: freg = ["f6", "ft6"],
        f7: freg = ["f7", "ft7"],
        f8: freg = ["f8", "fs0"],
        f9: freg = ["f9", "fs1"],
        f10: freg = ["f10", "fa0"],
        f11: freg = ["f11", "fa1"],
        f12: freg = ["f12", "fa2"],
        f13: freg = ["f13", "fa3"],
        f14: freg = ["f14", "fa4"],
        f15: freg = ["f15", "fa5"],
        f16: freg = ["f16", "fa6"],
        f17: freg = ["f17", "fa7"],
        f18: freg = ["f18", "fs2"],
        f19: freg = ["f19", "fs3"],
        f20: freg = ["f20", "fs4"],
        f21: freg = ["f21", "fs5"],
        f22: freg = ["f22", "fs6"],
        f23: freg = ["f23", "fs7"],
        f24: freg = ["f24", "fs8"],
        f25: freg = ["f25", "fs9"],
        f26: freg = ["f26", "fs10"],
        f27: freg = ["f27", "fs11"],
        f28: freg = ["f28", "ft8"],
        f29: freg = ["f29", "ft9"],
        f30: freg = ["f30", "ft10"],
        f31: freg = ["f31", "ft11"],
        #error = ["x8", "s0", "fp"] =>
            "the frame pointer cannot be used as an operand for inline asm",
        #error = ["x2", "sp"] =>
            "the stack pointer cannot be used as an operand for inline asm",
        #error = ["x3", "gp"] =>
            "the global pointer cannot be used as an operand for inline asm",
        #error = ["x4", "tp"] =>
            "the thread pointer cannot be used as an operand for inline asm" ,
        #error = ["x0", "zero"] =>
            "the zero register cannot be used as an operand for inline asm",
    }
}

impl RiscVInlineAsmReg {
    pub fn emit(
        self,
        out: &mut dyn fmt::Write,
        _arch: InlineAsmArch,
        _modifier: Option<char>,
    ) -> fmt::Result {
        out.write_str(self.name())
    }
}
