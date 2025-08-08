use itertools::Itertools;
use ryu::Buffer;

use crate::compiler::{
    codegen::AssemblySymbolInfo,
    types::{ComparableStatic, StaticInitialiser},
};

use super::{
    convert::ConvertContext, validate::ValidateContext, AssemblyType, BinaryOperator,
    ConditionCode, ImmediateValue, Instruction, Operand, Program, Register, TopLevel,
    UnaryOperator,
};
use std::{collections::HashMap, fmt::Display};

pub trait CodeDisplay {
    fn show(&self, context: &mut DisplayContext) -> String;
}

#[derive(Debug)]
pub struct DisplayContext {
    comments: bool,
    indent: usize,
    is_linux: bool,
    is_mac: bool,
    word_length_bytes: i32,
    instruction_suffix: String,
    pub symbols: HashMap<String, AssemblySymbolInfo>,
}

impl DisplayContext {
    pub fn new(context: &ConvertContext, validate_context: ValidateContext) -> DisplayContext {
        DisplayContext {
            comments: context.comments,
            indent: 0,
            word_length_bytes: 4,
            instruction_suffix: "l".to_string(),
            is_linux: context.is_linux,
            is_mac: context.is_mac,
            symbols: validate_context.symbols,
        }
    }
    fn indent(&mut self) -> &mut DisplayContext {
        self.indent += 4;
        self
    }
    fn unindent(&mut self) -> &mut DisplayContext {
        self.indent -= 4;
        self
    }
    fn short(&mut self) -> &mut DisplayContext {
        self.word_length_bytes = 1;
        self.instruction_suffix = "b".to_string(); // unused
        self
    }
    fn regular(&mut self) -> &mut DisplayContext {
        self.word_length_bytes = 4;
        self.instruction_suffix = "l".to_string();
        self
    }
    fn long(&mut self) -> &mut DisplayContext {
        self.word_length_bytes = 8;
        self.instruction_suffix = "q".to_string();
        self
    }
    fn double(&mut self) -> &mut DisplayContext {
        self.word_length_bytes = 8;
        self.instruction_suffix = "sd".to_string();
        self
    }
    fn suffix_for_type(&mut self, t: &AssemblyType) -> String {
        match t {
            AssemblyType::Longword => {
                self.regular();
            }
            AssemblyType::Quadword => {
                self.long();
            }
            AssemblyType::Double => {
                self.double();
            }
            AssemblyType::Byte => {
                self.short();
            }
            AssemblyType::ByteArray(_, _) => unreachable!(),
        }
        self.instruction_suffix.clone()
    }
}

impl<T> CodeDisplay for Vec<T>
where
    T: CodeDisplay,
{
    fn show(&self, context: &mut DisplayContext) -> String {
        self.iter().map(|f| f.show(context)).join("\n")
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // do some rust borrow checker wizardry to access the Program's
        // DisplayContext mutably.
        let context_ref = self.displaying_context.as_ref().unwrap();
        let mut context_object = context_ref.borrow_mut();
        let context = &mut *context_object;

        writeln!(f, "{}", self.body.show(context))?;
        context.indent();
        if context.is_linux {
            if context.comments {
                write!(
                    f,
                    "{}",
                    "# Tell the program we don't need an executable stack"
                        .to_string()
                        .show(context)
                )?;
            }
            write!(
                f,
                "{}",
                ".section .note.GNU-stack,\"\",@progbits\n"
                    .to_string()
                    .show(context)
            )?;
        };
        Ok(())
    }
}

impl CodeDisplay for TopLevel {
    fn show(&self, context: &mut DisplayContext) -> String {
        match self {
            TopLevel::Function(name, instructions, global) => {
                let function_name = if context.is_mac {
                    "_".to_string() + name
                } else {
                    name.clone()
                };

                context.indent();
                let mut out = if *global {
                    vec![format!(".globl {}", function_name), ".text".to_string()].show(context)
                        + "\n"
                } else {
                    vec![".text".to_string()].show(context) + "\n"
                };
                context.unindent();
                out += format!("{}:", function_name).show(context).as_str();
                out += "\n";
                context.indent();
                out += &instructions.show(context);
                out += "\n";
                context.unindent();
                out
            }
            TopLevel::StaticVariable(name, global, alignment, initialisers) => {
                let (address, align) = if context.is_mac {
                    (format!("_{}", name), ".balign")
                } else {
                    (name.to_string(), ".align")
                };

                // only write to data if all initialisers are ZeroBytes values
                let section = if initialisers.iter().any(|init| {
                    !matches!(
                        init,
                        StaticInitialiser::Ordinal(ComparableStatic::ZeroBytes(_))
                    )
                }) {
                    ".data".to_string()
                } else {
                    ".bss".to_string()
                };

                context.indent();
                let mut out = "".to_string();
                if *global {
                    out += format!(".globl {}\n", address.clone())
                        .show(context)
                        .as_str();
                }
                out += section.to_string().show(context).as_str();
                out += "\n";
                // 1-byte alignment is meaningless, so can be omitted
                if *alignment != 1 {
                    out += format!("{} {}", align, alignment).show(context).as_str();
                    out += "\n";
                }
                context.unindent();
                out += format!("{}:", address).show(context).as_str();
                out += "\n";
                context.indent();
                for init in initialisers {
                    out += init.show(context).as_str();
                    out += "\n";
                }
                context.unindent();
                out
            }
            TopLevel::StaticConstant(name, alignment, init) => {
                let label_start = if let AssemblySymbolInfo::Object(_, _, is_top_level_constant) =
                    context.symbols.get(name).unwrap()
                {
                    if *is_top_level_constant && context.is_mac {
                        "L"
                    } else if *is_top_level_constant {
                        ".L"
                    } else if context.is_mac {
                        "_"
                    } else {
                        ""
                    }
                } else {
                    unreachable!()
                };

                let (address, align) = if context.is_mac {
                    (format!("_{}", name), ".balign")
                } else {
                    (name.to_string(), ".align")
                };

                let section = if context.is_mac {
                    if matches!(
                        init,
                        StaticInitialiser::Ordinal(ComparableStatic::String(_, _))
                    ) {
                        ".cstring"
                    } else {
                        match alignment {
                            8 => ".literal8",
                            16 => ".literal16",
                            _ => unreachable!(),
                        }
                    }
                } else {
                    ".section .rodata"
                };

                context.indent();
                let mut out = "".to_string();
                out += section.to_string().show(context).as_str();
                out += "\n";
                out += format!("{} {}", align, alignment).show(context).as_str();
                out += "\n";
                context.unindent();
                out += format!("{}{}:", label_start, address)
                    .show(context)
                    .as_str();
                out += "\n";
                context.indent();
                out += init.show(context).as_str();
                out += "\n";
                if context.is_mac && *alignment == 16 {
                    // here, alignment is 16 but we allocate 8 bytes for
                    // negative_zero, which is currently our only use of 16-alignment
                    // Allocate 8 more to avoid confusing the linker.
                    out += ".quad 0".to_string().show(context).as_str();
                    out += "\n";
                }
                context.unindent();
                out
            }
        }
    }
}

impl CodeDisplay for StaticInitialiser {
    fn show(&self, context: &mut DisplayContext) -> String {
        let s = match self {
            StaticInitialiser::Ordinal(ComparableStatic::Integer(0))
            | StaticInitialiser::Ordinal(ComparableStatic::Long(0))
            | StaticInitialiser::Ordinal(ComparableStatic::UnsignedInteger(0))
            | StaticInitialiser::Ordinal(ComparableStatic::UnsignedLong(0)) => unreachable!(),
            StaticInitialiser::Ordinal(ComparableStatic::Integer(i)) => format!(".long {}", i),
            StaticInitialiser::Ordinal(ComparableStatic::Long(l)) => format!(".quad {}", l),
            StaticInitialiser::Ordinal(ComparableStatic::UnsignedInteger(i)) => {
                format!(".long {}", i)
            }
            StaticInitialiser::Ordinal(ComparableStatic::UnsignedLong(l)) => format!(".quad {}", l),
            StaticInitialiser::Ordinal(ComparableStatic::Char(l)) => {
                format!(".byte {}", *l as u8)
            }
            // converting unsigned char to signed char here (only to convert it back in a second)
            StaticInitialiser::Ordinal(ComparableStatic::UnsignedChar(l)) => {
                format!(".byte {}", l)
            }
            StaticInitialiser::Ordinal(ComparableStatic::ZeroBytes(n)) => format!(".zero {}", n),
            StaticInitialiser::Ordinal(ComparableStatic::String(l, term)) => {
                if *term {
                    format!(".asciz \"{}\"", l.iter().map(|f| f.show(context)).join(""))
                } else {
                    format!(".ascii \"{}\"", l.iter().map(|f| f.show(context)).join(""))
                }
            }
            StaticInitialiser::Ordinal(ComparableStatic::Pointer(l)) => format!(".quad {}", l),
            // print 17 digits of precision so that when the linker re-converts this to a real
            // double value, this has enough precision to guarantee the original value.
            // no special case for zero values here, so that we can avoid confusion about
            // 0.0 vs -0.0
            StaticInitialiser::Double(d) => {
                // use the actual bits of the f64 value in the assembly
                // more precise (if needed), but less nice to look at/debug
                // let as_number = d.to_bits();
                // format!(".quad {}", as_number)

                // alternatively, pretty print
                let mut ryu_buffer = Buffer::new();
                format!(".double {}", ryu_buffer.format(*d))
            }
        };

        format!("{:indent$}{}", "", s, indent = context.indent)
    }
}

impl CodeDisplay for i8 {
    fn show(&self, _context: &mut DisplayContext) -> String {
        char::from_u32(*self as u32)
            .map(|c| match c {
                '\n' => r"\n".to_string(),
                '\r' => r"\r".to_string(),
                '\\' => r"\\".to_string(),
                '\"' => r#"\""#.to_string(),
                _ => match c as u32 {
                    7 => r"\7".to_string(),
                    8 => r"\8".to_string(),
                    11 => r"\11".to_string(),
                    12 => r"\12".to_string(),
                    _ => c.to_string(),
                },
            })
            .unwrap_or("NULL".to_string())
    }
}

impl CodeDisplay for String {
    fn show(&self, context: &mut DisplayContext) -> String {
        format!("{:indent$}{}", "", self, indent = context.indent)
    }
}

impl CodeDisplay for Instruction {
    fn show(&self, context: &mut DisplayContext) -> String {
        match self {
            Instruction::Ret => {
                if context.comments {
                    format!(
                        "{:indent$}# return from this function\n\
                            {:indent$}# Restore RBP to where it was previously\n\
                            {:indent$}# (whose location was stored at the current RBP),\n\
                            {:indent$}# to free up the memory used by this function.\n\
                            {:indent$}# popq reads the value at RSP.\n\
                            {:indent$}movq %rbp, %rsp\n\
                            {:indent$}popq %rbp\n\
                            {:indent$}ret",
                        "",
                        "",
                        "",
                        "",
                        "",
                        "",
                        "",
                        "",
                        indent = context.indent
                    )
                } else {
                    format!(
                        "{:indent$}movq %rbp, %rsp\n\
                            {:indent$}popq %rbp\n\
                            {:indent$}ret",
                        "",
                        "",
                        "",
                        indent = context.indent
                    )
                }
            }
            Instruction::Mov(t, src, dst) => {
                if context.comments {
                    format!(
                        "{:indent$}# movl means move a 32 bit word (left) to a register (right)\n\
                            {:indent$}mov{} {}, {}",
                        "",
                        "",
                        context.suffix_for_type(t),
                        src.show(context),
                        dst.show(context),
                        indent = context.indent
                    )
                } else {
                    format!(
                        "{:indent$}mov{} {}, {}",
                        "",
                        context.suffix_for_type(t),
                        src.show(context),
                        dst.show(context),
                        indent = context.indent
                    )
                }
            }
            Instruction::Movsx(src_type, dst_type, src, dst) => {
                let src_suffix = context.suffix_for_type(src_type);
                let src_show = src.show(context);
                format!(
                    "{:indent$}movs{}{} {}, {}",
                    "",
                    src_suffix,
                    context.suffix_for_type(dst_type),
                    src_show,
                    dst.show(context),
                    indent = context.indent
                )
            }
            Instruction::MovZeroExtend(src_type, dst_type, src, dst) => {
                let src_suffix = context.suffix_for_type(src_type);
                let src_show = src.show(context);
                format!(
                    "{:indent$}movz{}{} {}, {}",
                    "",
                    src_suffix,
                    context.suffix_for_type(dst_type),
                    src_show,
                    dst.show(context),
                    indent = context.indent
                )
            }
            Instruction::Cvttsd2si(dst_type, src, dst) => {
                format!(
                    "{:indent$}cvttsd2si{} {}, {}",
                    "",
                    context.suffix_for_type(dst_type),
                    src.show(context),
                    dst.show(context),
                    indent = context.indent
                )
            }
            Instruction::Cvtsi2sd(src_type, src, dst) => {
                format!(
                    "{:indent$}cvtsi2sd{} {}, {}",
                    "",
                    context.suffix_for_type(src_type),
                    src.show(context),
                    dst.show(context),
                    indent = context.indent
                )
            }
            Instruction::Unary(op, t, src) => {
                format!(
                    "{:indent$}{}{} {}",
                    "",
                    op.show(context),
                    context.suffix_for_type(t),
                    src.show(context),
                    indent = context.indent
                )
            }
            Instruction::Binary(op, t, src, dst)
                if matches!(
                    op,
                    BinaryOperator::ShiftLeft
                        | BinaryOperator::ShiftRight
                        | BinaryOperator::UnsignedShiftLeft
                        | BinaryOperator::UnsignedShiftRight
                ) =>
            {
                // bit-shift operations (as of today) require that the first argument, ie the
                // number to shift by, is 1 byte long. This is sensible since we don't represent
                // any numbers higher than 2^256 anyway. I'm just not sure why this seems to have
                // come up as a regression in my code today, maybe a sneaky GCC change...?
                context.short();
                let shift_by = src.show(context);
                format!(
                    "{:indent$}{}{} {}, {}",
                    "",
                    op.show(context),
                    context.suffix_for_type(t),
                    shift_by,
                    dst.show(context),
                    indent = context.indent
                )
            }
            Instruction::Binary(op, t, src, dst) => {
                if *t == AssemblyType::Double {
                    context.double();
                    format!(
                        "{:indent$}{} {}, {}",
                        "",
                        op.show_double(context),
                        src.show(context),
                        dst.show(context),
                        indent = context.indent
                    )
                } else {
                    format!(
                        "{:indent$}{}{} {}, {}",
                        "",
                        op.show(context),
                        context.suffix_for_type(t),
                        src.show(context),
                        dst.show(context),
                        indent = context.indent
                    )
                }
            }
            Instruction::Idiv(t, src) => {
                if context.comments {
                    format!(
                        "{:indent$}# Read the dividend from EDX and EAX,\n\
                            {:indent$}# and then store the quotient in EDX and\n\
                            {:indent$}# the remainder in EAX\n\
                            {:indent$}idiv{} {}",
                        "",
                        "",
                        "",
                        "",
                        context.suffix_for_type(t),
                        src.show(context),
                        indent = context.indent
                    )
                } else {
                    format!(
                        "{:indent$}idiv{} {}",
                        "",
                        context.suffix_for_type(t),
                        src.show(context),
                        indent = context.indent
                    )
                }
            }
            Instruction::Div(t, src) => {
                format!(
                    "{:indent$}div{} {}",
                    "",
                    context.suffix_for_type(t),
                    src.show(context),
                    indent = context.indent
                )
            }
            Instruction::Cdq(AssemblyType::Byte | AssemblyType::Longword) => {
                if context.comments {
                    format!(
                        "{:indent$}# Extend the 32-bit value in EAX to a 64-bit\n\
                            {:indent$}# value spanning EDX and EAX\n\
                            {:indent$}cdq",
                        "",
                        "",
                        "",
                        indent = context.indent
                    )
                } else {
                    format!("{:indent$}cdq", "", indent = context.indent)
                }
            }
            Instruction::Cdq(AssemblyType::Quadword | AssemblyType::Double) => {
                format!("{:indent$}cqo", "", indent = context.indent)
            }
            Instruction::Cdq(AssemblyType::ByteArray(_size, _alignment)) => {
                todo!()
            }
            Instruction::Cmp(t, left, right) => {
                if *t == AssemblyType::Double {
                    format!(
                        "{:indent$}comi{} {}, {}",
                        "",
                        context.suffix_for_type(t),
                        left.show(context),
                        right.show(context),
                        indent = context.indent
                    )
                } else {
                    format!(
                        "{:indent$}cmp{} {}, {}",
                        "",
                        context.suffix_for_type(t),
                        left.show(context),
                        right.show(context),
                        indent = context.indent
                    )
                }
            }
            Instruction::Jmp(s) => {
                let label_start = if context.is_mac { "L" } else { ".L" };
                format!(
                    "{:indent$}jmp {}{}",
                    "",
                    label_start,
                    s,
                    indent = context.indent,
                )
            }
            Instruction::JmpCondition(c, s) => {
                let label_start = if context.is_mac { "L" } else { ".L" };
                format!(
                    "{:indent$}j{} {}{}",
                    "",
                    c.show(context),
                    label_start,
                    s,
                    indent = context.indent,
                )
            }
            Instruction::SetCondition(c, dst) => {
                context.short();
                let out = format!(
                    "{:indent$}set{} {}",
                    "",
                    c.show(context),
                    dst.show(context),
                    indent = context.indent,
                );
                context.regular();
                out
            }
            Instruction::Label(s) => {
                context.unindent();
                let label_start = if context.is_mac { "L" } else { ".L" };
                let out = format!(
                    "{:indent$}{}{}:",
                    "",
                    label_start,
                    s,
                    indent = context.indent,
                );
                context.indent();
                out
            }
            Instruction::Push(o) => {
                context.long();
                let out = format!(
                    "{:indent$}pushq {}",
                    "",
                    o.show(context),
                    indent = context.indent
                );
                context.regular();
                out
            }
            Instruction::Call(s) => {
                let function_name = if context.is_mac {
                    "_".to_string() + s
                } else if context.is_linux {
                    if let AssemblySymbolInfo::Function(false) = context.symbols.get(s).unwrap() {
                        // if a function isn't defined in this file, call it from another file (ie
                        // externally link it)
                        s.to_string() + "@PLT"
                    } else {
                        s.to_string()
                    }
                } else {
                    s.to_string()
                };
                format!(
                    "{:indent$}call {}",
                    "",
                    function_name,
                    indent = context.indent
                )
            }
            Instruction::Lea(src, dst) => {
                context.long();
                format!(
                    "{:indent$}lea{} {}, {}",
                    "",
                    context.instruction_suffix.clone(),
                    src.show(context),
                    dst.show(context),
                    indent = context.indent
                )
            }
        }
    }
}

impl CodeDisplay for Operand {
    fn show(&self, context: &mut DisplayContext) -> String {
        match self {
            Operand::Imm(value) => match value {
                ImmediateValue::Signed(i) => format!("${}", i),
                ImmediateValue::Unsigned(i) => format!("${}", i),
            },
            Operand::Reg(reg) => reg.show(context),
            Operand::MockReg(name) => panic!(
                "Tried to generate assembly code with a mock register: {}",
                name
            ),
            Operand::Memory(reg, num) => {
                let previous_word_length = context.word_length_bytes;
                context.word_length_bytes = 8;
                let out = format!("{}({})", num, reg.show(context));
                context.word_length_bytes = previous_word_length;
                out
            }
            Operand::MockMemory(reg, num) => panic!(
                "Tried to generate assembly code with a mock memory address: {}, {}",
                reg, num
            ),
            Operand::Data(name) => {
                let label_start = if let AssemblySymbolInfo::Object(_, _, is_top_level_constant) =
                    context.symbols.get(name).unwrap()
                {
                    if *is_top_level_constant && context.is_mac {
                        "L"
                    } else if *is_top_level_constant {
                        ".L"
                    } else if context.is_mac {
                        "_"
                    } else {
                        ""
                    }
                } else {
                    unreachable!()
                };

                format!("{}{}(%rip)", label_start, name)
            }
            Operand::Indexed(base, index, scale) => {
                format!(
                    "({}, {}, {})",
                    base.show(context),
                    index.show(context),
                    scale
                )
            }
        }
    }
}

impl CodeDisplay for Register {
    fn show(&self, context: &mut DisplayContext) -> String {
        match context.word_length_bytes {
            1 => match self {
                Register::AX => "%al",
                Register::CX => "%cl",
                Register::DI => "%dil",
                Register::DX => "%dl",
                Register::SI => "%sil",
                Register::R8 => "%r8b",
                Register::R9 => "%r9b",
                Register::R10 => "%r10b",
                Register::R11 => "%r11b",
                Register::SP => "%rsp",
                Register::BP => "%rbp",
                Register::XMM0 => "%xmm0",
                Register::XMM1 => "%xmm1",
                Register::XMM2 => "%xmm2",
                Register::XMM3 => "%xmm3",
                Register::XMM4 => "%xmm4",
                Register::XMM5 => "%xmm5",
                Register::XMM6 => "%xmm6",
                Register::XMM7 => "%xmm7",
                Register::XMM14 => "%xmm14",
                Register::XMM15 => "%xmm15",
            },
            4 => match self {
                Register::AX => "%eax",
                Register::CX => "%ecx",
                Register::DI => "%edi",
                Register::DX => "%edx",
                Register::SI => "%esi",
                Register::R8 => "%r8d",
                Register::R9 => "%r9d",
                Register::R10 => "%r10d",
                Register::R11 => "%r11d",
                Register::SP => "%rsp",
                Register::BP => "%rbp",
                Register::XMM0 => "%xmm0",
                Register::XMM1 => "%xmm1",
                Register::XMM2 => "%xmm2",
                Register::XMM3 => "%xmm3",
                Register::XMM4 => "%xmm4",
                Register::XMM5 => "%xmm5",
                Register::XMM6 => "%xmm6",
                Register::XMM7 => "%xmm7",
                Register::XMM14 => "%xmm14",
                Register::XMM15 => "%xmm15",
            },
            8 => match self {
                Register::AX => "%rax",
                Register::CX => "%rcx",
                Register::DI => "%rdi",
                Register::DX => "%rdx",
                Register::SI => "%rsi",
                Register::R8 => "%r8",
                Register::R9 => "%r9",
                Register::R10 => "%r10",
                Register::R11 => "%r11",
                Register::SP => "%rsp",
                Register::BP => "%rbp",
                Register::XMM0 => "%xmm0",
                Register::XMM1 => "%xmm1",
                Register::XMM2 => "%xmm2",
                Register::XMM3 => "%xmm3",
                Register::XMM4 => "%xmm4",
                Register::XMM5 => "%xmm5",
                Register::XMM6 => "%xmm6",
                Register::XMM7 => "%xmm7",
                Register::XMM14 => "%xmm14",
                Register::XMM15 => "%xmm15",
            },
            _ => panic!("Invalid word length: {}", context.word_length_bytes),
        }
        .to_string()
    }
}

impl CodeDisplay for UnaryOperator {
    fn show(&self, _context: &mut DisplayContext) -> String {
        match self {
            UnaryOperator::Neg => "neg",
            UnaryOperator::Not => "not",
        }
        .to_string()
    }
}

impl CodeDisplay for BinaryOperator {
    fn show(&self, _context: &mut DisplayContext) -> String {
        match self {
            BinaryOperator::Add => "add",
            BinaryOperator::Sub => "sub",
            BinaryOperator::Mult => "imul",
            BinaryOperator::ShiftLeft => "sal",
            BinaryOperator::ShiftRight => "sar",
            BinaryOperator::UnsignedShiftLeft => "shl",
            BinaryOperator::UnsignedShiftRight => "shr",
            BinaryOperator::And => "and",
            BinaryOperator::Xor => "xor",
            BinaryOperator::Or => "or",
            BinaryOperator::DivDouble => "div",
        }
        .to_string()
    }
}

impl BinaryOperator {
    fn show_double(&self, context: &mut DisplayContext) -> String {
        match self {
            BinaryOperator::Mult => "mulsd".to_string(),
            BinaryOperator::Xor => "xorpd".to_string(),
            op => format!("{}{}", op.show(context), context.instruction_suffix),
        }
    }
}

impl CodeDisplay for ConditionCode {
    fn show(&self, _context: &mut DisplayContext) -> String {
        match self {
            ConditionCode::E => "e",
            ConditionCode::Ne => "ne",
            ConditionCode::G => "g",
            ConditionCode::Ge => "ge",
            ConditionCode::L => "l",
            ConditionCode::Le => "le",
            ConditionCode::A => "a",
            ConditionCode::Ae => "ae",
            ConditionCode::B => "b",
            ConditionCode::Be => "be",
            ConditionCode::P => "p",
        }
        .to_string()
    }
}
