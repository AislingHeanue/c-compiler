use itertools::Itertools;

use crate::compiler::{codegen::AssemblySymbolInfo, parser::StaticInitial};

use super::{
    AssemblyType, BinaryOperator, CodeDisplay, ConditionCode, DisplayContext, Instruction, Operand,
    Program, Register, TopLevel, UnaryOperator,
};
use std::fmt::Display;

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
            TopLevel::StaticVariable(name, global, alignment, init) => {
                let (address, align) = if context.is_mac {
                    (format!("_{}", name), ".balign")
                } else {
                    (name.to_string(), ".align")
                };
                let init_is_zero =
                    matches!(init, StaticInitial::Integer(0) | StaticInitial::Long(0));

                context.indent();
                let mut out = "".to_string();
                if *global {
                    out += format!(".globl {}", address).show(context).as_str();
                    out += "\n";
                }

                if init_is_zero {
                    out += ".bss".to_string().show(context).as_str();
                } else {
                    out += ".data".to_string().show(context).as_str();
                };
                out += "\n";

                out += format!("{} {}", align, alignment).show(context).as_str();
                out += "\n";

                context.unindent();
                out += format!("{}:", address).show(context).as_str();
                out += "\n";

                context.indent();
                out += init.show(context).as_str();
                out += "\n";
                context.unindent();
                out
            }
        }
    }
}

impl CodeDisplay for StaticInitial {
    fn show(&self, context: &mut DisplayContext) -> String {
        let s = match self {
            StaticInitial::Integer(0) => ".zero 4".to_string(),
            StaticInitial::Integer(i) => format!(".long {}", i),
            StaticInitial::Long(0) => ".zero 8".to_string(),
            StaticInitial::Long(l) => format!(".quad {}", l),
        };

        format!("{:indent$}{}", "", s, indent = context.indent)
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
            Instruction::Movsx(src, dst) => {
                format!(
                    "{:indent$}movslq {}, {}",
                    "",
                    src.show(context.regular()),
                    dst.show(context.long()),
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
                if matches!(op, BinaryOperator::ShiftLeft | BinaryOperator::ShiftRight) =>
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
            Instruction::Cdq(AssemblyType::Longword) => {
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
            Instruction::Cdq(AssemblyType::Quadword) => {
                format!("{:indent$}cqo", "", indent = context.indent)
            }
            Instruction::Cmp(t, left, right) => {
                format!(
                    "{:indent$}cmp{} {}, {}",
                    "",
                    context.suffix_for_type(t),
                    left.show(context),
                    right.show(context),
                    indent = context.indent
                )
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
        }
    }
}

impl CodeDisplay for Operand {
    fn show(&self, context: &mut DisplayContext) -> String {
        match self {
            Operand::Imm(int) => {
                format!("${}", int)
            }
            Operand::Reg(reg) => match context.word_length_bytes {
                1 => match reg {
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
                },
                4 => match reg {
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
                },
                8 => match reg {
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
                },
                _ => panic!("Invalid word length: {}", context.word_length_bytes),
            }
            .to_string(),
            Operand::MockReg(name) => panic!(
                "Tried to generate assembly code with a mock register: {}",
                name
            ),
            Operand::Stack(num) => format!("{}(%rbp)", num),
            Operand::Data(name) => {
                if context.is_mac {
                    format!("_{}(%rip)", name)
                } else {
                    format!("{}(%rip)", name)
                }
            }
        }
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
            BinaryOperator::BitwiseAnd => "and",
            BinaryOperator::BitwiseXor => "xor",
            BinaryOperator::BitwiseOr => "or",
        }
        .to_string()
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
        }
        .to_string()
    }
}
