use itertools::Itertools;

use super::{
    BinaryOperator, CodeDisplay, ConditionCode, DisplayContext, Function, Instruction, Operand,
    Program, Register, UnaryOperator,
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
        let context = &mut self.displaying_context.clone();
        write!(f, "{}", self.body.show(context))?;
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

impl CodeDisplay for Function {
    fn show(&self, context: &mut DisplayContext) -> String {
        let function_name = if context.is_mac {
            "_".to_string() + &self.name
        } else {
            self.name.clone()
        };

        context.indent();
        let mut out = format!("{}\n", self.header.show(context));
        context.unindent();
        out += format!(
            "{:indent$}{}:\n",
            "",
            function_name,
            indent = context.indent
        )
        .as_str();
        context.indent();
        out += &self.instructions.show(context);
        context.unindent();
        out
    }
}

impl CodeDisplay for String {
    fn show(&self, context: &mut DisplayContext) -> String {
        format!("\n{:indent$}{}", "", self, indent = context.indent)
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
            Instruction::Mov(src, dst) => {
                if context.comments {
                    format!(
                        "{:indent$}# movl means move a 32 bit word (left) to a register (right)\n\
                            {:indent$}movl {}, {}",
                        "",
                        "",
                        src.show(context),
                        dst.show(context),
                        indent = context.indent
                    )
                } else {
                    format!(
                        "{:indent$}movl {}, {}",
                        "",
                        src.show(context),
                        dst.show(context),
                        indent = context.indent
                    )
                }
            }
            Instruction::Custom(s) => format!("{:indent$}{}", "", s, indent = context.indent),
            Instruction::Unary(op, src) => format!("{} {}", op.show(context), src.show(context)),
            Instruction::Binary(op, src, dst) => format!(
                "{:indent$}{} {}, {}",
                "",
                op.show(context),
                src.show(context),
                dst.show(context),
                indent = context.indent
            ),
            Instruction::AllocateStack(num) => {
                if context.comments {
                    format!(
                        "{:indent$}# Move the stack pointer by n bytes to the left.\n\
                            {:indent$}# This has the effect of allocating n bytes for\n\
                            {:indent$}# use in this function.\n\
                            {:indent$}# Variables are allocated relative to RBP, which is\n\
                            {:indent$}# where RSP was also located before this operation\n\
                            {:indent$}subq ${}, %rsp",
                        "",
                        "",
                        "",
                        "",
                        "",
                        "",
                        num,
                        indent = context.indent
                    )
                } else {
                    format!("{:indent$}subq ${}, %rsp", "", num, indent = context.indent)
                }
            }
            Instruction::Idiv(src) => {
                if context.comments {
                    format!(
                        "{:indent$}# Read the dividend from EDX and EAX,\n\
                            {:indent$}# and then store the quotient in EDX and\n\
                            {:indent$}# the remainder in EAX\n\
                            {:indent$}idivl {}",
                        "",
                        "",
                        "",
                        "",
                        src.show(context),
                        indent = context.indent
                    )
                } else {
                    format!(
                        "{:indent$}idivl {}",
                        "",
                        src.show(context),
                        indent = context.indent
                    )
                }
            }
            Instruction::Cdq => {
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
            Instruction::Cmp(left, right) => {
                format!(
                    "{:indent$}cmpl {}, {}",
                    "",
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
            Instruction::FreeStack(num) => {
                format!("{:indent$}addq ${}, %rsp", "", num, indent = context.indent)
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
                } else if context.is_linux && !context.types.get(s).unwrap().is_defined {
                    // if a function isn't defined in this file, call it from another file (ie
                    // externally link it)
                    s.to_string() + "@PLT"
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
                },
                _ => panic!("Invalid word length: {}", context.word_length_bytes),
            }
            .to_string(),
            Operand::MockReg(name) => panic!(
                "Tried to generate assembly code with a mock register: {}",
                name
            ),
            Operand::Stack(num) => format!("{}(%rbp)", num),
        }
    }
}

impl CodeDisplay for UnaryOperator {
    fn show(&self, _context: &mut DisplayContext) -> String {
        match self {
            UnaryOperator::Neg => "negl",
            UnaryOperator::Not => "notl",
        }
        .to_string()
    }
}

impl CodeDisplay for BinaryOperator {
    fn show(&self, _context: &mut DisplayContext) -> String {
        match self {
            BinaryOperator::Add => "addl",
            BinaryOperator::Sub => "subl",
            BinaryOperator::Mult => "imull",
            BinaryOperator::ShiftLeft => "sall",
            BinaryOperator::ShiftRight => "sarl",
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
