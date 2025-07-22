use super::{
    BinaryOperator, CodeDisplay, ConditionCode, DisplayContext, ExtraStrings, Function,
    Instruction, Instructions, Operand, Program, Register, UnaryOperator,
};
use std::fmt::Display;

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let context = &mut self.displaying_context.clone();
        write!(
            f,
            "{}\n{}\n",
            self.function.show(context),
            self.footer.show(&mut context.indent())
        )
    }
}

impl CodeDisplay for Function {
    fn show(&self, context: &mut DisplayContext) -> String {
        let function_name = if context.is_mac {
            "_".to_string() + &self.name
        } else {
            self.name.clone()
        };

        format!(
            "{}\n{:indent$}{}:\n{}",
            self.header.show(&mut context.indent()),
            "",
            function_name,
            self.instructions.show(&mut context.indent()),
            indent = context.indent
        )
    }
}

impl CodeDisplay for ExtraStrings {
    fn show(&self, context: &mut DisplayContext) -> String {
        let mut out = format!("{:indent$}", "", indent = context.indent);
        out += &self
            .0
            .join(format!("\n{:indent$}", "", indent = context.indent).as_str())
            .to_string();

        out
    }
}

impl CodeDisplay for Instructions {
    fn show(&self, context: &mut DisplayContext) -> String {
        self.0
            .iter()
            .map(|instruction| instruction.show(context))
            .collect::<Vec<String>>()
            .join("\n")
            .to_string()
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
                format!(
                    "{:indent$}set{} {}",
                    "",
                    c.show(context),
                    dst.show(&mut context.short()),
                    indent = context.indent,
                )
            }
            Instruction::Label(s) => {
                let context = context.unindent();
                let label_start = if context.is_mac { "L" } else { ".L" };
                format!(
                    "{:indent$}{}{}:",
                    "",
                    label_start,
                    s,
                    indent = context.indent,
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
            Operand::Reg(reg) => if context.word_length_bytes == 1 {
                match reg {
                    Register::AX => "%al",
                    Register::R10 => "%r10b",
                    Register::DX => "%dl",
                    Register::R11 => "%r11b",
                }
            } else {
                match reg {
                    Register::AX => "%eax",
                    Register::R10 => "%r10d",
                    Register::DX => "%edx",
                    Register::R11 => "%r11d",
                }
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
