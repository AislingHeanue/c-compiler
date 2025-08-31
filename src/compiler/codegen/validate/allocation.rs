use std::{collections::HashMap, error::Error};

use crate::compiler::codegen::{Instruction, Operand};

use super::{
    interference_graph::{true_register_value, InterferenceGraph},
    ValidateContext,
};

pub trait AllocateRegisters {
    fn allocate_registers(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>>;

    fn allocate_registers_for_double(
        &mut self,
        context: &mut ValidateContext,
    ) -> Result<(), Box<dyn Error>>;

    fn rewrite_coalesced(&mut self, coalesced_registers: HashMap<Operand, Operand>);
    fn replace_mock_registers_with_map(&mut self, context: &mut ValidateContext);
}

impl AllocateRegisters for Vec<Instruction> {
    fn allocate_registers(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        let mut interference_graph: InterferenceGraph;
        loop {
            interference_graph = InterferenceGraph::new(self, false, context);
            let coalesced_registers = interference_graph.coalesce(self);
            if coalesced_registers.is_empty() {
                break;
            }
            self.rewrite_coalesced(coalesced_registers);
        }
        interference_graph.colour_graph();
        interference_graph.get_register_map(context);
        self.replace_mock_registers_with_map(context);
        Ok(())
    }

    fn allocate_registers_for_double(
        &mut self,
        context: &mut ValidateContext,
    ) -> Result<(), Box<dyn Error>> {
        let mut interference_graph;
        loop {
            interference_graph = InterferenceGraph::new(self, true, context);
            let coalesced_registers = interference_graph.coalesce(self);
            if coalesced_registers.is_empty() {
                break;
            }
            self.rewrite_coalesced(coalesced_registers);
        }
        interference_graph.colour_graph();
        interference_graph.get_register_map(context);
        self.replace_mock_registers_with_map(context);
        Ok(())
    }

    fn replace_mock_registers_with_map(&mut self, context: &mut ValidateContext) {
        let mut out = Vec::new();
        for mut instruction in self.drain(..) {
            match instruction {
                Instruction::Mov(_, ref mut src, ref mut dst) => {
                    src.replace_mock_register_with_map(context);
                    dst.replace_mock_register_with_map(context);
                }
                Instruction::Movsx(_, _, ref mut src, ref mut dst) => {
                    src.replace_mock_register_with_map(context);
                    dst.replace_mock_register_with_map(context);
                }
                Instruction::MovZeroExtend(_, _, ref mut src, ref mut dst) => {
                    src.replace_mock_register_with_map(context);
                    dst.replace_mock_register_with_map(context);
                }
                Instruction::FloatToInt(_, _, ref mut src, ref mut dst) => {
                    src.replace_mock_register_with_map(context);
                    dst.replace_mock_register_with_map(context);
                }
                Instruction::IntToFloat(_, _, ref mut src, ref mut dst) => {
                    src.replace_mock_register_with_map(context);
                    dst.replace_mock_register_with_map(context);
                }
                Instruction::Cvtss2sd(ref mut src, ref mut dst) => {
                    src.replace_mock_register_with_map(context);
                    dst.replace_mock_register_with_map(context);
                }
                Instruction::Cvtsd2ss(ref mut src, ref mut dst) => {
                    src.replace_mock_register_with_map(context);
                    dst.replace_mock_register_with_map(context);
                }
                Instruction::Unary(_, _, ref mut dst) => {
                    dst.replace_mock_register_with_map(context);
                }
                Instruction::Binary(_, _, ref mut src, ref mut dst) => {
                    src.replace_mock_register_with_map(context);
                    dst.replace_mock_register_with_map(context);
                }
                Instruction::Idiv(_, ref mut src) => {
                    src.replace_mock_register_with_map(context);
                }
                Instruction::Div(_, ref mut src) => {
                    src.replace_mock_register_with_map(context);
                }
                Instruction::Cmp(_, ref mut src, ref mut dst) => {
                    src.replace_mock_register_with_map(context);
                    dst.replace_mock_register_with_map(context);
                }
                Instruction::Cdq(_) => {}
                Instruction::Jmp(_) => {}
                Instruction::JmpCondition(_, _, _) => {}
                Instruction::SetCondition(_, ref mut dst, _) => {
                    dst.replace_mock_register_with_map(context);
                }
                Instruction::Label(_) => {}
                Instruction::Ret => {}
                Instruction::Push(ref mut src) => {
                    src.replace_mock_register_with_map(context);
                }
                Instruction::Pop(_) => {} // pop only points to real registers
                Instruction::Call(_) => {}
                Instruction::CallIndirect(ref mut src) => {
                    src.replace_mock_register_with_map(context);
                }
                Instruction::Lea(ref mut src, ref mut dst) => {
                    src.replace_mock_register_with_map(context);
                    dst.replace_mock_register_with_map(context);
                }
                Instruction::VaStart(ref mut dst) => {
                    dst.replace_mock_register_with_map(context);
                }
            }
            let delete_this_instruction = if let Instruction::Mov(_, ref src, ref dst) = instruction
            {
                src == dst
            } else {
                false
            };
            if !delete_this_instruction {
                out.push(instruction);
            }
        }
        *self = out;
    }

    fn rewrite_coalesced(&mut self, coalesced_registers: HashMap<Operand, Operand>) {
        let mut instructions = Vec::new();
        for mut instruction in self.drain(..) {
            match instruction {
                Instruction::Mov(_, ref mut src, ref mut dst) => {
                    *src = true_register_value(src, &coalesced_registers);
                    *dst = true_register_value(dst, &coalesced_registers);
                    if src == dst {
                        continue;
                    }
                }
                Instruction::Movsx(_, _, ref mut src, ref mut dst)
                | Instruction::MovZeroExtend(_, _, ref mut src, ref mut dst)
                | Instruction::Lea(ref mut src, ref mut dst)
                | Instruction::FloatToInt(_, _, ref mut src, ref mut dst)
                | Instruction::IntToFloat(_, _, ref mut src, ref mut dst)
                | Instruction::Cvtsd2ss(ref mut src, ref mut dst)
                | Instruction::Cvtss2sd(ref mut src, ref mut dst)
                | Instruction::Binary(_, _, ref mut src, ref mut dst)
                | Instruction::Cmp(_, ref mut src, ref mut dst) => {
                    *src = true_register_value(src, &coalesced_registers);
                    *dst = true_register_value(dst, &coalesced_registers);
                }
                Instruction::Unary(_, _, ref mut src)
                | Instruction::Idiv(_, ref mut src)
                | Instruction::Div(_, ref mut src)
                | Instruction::SetCondition(_, ref mut src, _)
                | Instruction::CallIndirect(ref mut src)
                | Instruction::Push(ref mut src) => {
                    *src = true_register_value(src, &coalesced_registers);
                }
                Instruction::Pop(ref mut r) => {
                    if let Operand::Reg(new_r) =
                        true_register_value(&Operand::Reg(r.clone()), &coalesced_registers)
                    {
                        *r = new_r;
                    } else {
                        // registers cannot be coalesced into non-registers ever
                        unreachable!()
                    }
                }
                Instruction::VaStart(ref mut dst) => {
                    *dst = true_register_value(dst, &coalesced_registers);
                }
                Instruction::Cdq(_)
                | Instruction::Jmp(_)
                | Instruction::JmpCondition(_, _, _)
                | Instruction::Label(_)
                | Instruction::Call(_)
                | Instruction::Ret => {}
            }
            instructions.push(instruction);
        }
        *self = instructions;
    }
}
