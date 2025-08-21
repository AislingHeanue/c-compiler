use std::error::Error;

use crate::compiler::codegen::Instruction;

use super::{interference_graph::InterferenceGraph, ValidateContext};

pub trait AllocateRegisters {
    fn allocate_registers(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>>;

    fn allocate_registers_for_double(
        &mut self,
        context: &mut ValidateContext,
    ) -> Result<(), Box<dyn Error>>;

    fn rewrite_coalesced(&mut self) {}
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
            self.rewrite_coalesced();
        }
        // println!("aliased {:?}", context.aliased_variables);
        // println!("BEGIN INTEGER");
        // for instruction in self.iter() {
        //     println!("{:?}", instruction);
        // }
        interference_graph.colour_graph();
        // for node in interference_graph.nodes.iter() {
        //     if let Operand::MockReg(name) = node.0 {
        //         if name.contains(".") {
        //             println!("{:?} {:?}", name, node.1.spill_cost);
        //         }
        //     }
        // }
        interference_graph.get_register_map(context);
        self.replace_mock_registers_with_map(context);
        // println!("END INTEGER");
        // for instruction in self.iter() {
        //     println!("{:?}", instruction);
        // }
        Ok(())
    }

    fn allocate_registers_for_double(
        &mut self,
        context: &mut ValidateContext,
    ) -> Result<(), Box<dyn Error>> {
        let mut interference_graph = InterferenceGraph::new(self, true, context);
        // println!("BEGIN DOUBLE");
        // for instruction in self.iter() {
        //     println!("{:?}", instruction);
        // }
        interference_graph.colour_graph();
        interference_graph.get_register_map(context);
        self.replace_mock_registers_with_map(context);
        // println!("END DOUBLE");
        // for instruction in self.iter() {
        //     println!("{:?}", instruction);
        // }
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
                Instruction::Cvttsd2si(_, ref mut src, ref mut dst) => {
                    src.replace_mock_register_with_map(context);
                    dst.replace_mock_register_with_map(context);
                }
                Instruction::Cvtsi2sd(_, ref mut src, ref mut dst) => {
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
                Instruction::Lea(ref mut src, ref mut dst) => {
                    src.replace_mock_register_with_map(context);
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

    fn rewrite_coalesced(&mut self) {
        todo!()
    }
}
