use std::collections::{HashMap, HashSet, VecDeque};

use crate::compiler::{
    codegen::{AssemblySymbolInfo, Instruction, Operand},
    flow_graph::{FlowGraph, FlowNode, InstructionMatching},
};

use super::ValidateContext;

#[derive(Debug)]
pub struct InstructionInfo {
    pub live_registers: HashSet<Operand>,
}

impl InstructionInfo {
    fn new() -> InstructionInfo {
        InstructionInfo {
            live_registers: HashSet::new(),
        }
    }
}

impl InstructionMatching<Instruction, InstructionInfo> for FlowGraph<Instruction, InstructionInfo> {
    fn match_instruction_for_new(
        instruction: Instruction,
        mut current_node: FlowNode<Instruction, InstructionInfo>,
        nodes: &mut HashMap<usize, FlowNode<Instruction, InstructionInfo>>,
        label_block_locations: &mut HashMap<String, usize>,
    ) -> FlowNode<Instruction, InstructionInfo> {
        match instruction {
            Instruction::Label(ref label) => {
                if !current_node.instructions.is_empty() {
                    nodes.insert(nodes.len(), current_node);
                    current_node = FlowNode::new();
                }
                label_block_locations.insert(label.clone(), nodes.len());
                current_node
                    .instructions
                    .push_back((instruction, InstructionInfo::new()));
            }
            Instruction::Jmp(_) | Instruction::JmpCondition(_, _, _) | Instruction::Ret => {
                current_node
                    .instructions
                    .push_back((instruction, InstructionInfo::new()));
                nodes.insert(nodes.len(), current_node);
                current_node = FlowNode::new();
            }
            _ => current_node
                .instructions
                .push_back((instruction, InstructionInfo::new())),
        }
        current_node
    }

    fn match_instruction_for_edge(
        &mut self,
        index: &usize,
        next_index: &usize,
        last_index: &usize,
    ) {
        let instruction = &self
            .nodes
            .get(index)
            .unwrap()
            .instructions
            .back()
            .unwrap()
            .0;
        match instruction {
            Instruction::Ret => self.add_edge(index, last_index),
            Instruction::Jmp(label) => {
                let other_index = *self.label_block_locations.get(label).unwrap();
                self.add_edge(index, &other_index);
            }
            Instruction::JmpCondition(_, label, _) => {
                let other_index = *self.label_block_locations.get(label).unwrap();
                self.add_edge(index, &other_index);
                self.add_edge(index, next_index);
            }
            _ => {
                self.add_edge(index, next_index);
            }
        }
    }
}

impl FlowGraph<Instruction, InstructionInfo> {
    fn transfer_registers(
        &mut self,
        index: &usize,
        mut live_registers: HashSet<Operand>,
        context: &mut ValidateContext,
    ) {
        let node = self.nodes.get_mut(index).unwrap();
        for instruction in node.instructions.iter_mut().rev() {
            instruction.1.live_registers = live_registers.clone();
            let (used, updated) = instruction.0.get_used_and_updated_operands(context);
            for op in updated {
                // MockMemory and Memory are already dealt with in convert_uses_and_updates
                if matches!(op, Operand::Reg(_) | Operand::MockReg(_)) {
                    live_registers.remove(&op);
                }
            }
            for op in used {
                if matches!(op, Operand::Reg(_) | Operand::MockReg(_)) {
                    live_registers.insert(op);
                }
            }
        }
        context.block_live_variables.insert(*index, live_registers);
    }

    fn meet_registers(&self, index: &usize, context: &mut ValidateContext) -> HashSet<Operand> {
        let mut live_registers = HashSet::new();
        let node = self.nodes.get(index).unwrap();
        for after_index in node.afters.iter() {
            for i in context
                .block_live_variables
                .get(after_index)
                .unwrap()
                .iter()
            {
                live_registers.insert(i.clone());
            }
        }
        live_registers
    }
    pub fn check_liveness(&mut self, context: &mut ValidateContext) {
        // post-order claims to be best here because it means you evaluate all of a nodes
        // 'afters' before analysing the node itself, minimizing the amount of revisits to this
        // node in this function
        let keys = self.post_order_indexes();

        let mut working_list = VecDeque::new();
        for index in keys.iter() {
            if *index == 0 {
                continue;
            }
            if *index == self.last_index {
                let function_info = context
                    .symbols
                    .get(context.current_function_name.as_ref().unwrap())
                    .unwrap();
                if let AssemblySymbolInfo::Function(_, _, _, return_registers, _) = function_info {
                    let return_operands = return_registers
                        .iter()
                        .map(|r| Operand::Reg(r.clone()))
                        .collect();
                    context.block_live_variables.insert(*index, return_operands);
                }
                continue;
            }
            working_list.push_back(*index);
            context.block_live_variables.insert(*index, HashSet::new());
        }

        while !working_list.is_empty() {
            let index = working_list.pop_front().unwrap();

            let previous_reaching = context.block_live_variables.get(&index).unwrap().clone();

            let incoming = self.meet_registers(&index, context);
            self.transfer_registers(&index, incoming, context);

            if previous_reaching != *context.block_live_variables.get(&index).unwrap() {
                let node = self.nodes.get(&index).unwrap();
                for before_index in node.befores.iter() {
                    if *before_index == 0
                        // this condition indicates the node isn't reachable, so ignore it
                        || !context.block_live_variables.contains_key(before_index)
                    {
                        continue;
                    }
                    // yes this also counts self-loops
                    if !working_list.contains(before_index) {
                        working_list.push_back(*before_index);
                    }
                }
            }
        }
    }
}
