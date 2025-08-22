use std::{collections::VecDeque, error::Error};

use crate::compiler::{
    birds::{BirdsInstructionNode, BirdsValueNode},
    flow_graph::FlowGraph,
};

use super::{BirdsInstructionInfo, OptimizeContext};

impl FlowGraph<BirdsInstructionNode, BirdsInstructionInfo> {
    pub fn eliminate_dead_stores(
        &mut self,
        context: &mut OptimizeContext,
    ) -> Result<(), Box<dyn Error>> {
        self.find_dead_stores(context);

        let keys = self.indexes();
        for index in keys.iter() {
            let node = self.nodes.get_mut(index).unwrap();
            let mut new_instructions = VecDeque::new();
            for instruction in node.instructions.drain(..) {
                let info = instruction.1.clone();
                let updated_instruction = Self::replace_instruction_dead_stores(instruction);
                if let Some(i) = updated_instruction {
                    new_instructions.push_back((i, info));
                }
            }
            node.instructions = new_instructions;
        }

        Ok(())
    }

    fn replace_instruction_dead_stores(
        (instruction, info): (BirdsInstructionNode, BirdsInstructionInfo),
    ) -> Option<BirdsInstructionNode> {
        let live_variables = info.live_variables;
        match &instruction {
            BirdsInstructionNode::Unary(_, _, dst)
            | BirdsInstructionNode::SignedExtend(_, dst)
            | BirdsInstructionNode::Truncate(_, dst)
            | BirdsInstructionNode::ZeroExtend(_, dst)
            | BirdsInstructionNode::FloatToInt(_, dst)
            | BirdsInstructionNode::FloatToUint(_, dst)
            | BirdsInstructionNode::IntToFloat(_, dst)
            | BirdsInstructionNode::UintToFloat(_, dst)
            | BirdsInstructionNode::FloatToDouble(_, dst)
            | BirdsInstructionNode::DoubleToFloat(_, dst)
            | BirdsInstructionNode::GetAddress(_, dst)
            | BirdsInstructionNode::LoadFromPointer(_, dst)
            | BirdsInstructionNode::AddPointer(_, _, _, dst)
            | BirdsInstructionNode::CopyFromOffset(_, _, dst)
            | BirdsInstructionNode::Copy(_, dst)
            | BirdsInstructionNode::Binary(_, _, _, dst) => {
                if !live_variables.contains(dst) {
                    // println!("KILLING {:?}", instruction);
                    None
                } else {
                    Some(instruction)
                }
            }
            BirdsInstructionNode::CopyToOffset(_, name, _) => {
                if !live_variables.contains(&BirdsValueNode::Var(name.clone())) {
                    // println!("KILLING {:?}", instruction);
                    None
                } else {
                    Some(instruction)
                }
            }
            BirdsInstructionNode::FunctionCall(_, _, _)
            | BirdsInstructionNode::Return(_)
            | BirdsInstructionNode::Jump(_)
            | BirdsInstructionNode::JumpZero(_, _)
            | BirdsInstructionNode::JumpNotZero(_, _)
            | BirdsInstructionNode::JumpCondition(_, _, _, _)
            | BirdsInstructionNode::Label(_)
            | BirdsInstructionNode::StoreInPointer(_, _) => Some(instruction),
        }
    }

    fn transfer_dead_stores(
        &mut self,
        index: &usize,
        mut live_variables: Vec<BirdsValueNode>,
        context: &mut OptimizeContext,
    ) {
        let node = self.nodes.get_mut(index).unwrap();
        for instruction in node.instructions.iter_mut().rev() {
            instruction.1.live_variables = live_variables.clone();
            // any dst we encounter kills a live variable
            match &instruction.0 {
                BirdsInstructionNode::Unary(_, _, dst)
                | BirdsInstructionNode::SignedExtend(_, dst)
                | BirdsInstructionNode::Truncate(_, dst)
                | BirdsInstructionNode::ZeroExtend(_, dst)
                | BirdsInstructionNode::FloatToInt(_, dst)
                | BirdsInstructionNode::FloatToUint(_, dst)
                | BirdsInstructionNode::IntToFloat(_, dst)
                | BirdsInstructionNode::UintToFloat(_, dst)
                | BirdsInstructionNode::FloatToDouble(_, dst)
                | BirdsInstructionNode::DoubleToFloat(_, dst)
                | BirdsInstructionNode::GetAddress(_, dst)
                | BirdsInstructionNode::LoadFromPointer(_, dst)
                | BirdsInstructionNode::AddPointer(_, _, _, dst)
                | BirdsInstructionNode::CopyFromOffset(_, _, dst)
                | BirdsInstructionNode::Copy(_, dst)
                | BirdsInstructionNode::FunctionCall(_, _, Some(dst))
                | BirdsInstructionNode::Binary(_, _, _, dst) => {
                    for (i, value) in live_variables.clone().iter().enumerate().rev() {
                        if value == dst {
                            live_variables.remove(i);
                        }
                    }
                }
                // an address does not kill a variable
                BirdsInstructionNode::StoreInPointer(_, _dst_pointer) => {}
                // nor does copying to part of an array, eg.
                BirdsInstructionNode::CopyToOffset(_, _, _) => {}
                _ => {}
            }
            match &instruction.0 {
                BirdsInstructionNode::Copy(src, _)
                | BirdsInstructionNode::Unary(_, src, _)
                | BirdsInstructionNode::Return(Some(src))
                | BirdsInstructionNode::JumpZero(src, _)
                | BirdsInstructionNode::JumpNotZero(src, _)
                | BirdsInstructionNode::Truncate(src, _)
                | BirdsInstructionNode::SignedExtend(src, _)
                | BirdsInstructionNode::ZeroExtend(src, _)
                | BirdsInstructionNode::FloatToInt(src, _)
                | BirdsInstructionNode::FloatToUint(src, _)
                | BirdsInstructionNode::IntToFloat(src, _)
                | BirdsInstructionNode::UintToFloat(src, _)
                | BirdsInstructionNode::FloatToDouble(src,_)
                | BirdsInstructionNode::DoubleToFloat(src,_)
                | BirdsInstructionNode::CopyToOffset(src, _, _) => {
                    if matches!(src, BirdsValueNode::Var(_)) && !live_variables.contains(src) {
                        live_variables.push(src.clone())
                    }
                }
                BirdsInstructionNode::StoreInPointer(src, dst)
                | BirdsInstructionNode::LoadFromPointer(src, dst) => {
                    if matches!(src, BirdsValueNode::Var(_)) && !live_variables.contains(src) {
                        live_variables.push(src.clone())
                    }
                    if matches!(dst, BirdsValueNode::Var(_)) && !live_variables.contains(dst) {
                        live_variables.push(dst.clone())
                    }
                    for v in context.aliased_variables.clone() {
                        if !live_variables.contains(&v) {
                            live_variables.push(v)
                        }
                    }
                    // for v in context.static_variables.clone() {
                    //     if !live_variables.contains(&v) {
                    //         live_variables.push(v)
                    //     }
                    // }
                }
                BirdsInstructionNode::Binary(_, left, right, _)
                |BirdsInstructionNode::JumpCondition(_, left, right, _)
                | BirdsInstructionNode::AddPointer(left, right, _, _) => {
                    if matches!(left, BirdsValueNode::Var(_)) && !live_variables.contains(left) {
                        live_variables.push(left.clone())
                    }
                    if matches!(right, BirdsValueNode::Var(_)) && !live_variables.contains(right) {
                        live_variables.push(right.clone())
                    }
                }
                BirdsInstructionNode::FunctionCall(_, args, _) => {
                    for arg in args.iter() {
                        if matches!(arg, BirdsValueNode::Var(_)) && !live_variables.contains(arg) {
                            live_variables.push(arg.clone())
                        }
                    }
                    for v in context.aliased_variables.clone() {
                        if !live_variables.contains(&v) {
                            live_variables.push(v)
                        }
                    }
                    for v in context.static_variables.clone() {
                        if !live_variables.contains(&v) {
                            live_variables.push(v)
                        }
                    }
                }
                BirdsInstructionNode::CopyFromOffset(name, _, _) => {
                    if !live_variables.contains(&BirdsValueNode::Var(name.clone())) {
                        live_variables.push(BirdsValueNode::Var(name.clone()))
                    }
                }
                BirdsInstructionNode::Jump(_)
                // GetAddress doesn't gen because getting the address of a register doesn't Use the
                // value stored inside it
                | BirdsInstructionNode::GetAddress(_, _)
                | BirdsInstructionNode::Return(None)
                | BirdsInstructionNode::Label(_) => {}
            }
        }
        context.block_live_variables.insert(*index, live_variables);
    }

    // gets the intersection of every incoming reaching copy to send to the next block (ie the
    // input of transfer())
    fn meet_dead_stores(
        &self,
        index: &usize,
        context: &mut OptimizeContext,
    ) -> Vec<BirdsValueNode> {
        let mut live_variables = Vec::new();
        let node = self.nodes.get(index).unwrap().clone();
        for after_index in node.afters.iter() {
            for i in context
                .block_live_variables
                .get(after_index)
                .unwrap()
                .iter()
            {
                if !live_variables.contains(i) {
                    live_variables.push(i.clone())
                }
            }
        }
        live_variables
    }

    fn find_dead_stores(&mut self, context: &mut OptimizeContext) {
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
                context
                    .block_live_variables
                    .insert(*index, context.static_variables.clone());
                continue;
            }
            working_list.push_back(*index);
            context.block_live_variables.insert(*index, Vec::new());
        }

        while !working_list.is_empty() {
            let index = working_list.pop_front().unwrap();

            let previous_reaching = context.block_live_variables.get(&index).unwrap().clone();

            let incoming = self.meet_dead_stores(&index, context);
            self.transfer_dead_stores(&index, incoming, context);

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
