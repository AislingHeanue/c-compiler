use std::{collections::VecDeque, error::Error};

use crate::compiler::{
    birds::{BirdsInstructionNode, BirdsValueNode},
    flow_graph::FlowGraph,
};

use super::{BirdsInstructionInfo, OptimizeContext};

impl FlowGraph<BirdsInstructionNode, BirdsInstructionInfo> {
    pub fn propagate_copies(
        &mut self,
        context: &mut OptimizeContext,
    ) -> Result<(), Box<dyn Error>> {
        self.find_reaching_copies(context);

        let keys = self.indexes();
        for index in keys.iter() {
            let node = self.nodes.get_mut(index).unwrap();
            let mut new_instructions = VecDeque::new();
            for instruction in node.instructions.drain(..) {
                let info = instruction.1.clone();
                let updated_instruction = Self::replace_instruction(instruction);
                if let Some(i) = updated_instruction {
                    new_instructions.push_back((i, info));
                }
            }
            node.instructions = new_instructions;
        }

        Ok(())
    }

    fn replace_instruction(
        (mut instruction, info): (BirdsInstructionNode, BirdsInstructionInfo),
    ) -> Option<BirdsInstructionNode> {
        let reaching_copies = info.reaching_copies;
        match instruction {
            BirdsInstructionNode::Copy(ref mut src, ref dst) => {
                for copy in reaching_copies.iter() {
                    if (copy.0 == *src && copy.1 == *dst) || (copy.1 == *src && copy.0 == *dst) {
                        // println!("KILLING (propagate copies) {:?}", instruction);
                        return None;
                    }
                }
                Self::replace_value(src, &reaching_copies);
                Some(instruction)
            }
            BirdsInstructionNode::Unary(_, ref mut src, _)
            | BirdsInstructionNode::Return(Some(ref mut src))
            | BirdsInstructionNode::JumpZero(ref mut src, _)
            | BirdsInstructionNode::JumpNotZero(ref mut src, _)
            | BirdsInstructionNode::Truncate(ref mut src, _)
            | BirdsInstructionNode::SignedExtend(ref mut src, _)
            | BirdsInstructionNode::ZeroExtend(ref mut src, _)
            | BirdsInstructionNode::FloatToInt(ref mut src, _)
            | BirdsInstructionNode::FloatToUint(ref mut src, _)
            | BirdsInstructionNode::IntToFloat(ref mut src, _)
            | BirdsInstructionNode::UintToFloat(ref mut src, _)
            | BirdsInstructionNode::FloatToDouble(ref mut src, _)
            | BirdsInstructionNode::DoubleToFloat(ref mut src, _)
            // | BirdsInstructionNode::FloatToInt(ref mut src, _)
            // | BirdsInstructionNode::FloatToUint(ref mut src, _)
            // | BirdsInstructionNode::UintToFloat(ref mut src, _)
            // | BirdsInstructionNode::IntToFloat(ref mut src, _)
            | BirdsInstructionNode::CopyToOffset(ref mut src, _, _)
            | BirdsInstructionNode::LoadFromPointer(ref mut src, _)
            | BirdsInstructionNode::StoreInPointer(ref mut src, _) => {
                Self::replace_value(src, &reaching_copies);
                Some(instruction)
            }
            BirdsInstructionNode::Binary(_, ref mut left, ref mut right, _)
            |BirdsInstructionNode::JumpCondition(_, ref mut left, ref mut right, _)
            | BirdsInstructionNode::AddPointer(ref mut left, ref mut right, _, _) => {
                Self::replace_value(left, &reaching_copies);
                Self::replace_value(right, &reaching_copies);
                Some(instruction)
            }
            BirdsInstructionNode::FunctionCall(_, ref mut args, _) => {
                for arg in args.iter_mut() {
                    Self::replace_value(arg, &reaching_copies);
                }
                Some(instruction)
            }
            BirdsInstructionNode::CopyFromOffset(ref mut name, _, _) => {
                for copy in reaching_copies.iter() {
                    if let (BirdsValueNode::Var(src_name), BirdsValueNode::Var(dst_name)) = copy {
                        if dst_name == name {
                            *name = src_name.clone();
                            break;
                        }
                    }
                }
                Some(instruction)
            }
            BirdsInstructionNode::Jump(_)
            | BirdsInstructionNode::Return(None)
            | BirdsInstructionNode::Label(_)
            // GetAddress is never propagated to because it returns the sources, address, not its
            // pointer
            | BirdsInstructionNode::GetAddress(_,_)=> Some(instruction),
        }
    }

    fn replace_value(
        value: &mut BirdsValueNode,
        reaching_copies: &[(BirdsValueNode, BirdsValueNode)],
    ) {
        if matches!(value, BirdsValueNode::Constant(_)) {
            return;
        }

        for copy in reaching_copies.iter() {
            if copy.1 == *value {
                *value = copy.0.clone();
                return;
            }
        }
    }

    fn transfer(
        &mut self,
        index: &usize,
        mut reaching_copies: Vec<(BirdsValueNode, BirdsValueNode)>,
        context: &mut OptimizeContext,
    ) {
        let node = self.nodes.get_mut(index).unwrap();
        for instruction in node.instructions.iter_mut() {
            instruction.1.reaching_copies = reaching_copies.clone();
            match &instruction.0 {
                BirdsInstructionNode::Copy(src, dst) => {
                    // x = y followed by y = x. Ignore the second one.
                    if reaching_copies.contains(&(dst.clone(), src.clone())) {
                        continue;
                    }

                    for (i, copy) in reaching_copies.clone().iter().enumerate().rev() {
                        if copy.0 == *dst || copy.1 == *dst {
                            reaching_copies.remove(i);
                        }
                    }

                    let src_type = src.get_type(&context.symbols);
                    let dst_type = dst.get_type(&context.symbols);
                    if src_type == dst_type
                        || (src_type.is_signed() && dst_type.is_signed())
                        || (!src_type.is_signed() && !dst_type.is_signed())
                    {
                        reaching_copies.push((src.clone(), dst.clone()));
                    }
                }
                BirdsInstructionNode::FunctionCall(_, _, maybe_dst) => {
                    for (i, copy) in reaching_copies.clone().iter().enumerate().rev() {
                        if let Some(ref dst) = maybe_dst {
                            if copy.0 == *dst || copy.1 == *dst {
                                reaching_copies.remove(i);
                                continue;
                            }
                        }
                        if context.aliased_variables.contains(&copy.0)
                            || context.aliased_variables.contains(&copy.1)
                            || context.static_variables.contains(&copy.0)
                            || context.static_variables.contains(&copy.1)
                        {
                            reaching_copies.remove(i);
                            continue;
                        }
                    }
                }
                BirdsInstructionNode::StoreInPointer(_, _) => {
                    for (i, copy) in reaching_copies.clone().iter().enumerate().rev() {
                        if context.aliased_variables.contains(&copy.0)
                            || context.aliased_variables.contains(&copy.1)
                            || context.static_variables.contains(&copy.0)
                            || context.static_variables.contains(&copy.1)
                        {
                            reaching_copies.remove(i);
                        }
                    }
                }
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
                // | BirdsInstructionNode::FloatToInt(_, dst)
                // | BirdsInstructionNode::FloatToUint(_, dst)
                // | BirdsInstructionNode::UintToFloat(_, dst)
                // | BirdsInstructionNode::IntToFloat(_, dst)
                | BirdsInstructionNode::GetAddress(_, dst)
                | BirdsInstructionNode::LoadFromPointer(_, dst)
                | BirdsInstructionNode::AddPointer(_, _, _, dst)
                | BirdsInstructionNode::CopyFromOffset(_, _, dst)
                | BirdsInstructionNode::Binary(_, _, _, dst) => {
                    for (i, copy) in reaching_copies.clone().iter().enumerate().rev() {
                        if copy.0 == *dst || copy.1 == *dst {
                            reaching_copies.remove(i);
                        }
                    }
                }
                BirdsInstructionNode::CopyToOffset(_, name, _) => {
                    let dst = BirdsValueNode::Var(name.clone());
                    for (i, copy) in reaching_copies.clone().iter().enumerate().rev() {
                        if copy.0 == dst || copy.1 == dst {
                            reaching_copies.remove(i);
                        }
                    }
                }
                _ => {}
            }
        }
        context
            .block_reaching_copies
            .insert(*index, reaching_copies);
    }

    // gets the intersection of every incoming reaching copy to send to the next block (ie the
    // input of transfer())
    fn meet(
        &self,
        index: &usize,
        mut reaching_copies: Vec<(BirdsValueNode, BirdsValueNode)>,
        context: &mut OptimizeContext,
    ) -> Vec<(BirdsValueNode, BirdsValueNode)> {
        let node = self.nodes.get(index).unwrap().clone();
        for before_index in node.befores.iter() {
            if !context.block_reaching_copies.contains_key(before_index) {
                // if this block was never encountered in the post-order search, it is not
                // reachable, therefore ignore it here.
                continue;
            }

            // ban any reaching_copies which aren't present in ALL incoming branches
            reaching_copies.retain(|copy| {
                context
                    .block_reaching_copies
                    .get(before_index)
                    .unwrap()
                    .contains(copy)
            });
        }
        reaching_copies
    }

    fn get_all_copies(&self) -> Vec<(BirdsValueNode, BirdsValueNode)> {
        self.indexes()
            .iter()
            .flat_map(|index| {
                self.nodes
                    .get(index)
                    .unwrap()
                    .instructions
                    .iter()
                    .filter_map(|instruction| match &instruction.0 {
                        BirdsInstructionNode::Copy(src, dst) => Some((src.clone(), dst.clone())),
                        _ => None,
                    })
            })
            .collect()
    }

    fn find_reaching_copies(&mut self, context: &mut OptimizeContext) {
        let all_copies = self.get_all_copies();
        // reverse post-order claims to be best here because it means you evaluate all of a nodes
        // 'befores' before analysing the node itself, minimizing the amount of revisits to this
        // node in this function
        let mut keys = self.post_order_indexes();
        keys.reverse();

        let mut working_list = VecDeque::new();
        for index in keys.iter() {
            if *index == 0 || *index == self.last_index {
                // entry and exit nodes have no reaching copies
                context.block_reaching_copies.insert(*index, Vec::new());
                continue;
            }
            working_list.push_back(*index);
            context
                .block_reaching_copies
                .insert(*index, all_copies.clone());
        }

        while !working_list.is_empty() {
            let index = working_list.pop_front().unwrap();

            let previous_reaching = context.block_reaching_copies.get(&index).unwrap().clone();

            let incoming = self.meet(&index, all_copies.clone(), context);
            self.transfer(&index, incoming, context);

            if previous_reaching != *context.block_reaching_copies.get(&index).unwrap() {
                let node = self.nodes.get(&index).unwrap();
                for after_index in node.afters.iter() {
                    if *after_index == self.last_index {
                        continue;
                    }
                    // yes this also counts self-loops
                    if !working_list.contains(after_index) {
                        working_list.push_back(*after_index);
                    }
                }
            }
        }
    }
}
