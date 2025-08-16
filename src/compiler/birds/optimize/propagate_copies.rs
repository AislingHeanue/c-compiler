use std::error::Error;

use crate::compiler::{
    birds::{BirdsInstructionNode, BirdsValueNode},
    flow_graph::{FlowGraph, FlowNode},
    types::StorageInfo,
};

use super::{BirdsInstructionInfo, OptimizeContext};

impl FlowGraph<BirdsInstructionNode, BirdsInstructionInfo> {
    pub fn propagate_copies(
        &mut self,
        _context: &mut OptimizeContext,
    ) -> Result<(), Box<dyn Error>> {
        Ok(())
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

                    reaching_copies.push((src.clone(), dst.clone()));
                }
                BirdsInstructionNode::FunctionCall(_, _, maybe_dst) => {
                    for (i, copy) in reaching_copies.clone().iter().enumerate().rev() {
                        if let Some(ref dst) = maybe_dst {
                            if copy.0 == *dst || copy.1 == *dst {
                                reaching_copies.remove(i);
                                continue;
                            }
                        }
                        if let BirdsValueNode::Var(src_name) = &copy.0 {
                            if let StorageInfo::Static(_, _) =
                                context.symbols.get(src_name).unwrap().storage
                            {
                                reaching_copies.remove(i);
                                continue;
                            }
                        }
                        if let BirdsValueNode::Var(dst_name) = &copy.1 {
                            if let StorageInfo::Static(_, _) =
                                context.symbols.get(dst_name).unwrap().storage
                            {
                                reaching_copies.remove(i);
                                continue;
                            }
                        }
                    }
                }
                BirdsInstructionNode::Unary(_, _, dst)
                | BirdsInstructionNode::Binary(_, _, _, dst) => {
                    for (i, copy) in reaching_copies.clone().iter().enumerate().rev() {
                        if copy.0 == *dst || copy.1 == *dst {
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
        &mut self,
        index: &usize,
        mut reaching_copies: Vec<(BirdsValueNode, BirdsValueNode)>,
        context: &mut OptimizeContext,
    ) -> Vec<(BirdsValueNode, BirdsValueNode)> {
        let node = self.nodes.get_mut(index).unwrap().clone();
        for before_index in node.befores.iter() {
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
}
