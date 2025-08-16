use std::{collections::HashSet, error::Error};

use itertools::Itertools;

use crate::compiler::{birds::BirdsInstructionNode, flow_graph::FlowGraph};

use super::{BirdsInstructionInfo, OptimizeContext};

impl FlowGraph<BirdsInstructionNode, BirdsInstructionInfo> {
    pub fn eliminate_dead_code(
        &mut self,
        _context: &mut OptimizeContext,
    ) -> Result<(), Box<dyn Error>> {
        let mut marked = HashSet::new();

        self.traverse(0, &mut marked);
        for (k, _) in self.nodes.clone().iter() {
            if !marked.contains(k) {
                self.remove_node(*k)
            }
        }

        let keys: Vec<usize> = self.nodes.keys().sorted().copied().collect();
        let keys_len = keys.len();

        // CLEAR USELESS JUMPS
        // ignore the second last key, since a jump at the end of a function is never redundant
        for (index, next_index) in keys.iter().take(keys_len - 1).tuple_windows() {
            if let Some((
                BirdsInstructionNode::Jump(_)
                | BirdsInstructionNode::JumpZero(_, _)
                | BirdsInstructionNode::JumpNotZero(_, _),
                _,
            )) = self.nodes.get(index).unwrap().instructions.back()
            {
                // skip the last block, shouldn't delete the last jump in a function
                if !self
                    .nodes
                    .get(index)
                    .unwrap()
                    .afters
                    .iter()
                    .any(|after| after != next_index)
                {
                    // pop the jump instruction off the end of this node
                    self.nodes.get_mut(index).unwrap().instructions.pop_back();
                }
            }
        }

        // CLEAR USELESS LABELS
        for (index, next_index) in keys.iter().tuple_windows() {
            if let Some((BirdsInstructionNode::Label(_), _)) =
                self.nodes.get(next_index).unwrap().instructions.front()
            {
                if !self
                    .nodes
                    .get(next_index)
                    .unwrap()
                    .befores
                    .iter()
                    .any(|before| before != index)
                {
                    // pop the label off the front of this block
                    self.nodes
                        .get_mut(next_index)
                        .unwrap()
                        .instructions
                        .pop_front();
                }
            }
        }

        // DELETE EMPTY BLOCKS
        for index in keys.iter() {
            if self.nodes.get(index).unwrap().instructions.is_empty() {
                self.remove_node(*index);
            }
        }

        Ok(())
    }
}
