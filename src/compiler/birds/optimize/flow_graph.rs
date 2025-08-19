use std::collections::HashMap;

use crate::compiler::{
    birds::BirdsInstructionNode,
    flow_graph::{FlowGraph, FlowNode, InstructionMatching},
};

use super::BirdsInstructionInfo;

impl InstructionMatching<BirdsInstructionNode, BirdsInstructionInfo>
    for FlowGraph<BirdsInstructionNode, BirdsInstructionInfo>
{
    fn match_instruction_for_new(
        instruction: BirdsInstructionNode,
        mut current_node: FlowNode<BirdsInstructionNode, BirdsInstructionInfo>,
        nodes: &mut HashMap<usize, FlowNode<BirdsInstructionNode, BirdsInstructionInfo>>,
        label_block_locations: &mut HashMap<String, usize>,
    ) -> FlowNode<BirdsInstructionNode, BirdsInstructionInfo> {
        match instruction {
            BirdsInstructionNode::Label(ref label) => {
                if !current_node.instructions.is_empty() {
                    nodes.insert(nodes.len(), current_node);
                    current_node = FlowNode::new();
                }
                label_block_locations.insert(label.clone(), nodes.len());
                current_node
                    .instructions
                    .push_back((instruction, BirdsInstructionInfo::new()));
            }
            BirdsInstructionNode::Jump(_)
            | BirdsInstructionNode::JumpZero(_, _)
            | BirdsInstructionNode::JumpNotZero(_, _)
            | BirdsInstructionNode::JumpCondition(_, _, _, _)
            | BirdsInstructionNode::Return(_) => {
                current_node
                    .instructions
                    .push_back((instruction, BirdsInstructionInfo::new()));
                nodes.insert(nodes.len(), current_node);
                current_node = FlowNode::new();
            }
            _ => current_node
                .instructions
                .push_back((instruction, BirdsInstructionInfo::new())),
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
            BirdsInstructionNode::Return(_) => self.add_edge(index, last_index),
            BirdsInstructionNode::Jump(label) => {
                let other_index = *self.label_block_locations.get(label).unwrap();
                self.add_edge(index, &other_index);
            }
            BirdsInstructionNode::JumpZero(_, label)
            | BirdsInstructionNode::JumpNotZero(_, label)
            | BirdsInstructionNode::JumpCondition(_, _, _, label) => {
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
