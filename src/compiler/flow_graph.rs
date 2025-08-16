use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::Debug,
};

use itertools::Itertools;

#[derive(Debug)]
pub struct FlowGraph<T, I> {
    pub nodes: HashMap<usize, FlowNode<T, I>>,
    pub label_block_locations: HashMap<String, usize>,
}

#[derive(Clone, Debug)]
pub struct FlowNode<T, I> {
    pub befores: HashSet<usize>,
    pub instructions: VecDeque<(T, I)>,
    pub afters: HashSet<usize>,
}

pub trait InstructionMatching<T, I> {
    fn match_instruction_for_new(
        instruction: T,
        current_node: FlowNode<T, I>,
        nodes: &mut HashMap<usize, FlowNode<T, I>>,
        label_block_locations: &mut HashMap<String, usize>,
    ) -> FlowNode<T, I>;

    fn match_instruction_for_edge(&mut self, index: &usize, next_index: &usize, last_index: &usize);
}

impl<T, I> FlowNode<T, I> {
    pub fn new() -> FlowNode<T, I> {
        FlowNode {
            befores: HashSet::new(),
            instructions: VecDeque::new(),
            afters: HashSet::new(),
        }
    }
}

impl<T, I> FlowGraph<T, I>
where
    T: Debug,
    I: Debug,
    FlowGraph<T, I>: InstructionMatching<T, I>,
{
    pub fn new(body: Vec<T>) -> FlowGraph<T, I> {
        println!("new loop with body {:?}", body);
        let mut label_block_locations = HashMap::new();
        let mut nodes = HashMap::new();
        nodes.insert(0, FlowNode::new()); // this is the entry node
        let mut current_node = FlowNode::new();

        // entry node always points to the first node only

        for instruction in body.into_iter() {
            current_node = <FlowGraph<T, I>>::match_instruction_for_new(
                instruction,
                current_node,
                &mut nodes,
                &mut label_block_locations,
            );
        }

        if !current_node.instructions.is_empty() {
            nodes.insert(nodes.len(), current_node);
        }

        nodes.insert(nodes.len(), FlowNode::new()); // this is the exit node

        let mut out = FlowGraph {
            nodes,
            label_block_locations,
        };

        out.add_edges();

        out
    }

    pub fn add_edge(&mut self, index: &usize, other: &usize) {
        self.nodes.get_mut(index).unwrap().afters.insert(*other);
        self.nodes.get_mut(other).unwrap().befores.insert(*index);
    }

    pub fn into_instructions(self) -> Vec<T> {
        self.nodes
            .into_iter()
            .sorted_by(|(k1, _), (k2, _)| k1.cmp(k2))
            .flat_map(|(_, v)| v.instructions)
            .map(|(i, _)| i)
            .collect()
    }

    pub fn add_edges(&mut self) {
        let keys: Vec<usize> = self.nodes.keys().sorted().copied().collect();
        let keys_len = keys.len();
        for index in keys.iter() {
            // special case just for the entry and exit nodes.
            if self.nodes.get(index).unwrap().instructions.is_empty() {
                if index + 1 < keys_len {
                    self.add_edge(index, &(index + 1));
                }
                continue;
            }

            self.match_instruction_for_edge(index, &(index + 1), &(&keys_len - 1));
        }
    }

    pub fn remove_node(&mut self, index: usize) {
        let befores = self.nodes.get(&index).unwrap().befores.clone();
        let afters = self.nodes.get(&index).unwrap().afters.clone();
        for before_index in befores.iter() {
            let before_node = self.nodes.get_mut(before_index).unwrap();
            before_node.afters.remove(&index);
        }
        for after_index in afters.iter() {
            let after_node = self.nodes.get_mut(after_index).unwrap();
            after_node.befores.remove(&index);
            // the block we're destroying has a link from a -> b -> c, so we need to make a
            // link from a -> c
            for before_index in befores.iter() {
                if *before_index != index && *after_index != index {
                    self.add_edge(before_index, after_index)
                }
            }
        }
        self.nodes.remove(&index);
        println!("{:?}", self.nodes);
    }

    pub fn traverse(&self, index: usize, marked: &mut HashSet<usize>) {
        let not_seen_already = marked.insert(index);
        if not_seen_already {
            let node = self.nodes.get(&index).unwrap();
            node.afters
                .iter()
                .for_each(|after| self.traverse(*after, marked))
        }
    }
}
