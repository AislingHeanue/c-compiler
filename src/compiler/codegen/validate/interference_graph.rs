use std::collections::{HashMap, HashSet};

use itertools::Itertools;

use crate::compiler::{
    codegen::{AssemblySymbolInfo, AssemblyType, Instruction, Operand, Register},
    flow_graph::FlowGraph,
};

use super::{flow_graph::InstructionInfo, ValidateContext};

#[derive(Debug)]
pub struct InterferenceGraph {
    pub nodes: HashMap<Operand, InterferenceNode>,
    // whether this instance of the graph is for general purpose or double registers
    double: bool,
}

#[derive(Debug)]
pub struct InterferenceNode {
    neighbours: HashSet<Operand>,
    pub spill_cost: f64,
    colour: Option<usize>,
    pruned: bool,
}

static INTEGER_REGISTERS: [Register; 12] = [
    Register::AX,
    Register::BX,
    Register::CX,
    Register::DX,
    Register::DI,
    Register::SI,
    Register::R8,
    Register::R9,
    Register::R12,
    Register::R13,
    Register::R14,
    Register::R15,
];

static DOUBLE_REGISTERS: [Register; 14] = [
    Register::XMM0,
    Register::XMM1,
    Register::XMM2,
    Register::XMM3,
    Register::XMM4,
    Register::XMM5,
    Register::XMM6,
    Register::XMM7,
    Register::XMM8,
    Register::XMM9,
    Register::XMM10,
    Register::XMM11,
    Register::XMM12,
    Register::XMM13,
];

impl InterferenceNode {
    fn new() -> InterferenceNode {
        InterferenceNode {
            neighbours: HashSet::new(),
            spill_cost: 1.,
            colour: None,
            pruned: false,
        }
    }
    fn new_register() -> InterferenceNode {
        InterferenceNode {
            neighbours: HashSet::new(),
            spill_cost: f64::INFINITY,
            colour: None,
            pruned: false,
        }
    }
}

impl InterferenceGraph {
    fn base_graph(double: bool) -> InterferenceGraph {
        let mut nodes: HashMap<Operand, InterferenceNode> = HashMap::new();
        if double {
            for reg in DOUBLE_REGISTERS.iter() {
                let mut this_node = InterferenceNode::new_register();
                for (op, node) in nodes.iter_mut() {
                    node.neighbours.insert(Operand::Reg(reg.clone()));
                    this_node.neighbours.insert(op.clone());
                }
                nodes.insert(Operand::Reg(reg.clone()), this_node);
            }
        } else {
            for reg in INTEGER_REGISTERS.iter() {
                let mut this_node = InterferenceNode::new_register();
                for (op, node) in nodes.iter_mut() {
                    node.neighbours.insert(Operand::Reg(reg.clone()));
                    this_node.neighbours.insert(op.clone());
                }
                nodes.insert(Operand::Reg(reg.clone()), this_node);
            }
        }
        InterferenceGraph { nodes, double }
    }

    pub fn new(
        instructions: &Vec<Instruction>,
        double: bool,
        context: &mut ValidateContext,
    ) -> InterferenceGraph {
        let mut graph = InterferenceGraph::base_graph(double);
        graph.add_mock_registers(instructions, context);

        let mut flow_graph = FlowGraph::new(instructions.to_vec());
        flow_graph.check_liveness(context);
        graph.add_edges(&flow_graph, context);

        graph
    }

    fn add_mock_registers(
        &mut self,
        instructions: &Vec<Instruction>,
        context: &mut ValidateContext,
    ) {
        for instruction in instructions {
            match instruction {
                Instruction::Mov(t, src, dst) => {
                    self.add_mock_register(t, src, context);
                    self.add_mock_register(t, dst, context);
                }
                Instruction::Movsx(t, other_t, src, dst) => {
                    self.add_mock_register(t, src, context);
                    self.add_mock_register(other_t, dst, context);
                }
                Instruction::MovZeroExtend(t, other_t, src, dst) => {
                    self.add_mock_register(t, src, context);
                    self.add_mock_register(other_t, dst, context);
                }
                Instruction::Lea(_src, dst) => {
                    // src of Lea is aliased, we cannot add it to the map, and it's an error if
                    // it gets into the map
                    self.add_mock_register(&AssemblyType::Quadword, dst, context);
                }
                Instruction::FloatToInt(src_t, dst_t, src, dst) => {
                    self.add_mock_register(src_t, src, context);
                    self.add_mock_register(dst_t, dst, context);
                }
                Instruction::IntToFloat(src_t, dst_t, src, dst) => {
                    self.add_mock_register(src_t, src, context);
                    self.add_mock_register(dst_t, dst, context);
                }
                Instruction::Cvtss2sd(src_float, dst_double) => {
                    self.add_mock_register(&AssemblyType::Float, src_float, context);
                    self.add_mock_register(&AssemblyType::Double, dst_double, context);
                }
                Instruction::Cvtsd2ss(src_double, dst_float) => {
                    self.add_mock_register(&AssemblyType::Double, src_double, context);
                    self.add_mock_register(&AssemblyType::Float, dst_float, context);
                }
                Instruction::Unary(_, t, src) => {
                    self.add_mock_register(t, src, context);
                }
                Instruction::Binary(_, t, left, right) => {
                    self.add_mock_register(t, left, context);
                    self.add_mock_register(t, right, context);
                }
                Instruction::Cmp(t, src, dst) => {
                    self.add_mock_register(t, src, context);
                    self.add_mock_register(t, dst, context);
                }
                Instruction::Idiv(t, src) => {
                    self.add_mock_register(t, src, context);
                }
                Instruction::Div(t, src) => {
                    self.add_mock_register(t, src, context);
                }
                Instruction::Cdq(_) => {}
                Instruction::Jmp(_) => {}
                Instruction::JmpCondition(_, _, _) => {}
                Instruction::SetCondition(_, dst_int, _) => {
                    self.add_mock_register(&AssemblyType::Longword, dst_int, context)
                }
                Instruction::CallIndirect(src) => {
                    self.add_mock_register(&AssemblyType::Quadword, src, context)
                }
                Instruction::Label(_) => {}
                Instruction::Push(src) => {
                    if let Operand::MockReg(name) = src {
                        let symbol = context.symbols.get(name).unwrap();
                        if let AssemblySymbolInfo::Object(t, _, _) = symbol {
                            let t = *t;
                            self.add_mock_register(&t, src, context);
                        }
                    }
                }
                Instruction::Pop(_r) => {} // pop only pops Reg
                Instruction::Call(_f) => {}
                Instruction::Ret => {}
            }
        }
    }

    fn add_mock_register(
        &mut self,
        t: &AssemblyType,
        operand: &Operand,
        context: &mut ValidateContext,
    ) {
        if ((t.is_float() && self.double) || (!t.is_float() && !self.double))
            && !matches!(
                operand,
                Operand::Imm(_) | Operand::Data(_, _) | Operand::Indexed(_, _, _)
            )
            && !context.static_variables.contains(operand)
            && !context.aliased_variables.contains(operand)
        {
            if let Operand::MockReg(_r) = operand {
                if let Some(node) = self.nodes.get_mut(operand) {
                    node.spill_cost += 1.;
                } else {
                    self.nodes.insert(operand.clone(), InterferenceNode::new());
                }
            }
        }
    }

    fn add_edges(
        &mut self,
        flow_graph: &FlowGraph<Instruction, InstructionInfo>,
        context: &mut ValidateContext,
    ) {
        let keys = flow_graph.indexes();
        for index in keys.iter() {
            if *index == 0 || *index == flow_graph.last_index {
                continue;
            }
            let node = flow_graph.nodes.get(index).unwrap();
            for instruction in node.instructions.iter() {
                let (_used, updated) = instruction.0.get_used_and_updated_operands(context);
                for live_reg in instruction.1.live_registers.iter() {
                    // interference occurs if one register is updated while the other is live
                    if let Instruction::Mov(_, src, _)
                    | Instruction::Movsx(_, _, src, _)
                    | Instruction::MovZeroExtend(_, _, src, _) = &instruction.0
                    {
                        if src == live_reg {
                            continue;
                        }
                    }

                    for op in updated.iter() {
                        if self.nodes.contains_key(op)
                            && self.nodes.contains_key(live_reg)
                            && op != live_reg
                        {
                            // Interference detected and noted here
                            self.add_edge(op, live_reg);
                        }
                    }
                }
            }
        }
    }

    fn add_edge(&mut self, left: &Operand, right: &Operand) {
        let left_node = self.nodes.get_mut(left).unwrap();
        left_node.neighbours.insert(right.clone());

        let right_node = self.nodes.get_mut(right).unwrap();
        right_node.neighbours.insert(left.clone());
    }

    fn remove_edge(&mut self, left: &Operand, right: &Operand) {
        let left_node = self.nodes.get_mut(left).unwrap();
        left_node.neighbours.remove(right);

        let right_node = self.nodes.get_mut(right).unwrap();
        right_node.neighbours.remove(left);
    }

    pub fn colour_graph(&mut self) {
        let max_degree = if self.double { 14 } else { 12 };
        // NOTE: The order of this list is RANDOM, so results may differ per run.
        let remaining = self
            .nodes
            .iter()
            .filter(|(_op, node)| !node.pruned)
            .collect_vec();

        if remaining.is_empty() {
            return;
        }

        let chosen_node = remaining
            .iter()
            .find(|(_, node)| self.count_unpruned_neighbours(node) < max_degree)
            .unwrap_or_else(|| {
                remaining
                    .iter()
                    .min_by(|(_, node_a), (_, node_b)| {
                        let val_a: f64 =
                            node_a.spill_cost / self.count_unpruned_neighbours(node_a) as f64;
                        let val_b: f64 =
                            node_b.spill_cost / self.count_unpruned_neighbours(node_b) as f64;
                        // thanks https://stackoverflow.com/questions/28446632
                        match PartialOrd::partial_cmp(&val_a, &val_b) {
                            Some(o) => o,
                            _ => std::cmp::Ordering::Equal,
                        }
                    })
                    // this will never be called on an empty list, so this unwrap should be safe
                    .unwrap()
            })
            .0
            .clone();

        self.nodes.get_mut(&chosen_node).unwrap().pruned = true;
        // recursively colour the rest of the graph, with this node pruned
        self.colour_graph();

        let node = self.nodes.get(&chosen_node).unwrap();

        let neighbour_colours = node
            .neighbours
            .iter()
            .filter_map(|op| self.nodes.get(op).unwrap().colour)
            .collect_vec();

        let new_colours = (0..max_degree)
            .filter(|c| !neighbour_colours.contains(c))
            .collect_vec();

        if new_colours.is_empty() {
            return;
        }

        let node = self.nodes.get_mut(&chosen_node).unwrap();
        if chosen_node.is_callee_saved_register() {
            // add later colours to the registers BX and R12-R15 because
            // we have to make sure to restore their value after using them
            // before returning to the caller, so we want to avoid that as
            // much as possible
            node.colour = Some(*new_colours.last().unwrap());
        } else {
            node.colour = Some(*new_colours.first().unwrap());
        }
        node.pruned = false;
    }

    fn count_unpruned_neighbours(&self, node: &InterferenceNode) -> usize {
        let mut count = 0;
        for neighbour_op in node.neighbours.iter() {
            let neighbour_node = self.nodes.get(neighbour_op).unwrap();
            if !neighbour_node.pruned {
                count += 1;
            }
        }
        count
    }

    pub fn get_register_map(&self, context: &mut ValidateContext) {
        let colours_to_registers: HashMap<usize, Register> = self
            .nodes
            .iter()
            .filter_map(|(op, node)| {
                if let Operand::Reg(r) = op {
                    // hardware registers are always coloured because they are pruned last
                    // in the previous step
                    Some((node.colour.unwrap(), r.clone()))
                } else {
                    None
                }
            })
            .collect();

        let mut out: HashMap<String, Register> = HashMap::new();
        let mut callee_saved_registers = HashSet::new();
        for (op, node) in self.nodes.iter() {
            if let Operand::MockReg(name) = op {
                if let Some(colour) = node.colour {
                    let register = colours_to_registers.get(&colour).unwrap();
                    out.insert(name.clone(), register.clone());
                    if Operand::Reg(register.clone()).is_callee_saved_register() {
                        callee_saved_registers.insert(register.clone());
                    }
                }
            }
        }
        if !self.double {
            context.function_callee_saved_registers.insert(
                context.current_function_name.clone().unwrap(),
                callee_saved_registers.iter().cloned().collect_vec(),
            );
        }
        context.register_map = out;
    }

    // output map points from some operand X to the operand it is Rewritten as.
    pub fn coalesce(&mut self, instructions: &Vec<Instruction>) -> HashMap<Operand, Operand> {
        let mut out = HashMap::new();

        for instruction in instructions {
            if let Instruction::Mov(_, src, dst) = instruction {
                let src = true_register_value(src, &out);
                let dst = true_register_value(dst, &out);

                if self.nodes.contains_key(&src)
                    && self.nodes.contains_key(&dst)
                    // duh
                    && src != dst
                    // can't coalesce nodes that interfere with each other
                    && !self.nodes.get(&src).unwrap().neighbours.contains(&dst)
                    && self.can_coalesce(&src, &dst)
                {
                    let (merge_this_op, into_this) = if matches!(src, Operand::Reg(_)) {
                        (dst, src)
                    } else {
                        (src, dst)
                    };
                    self.coalesce_node(&merge_this_op, &into_this);
                    out.insert(merge_this_op, into_this);
                }
            }
        }

        out
    }

    fn can_coalesce(&self, left: &Operand, right: &Operand) -> bool {
        self.briggs_test(left, right)
            || (matches!(left, Operand::Reg(_)) && self.george_test(left, right))
            || (matches!(right, Operand::Reg(_)) && self.george_test(right, left))
    }

    fn briggs_test(&self, left: &Operand, right: &Operand) -> bool {
        let max_degree = if self.double { 14 } else { 12 };
        let left_node = self.nodes.get(left).unwrap();
        let right_node = self.nodes.get(right).unwrap();

        let significant_neighbours = left_node
            .neighbours
            .union(&right_node.neighbours)
            .filter(|op| {
                let node = self.nodes.get(op).unwrap();
                let degree = if node.neighbours.contains(left) && node.neighbours.contains(right) {
                    node.neighbours.len() - 1
                } else {
                    node.neighbours.len()
                };
                degree >= max_degree
            })
            .count();

        significant_neighbours < max_degree
    }

    fn george_test(&self, hard_register: &Operand, mock_register: &Operand) -> bool {
        let max_degree = if self.double { 14 } else { 12 };
        let mock_register_node = self.nodes.get(mock_register).unwrap();
        let hard_register_node = self.nodes.get(hard_register).unwrap();
        // ensures that if we move mock_register to this hard_register, then every neighbour either
        // 1. does not gain a new neighbour or
        // 2. does not have more than max_degree neighbours (which means it can be trivially pruned)
        mock_register_node.neighbours.iter().all(|op| {
            hard_register_node.neighbours.contains(op)
                || self.nodes.get(op).unwrap().neighbours.len() < max_degree
        })
    }

    fn coalesce_node(&mut self, from: &Operand, to: &Operand) {
        let node_to_remove = self.nodes.get(from).unwrap();
        node_to_remove.neighbours.clone().iter().for_each(|op| {
            self.remove_edge(from, op);
            self.add_edge(to, op);
        });

        self.nodes.remove(from);
    }
}

pub fn true_register_value(op: &Operand, map: &HashMap<Operand, Operand>) -> Operand {
    if let Some(new_op) = map.get(op) {
        // WARNING: infinite loop possible here if the map is constructed terribly,
        // so don't do that
        true_register_value(new_op, map)
    } else {
        op.clone()
    }
}
