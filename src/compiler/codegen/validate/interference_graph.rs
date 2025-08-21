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
                Instruction::Cvttsd2si(t, src_double, dst) => {
                    self.add_mock_register(&AssemblyType::Double, src_double, context);
                    self.add_mock_register(t, dst, context);
                }
                Instruction::Cvtsi2sd(t, src, dst_double) => {
                    self.add_mock_register(t, src, context);
                    self.add_mock_register(&AssemblyType::Double, dst_double, context);
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
        if ((*t == AssemblyType::Double && self.double)
            || (*t != AssemblyType::Double && !self.double))
            && !matches!(
                operand,
                Operand::Imm(_) | Operand::Data(_, _) | Operand::Indexed(_, _, _)
            )
            && !context.static_variables.contains(operand)
            && !context.aliased_variables.contains(operand)
        {
            if let Operand::MockReg(_r) = operand {
                // println!("{:?}", operand);
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
                            // println!(
                            //     "INTERFERENCE BETWEEN {:?} AND {:?} IN {:?}",
                            //     op, live_reg, instruction.0
                            // );
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
                        // println!("doing comparisons");
                        let val_a: f64 =
                            node_a.spill_cost / self.count_unpruned_neighbours(node_a) as f64;
                        let val_b: f64 =
                            node_b.spill_cost / self.count_unpruned_neighbours(node_b) as f64;
                        // println!(
                        //     "{:?} {} {} has {:?}: count {:?} total count {:?}",
                        //     op_a,
                        //     val_a,
                        //     node_a.spill_cost,
                        //     node_a
                        //         .neighbours
                        //         .iter()
                        //         .filter(|a| !self.nodes.get(a).unwrap().pruned)
                        //         .collect_vec(),
                        //     self.count_unpruned_neighbours(node_a),
                        //     node_a.neighbours.len(),
                        // );
                        // println!(
                        //     "{:?} {} {} has {:?}: count {:?} total count {:?}",
                        //     op_b,
                        //     val_b,
                        //     node_b.spill_cost,
                        //     node_b
                        //         .neighbours
                        //         .iter()
                        //         .filter(|a| !self.nodes.get(a).unwrap().pruned)
                        //         .collect_vec(),
                        //     self.count_unpruned_neighbours(node_b),
                        //     node_b.neighbours.len(),
                        // );
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

        // println!(
        //     "CHOSEN NODE IS {:?} with {:?}",
        //     chosen_node,
        //     self.count_unpruned_neighbours(self.nodes.get(&chosen_node).unwrap())
        // );
        // let node = self.nodes.get(&chosen_node).unwrap();
        // println!(
        //     "{:?} / {:?}",
        //     node.spill_cost,
        //     self.count_unpruned_neighbours(node)
        // );
        // println!("{:?}", node.neighbours,);
        self.nodes.get_mut(&chosen_node).unwrap().pruned = true;
        // recursively colour the rest of the graph, with this node pruned
        self.colour_graph();

        let node = self.nodes.get(&chosen_node).unwrap();

        let neighbour_colours = node
            .neighbours
            .iter()
            .filter_map(|op| self.nodes.get(op).unwrap().colour)
            .collect_vec();
        // println!(
        //     "neighbours of {:?}, {:?} have neighbour colours {:?}",
        //     chosen_node, node.neighbours, neighbour_colours
        // );

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
            // println!("and therefore choosing (big) {:?}", node.colour);
        } else {
            node.colour = Some(*new_colours.first().unwrap());
            // println!("and therefore choosing (small) {:?}", node.colour);
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

    pub fn coalesce(&self, _instructions: &Vec<Instruction>) -> HashMap<Operand, Vec<Operand>> {
        HashMap::new()
    }
}
