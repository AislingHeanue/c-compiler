use std::{collections::HashMap, error::Error};

use fold_constants::FoldConstants;

use crate::{
    compiler::{
        flow_graph::FlowGraph,
        parser::StructInfo,
        types::{StorageInfo, SymbolInfo},
    },
    OptimizeConfig,
};

use super::{
    BirdsBinaryOperatorNode, BirdsInstructionNode, BirdsProgramNode, BirdsValueNode, OptimizeResult,
};

mod dead_code;
mod flow_graph;
mod fold_constants;
mod propagate_copies;

pub struct OptimizeContext {
    symbols: HashMap<String, SymbolInfo>,
    structs: HashMap<String, StructInfo>,
    block_reaching_copies: HashMap<usize, Vec<(BirdsValueNode, BirdsValueNode)>>,
    aliased_variables: Vec<BirdsValueNode>,
    // all_copies: Vec<(BirdsValueNode, BirdsValueNode)>,
    config: OptimizeConfig,
}

#[derive(Debug, Clone)]
pub struct BirdsInstructionInfo {
    reaching_copies: Vec<(BirdsValueNode, BirdsValueNode)>,
}

impl BirdsInstructionInfo {
    pub fn new() -> BirdsInstructionInfo {
        BirdsInstructionInfo {
            reaching_copies: Vec::new(),
        }
    }
}

trait Optimize
where
    Self: Sized,
{
    fn optimize(self, context: &mut OptimizeContext) -> Result<Self, Box<dyn Error>>;
}

pub fn do_optimize(
    mut birds: BirdsProgramNode,
    symbols: HashMap<String, SymbolInfo>,
    structs: HashMap<String, StructInfo>,
    optimize_config: OptimizeConfig,
) -> Result<OptimizeResult, Box<dyn Error>> {
    let mut context = OptimizeContext {
        symbols,
        structs,
        block_reaching_copies: HashMap::new(),
        aliased_variables: Vec::new(),
        config: optimize_config,
    };
    birds = birds.optimize(&mut context)?;

    Ok((birds, context.symbols, context.structs))
}

impl Optimize for BirdsProgramNode {
    fn optimize(self, context: &mut OptimizeContext) -> Result<Self, Box<dyn Error>> {
        let mut out = Vec::new();
        for top_level in self.body.into_iter() {
            out.push(match top_level {
                super::BirdsTopLevel::Function(a, b, body, d) => {
                    super::BirdsTopLevel::Function(a, b, body.optimize(context)?, d)
                }
                super::BirdsTopLevel::StaticVariable(_, _, _, _) => top_level,
                super::BirdsTopLevel::StaticConstant(_, _, _) => top_level,
            });
        }
        Ok(BirdsProgramNode { body: out })
    }
}

impl Optimize for Vec<BirdsInstructionNode> {
    fn optimize(
        mut self,
        context: &mut OptimizeContext,
    ) -> Result<Vec<BirdsInstructionNode>, Box<dyn Error>> {
        if self.is_empty() {
            return Ok(self);
        }

        while !self.is_empty() {
            let original_instructions = self.clone();
            context.aliased_variables = address_taken_analysis(&self, context);

            if context.config.fold_constants {
                self.fold_constants(context)?;
            }

            // move self into a flow control graph
            let mut graph = FlowGraph::new(self);

            if context.config.propagate_copies {
                graph.propagate_copies(context)?;
            }

            if context.config.eliminate_dead_code {
                graph.eliminate_dead_code(context)?;
            }

            if context.config.eliminate_dead_stores {
                graph.eliminate_dead_stores(context)?;
            }

            // move back out of the graph into
            self = graph.into_instructions();

            // hit a fixed point, no further optimization needed
            if original_instructions == *self {
                break;
            }
        }
        Ok(self)
    }
}

fn address_taken_analysis(
    v: &[BirdsInstructionNode],
    context: &mut OptimizeContext,
) -> Vec<BirdsValueNode> {
    context
        .symbols
        .iter()
        .filter_map(|s| {
            if matches!(s.1.storage, StorageInfo::Static(_, _)) {
                Some(BirdsValueNode::Var(s.0.clone()))
            } else {
                None
            }
        })
        .chain(v.iter().filter_map(|instruction| {
            if let BirdsInstructionNode::GetAddress(src, _) = instruction {
                Some(src.clone())
            } else {
                None
            }
        }))
        .collect()
}
