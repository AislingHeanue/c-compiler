use std::error::Error;

use super::{
    lexer::Type,
    parser::{ExpressionNode, FunctionNode, ProgramNode, StatementNode, UnaryOperatorNode},
};

// BIRDS: Bodacious Intermediate Representation Design Spec
#[derive(Debug)]
pub struct BirdsProgramNode {
    pub function: BirdsFunctionNode,
}

#[derive(Debug)]
pub struct BirdsFunctionNode {
    pub name: String,
    pub instructions: Instructions,
}

#[derive(Debug)]
pub struct Instructions(Vec<BirdsInstructionNode>);

#[derive(Debug)]
pub enum BirdsInstructionNode {
    Return(BirdsValueNode),
    // op, src, dst
    Unary(BirdsUnaryOperatorNode, BirdsValueNode, BirdsValueNode),
}

#[derive(Clone, Debug)]
pub enum BirdsValueNode {
    Constant(Type),
    Var(String),
}

#[derive(Debug)]
pub enum BirdsUnaryOperatorNode {
    Complement,
    Negate,
}

pub fn birds(parsed: ProgramNode) -> Result<BirdsProgramNode, Box<dyn Error>> {
    BirdsProgramNode::convert(parsed)
}

impl BirdsProgramNode {
    fn convert(parsed: ProgramNode) -> Result<BirdsProgramNode, Box<dyn Error>> {
        Ok(BirdsProgramNode {
            function: BirdsFunctionNode::convert(parsed.function)?,
        })
    }
}

impl BirdsFunctionNode {
    fn convert(parsed: FunctionNode) -> Result<BirdsFunctionNode, Box<dyn Error>> {
        let name = parsed.name;

        let StatementNode::Return(expression) = parsed.body;
        let (mut instructions, new_src) = Instructions::convert(expression)?;
        let return_instruction = BirdsInstructionNode::Return(new_src);
        instructions.0.push(return_instruction);

        Ok(BirdsFunctionNode { name, instructions })
    }
}

static mut VARIABLE_COUNTER: usize = 0;
impl Instructions {
    fn convert(parsed: ExpressionNode) -> Result<(Instructions, BirdsValueNode), Box<dyn Error>> {
        match parsed {
            ExpressionNode::Constant(c) => {
                Ok((Instructions(Vec::new()), BirdsValueNode::Constant(c)))
            }
            ExpressionNode::Unary(op, src) => {
                let bird_op = match op {
                    UnaryOperatorNode::Complement => BirdsUnaryOperatorNode::Complement,
                    UnaryOperatorNode::Negate => BirdsUnaryOperatorNode::Negate,
                };
                let (mut instructions, new_src) = Instructions::convert(*src)?;
                let new_dst;
                unsafe {
                    new_dst = BirdsValueNode::Var(format!("tmp.{}", VARIABLE_COUNTER));
                    VARIABLE_COUNTER += 1;
                };
                instructions.0.push(BirdsInstructionNode::Unary(
                    bird_op,
                    new_src,
                    new_dst.clone(),
                ));
                Ok((instructions, new_dst))
            }
        }
    }
}
