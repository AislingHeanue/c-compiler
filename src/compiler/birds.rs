use std::error::Error;

use super::{
    lexer::Type,
    parser::{
        BinaryOperatorNode, ExpressionNode, FunctionNode, ProgramNode, StatementNode,
        UnaryOperatorNode,
    },
};

// BIRDS: Bodacious Intermediate Representation Design Spec
#[derive(Debug)]
pub struct BirdsProgramNode {
    pub function: BirdsFunctionNode,
}

#[derive(Debug)]
pub struct BirdsFunctionNode {
    pub name: String,
    pub instructions: BirdsInstructions,
}

#[derive(Debug)]
pub struct BirdsInstructions(pub Vec<BirdsInstructionNode>);

#[derive(Debug)]
pub enum BirdsInstructionNode {
    Return(BirdsValueNode),
    // op, src, dst
    Unary(BirdsUnaryOperatorNode, BirdsValueNode, BirdsValueNode),
    // op, left, right, dst
    Binary(
        BirdsBinaryOperatorNode,
        BirdsValueNode,
        BirdsValueNode,
        BirdsValueNode,
    ),
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

#[derive(Debug)]
pub enum BirdsBinaryOperatorNode {
    Add,
    Subtract,
    Multiply,
    Divide,
    Mod,
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
        let mut last_tmp_number = -1;
        let (mut instructions, new_src) =
            BirdsInstructions::convert(expression, &mut last_tmp_number)?;
        let return_instruction = BirdsInstructionNode::Return(new_src);
        instructions.0.push(return_instruction);

        Ok(BirdsFunctionNode { name, instructions })
    }
}

impl BirdsInstructions {
    fn convert(
        parsed: ExpressionNode,
        last_tmp_number: &mut i32,
    ) -> Result<(BirdsInstructions, BirdsValueNode), Box<dyn Error>> {
        match parsed {
            ExpressionNode::Constant(c) => {
                Ok((BirdsInstructions(Vec::new()), BirdsValueNode::Constant(c)))
            }
            ExpressionNode::Unary(op, src) => {
                let bird_op = match op {
                    UnaryOperatorNode::Complement => BirdsUnaryOperatorNode::Complement,
                    UnaryOperatorNode::Negate => BirdsUnaryOperatorNode::Negate,
                };

                let (mut instructions, new_src) =
                    BirdsInstructions::convert(*src, last_tmp_number)?;

                *last_tmp_number += 1;
                let new_dst = BirdsValueNode::Var(format!("tmp.{}", last_tmp_number));
                instructions.0.push(BirdsInstructionNode::Unary(
                    bird_op,
                    new_src,
                    new_dst.clone(),
                ));
                Ok((instructions, new_dst))
            }
            ExpressionNode::Binary(op, left, right) => {
                let bird_op = match op {
                    BinaryOperatorNode::Add => BirdsBinaryOperatorNode::Add,
                    BinaryOperatorNode::Subtract => BirdsBinaryOperatorNode::Subtract,
                    BinaryOperatorNode::Multiply => BirdsBinaryOperatorNode::Multiply,
                    BinaryOperatorNode::Divide => BirdsBinaryOperatorNode::Divide,
                    BinaryOperatorNode::Mod => BirdsBinaryOperatorNode::Mod,
                };

                let (mut instructions, new_left) =
                    BirdsInstructions::convert(*left, last_tmp_number)?;
                let (mut instructions_from_right, new_right) =
                    BirdsInstructions::convert(*right, last_tmp_number)?;

                instructions.0.append(&mut instructions_from_right.0);

                *last_tmp_number += 1;
                let new_dst = BirdsValueNode::Var(format!("tmp.{}", last_tmp_number));
                instructions.0.push(BirdsInstructionNode::Binary(
                    bird_op,
                    new_left,
                    new_right,
                    new_dst.clone(),
                ));
                Ok((instructions, new_dst))
            }
        }
    }
}
