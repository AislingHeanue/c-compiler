use std::error::Error;

use crate::compiler::parser::{
    BinaryOperatorNode, ExpressionNode, FunctionNode, ProgramNode, StatementNode, UnaryOperatorNode,
};

use super::{
    BirdsBinaryOperatorNode, BirdsFunctionNode, BirdsInstructionNode, BirdsInstructions,
    BirdsProgramNode, BirdsUnaryOperatorNode, BirdsValueNode, Convert,
};

impl Convert for BirdsProgramNode {
    type Input = ProgramNode;
    type Output = Self;

    fn convert(parsed: ProgramNode) -> Result<BirdsProgramNode, Box<dyn Error>> {
        Ok(BirdsProgramNode {
            function: BirdsFunctionNode::convert(parsed.function)?,
        })
    }
}

impl Convert for BirdsFunctionNode {
    type Input = FunctionNode;
    type Output = Self;

    fn convert(parsed: FunctionNode) -> Result<BirdsFunctionNode, Box<dyn Error>> {
        let name = parsed.name;

        let StatementNode::Return(expression) = parsed.body;
        let (mut instructions, new_src) = BirdsInstructions::convert(expression)?;
        let return_instruction = BirdsInstructionNode::Return(new_src);
        instructions.0.push(return_instruction);

        Ok(BirdsFunctionNode { name, instructions })
    }
}

impl Convert for BirdsInstructions {
    type Input = ExpressionNode;
    type Output = (BirdsInstructions, BirdsValueNode);

    fn convert(
        parsed: ExpressionNode,
    ) -> Result<(BirdsInstructions, BirdsValueNode), Box<dyn Error>> {
        BirdsInstructions::convert_with_tmp_number(parsed, &mut 0)
    }
}

impl BirdsInstructions {
    fn convert_with_tmp_number(
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
                    UnaryOperatorNode::Not => todo!(),
                };

                let (mut instructions, new_src) =
                    BirdsInstructions::convert_with_tmp_number(*src, last_tmp_number)?;

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
                    BinaryOperatorNode::And => todo!(),
                    BinaryOperatorNode::Or => todo!(),
                    BinaryOperatorNode::Equal => todo!(),
                    BinaryOperatorNode::NotEqual => todo!(),
                    BinaryOperatorNode::Less => todo!(),
                    BinaryOperatorNode::Greater => todo!(),
                    BinaryOperatorNode::LessEqual => todo!(),
                    BinaryOperatorNode::GreaterEqual => todo!(),
                };

                let (mut instructions, new_left) =
                    BirdsInstructions::convert_with_tmp_number(*left, last_tmp_number)?;
                let (mut instructions_from_right, new_right) =
                    BirdsInstructions::convert_with_tmp_number(*right, last_tmp_number)?;

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
