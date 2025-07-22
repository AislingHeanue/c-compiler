use std::error::Error;

use crate::compiler::{
    lexer::Type,
    parser::{
        BinaryOperatorNode, ExpressionNode, FunctionNode, ProgramNode, StatementNode,
        UnaryOperatorNode,
    },
};

use super::{
    BirdsBinaryOperatorNode, BirdsFunctionNode, BirdsInstructionNode, BirdsInstructions,
    BirdsProgramNode, BirdsUnaryOperatorNode, BirdsValueNode, Convert, ConvertContext,
};

impl Convert for BirdsProgramNode {
    type Input = ProgramNode;
    type Output = Self;

    fn convert(
        parsed: ProgramNode,
        context: &mut ConvertContext,
    ) -> Result<BirdsProgramNode, Box<dyn Error>> {
        Ok(BirdsProgramNode {
            function: BirdsFunctionNode::convert(parsed.function, context)?,
        })
    }
}

impl Convert for BirdsFunctionNode {
    type Input = FunctionNode;
    type Output = Self;

    fn convert(
        parsed: FunctionNode,
        context: &mut ConvertContext,
    ) -> Result<BirdsFunctionNode, Box<dyn Error>> {
        let name = parsed.name;

        let StatementNode::Return(expression) = parsed.body;
        let (mut instructions, new_src) = BirdsInstructions::convert(expression, context)?;
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
        context: &mut ConvertContext,
    ) -> Result<(BirdsInstructions, BirdsValueNode), Box<dyn Error>> {
        match parsed {
            ExpressionNode::Constant(c) => {
                Ok((BirdsInstructions(Vec::new()), BirdsValueNode::Constant(c)))
            }
            ExpressionNode::Unary(op, src) => {
                context.last_stack_number += 1;
                let new_dst = BirdsValueNode::Var(format!("stack.{}", context.last_stack_number));
                let bird_op = match op {
                    UnaryOperatorNode::Complement => BirdsUnaryOperatorNode::Complement,
                    UnaryOperatorNode::Negate => BirdsUnaryOperatorNode::Negate,
                    UnaryOperatorNode::Not => BirdsUnaryOperatorNode::Not,
                };

                let (mut instructions, new_src) = BirdsInstructions::convert(*src, context)?;

                instructions.0.push(BirdsInstructionNode::Unary(
                    bird_op,
                    new_src,
                    new_dst.clone(),
                ));
                Ok((instructions, new_dst))
            }
            ExpressionNode::Binary(op, left, right) => {
                let (mut instructions, new_left) = BirdsInstructions::convert(*left, context)?;

                let (mut instructions_from_right, new_right) =
                    BirdsInstructions::convert(*right, context)?;

                context.last_stack_number += 1;
                let new_dst = BirdsValueNode::Var(format!("stack.{}", context.last_stack_number));
                let bird_op = match op {
                    BinaryOperatorNode::Add => Some(BirdsBinaryOperatorNode::Add),
                    BinaryOperatorNode::Subtract => Some(BirdsBinaryOperatorNode::Subtract),
                    BinaryOperatorNode::Multiply => Some(BirdsBinaryOperatorNode::Multiply),
                    BinaryOperatorNode::Divide => Some(BirdsBinaryOperatorNode::Divide),
                    BinaryOperatorNode::Mod => Some(BirdsBinaryOperatorNode::Mod),
                    BinaryOperatorNode::Equal => Some(BirdsBinaryOperatorNode::Equal),
                    BinaryOperatorNode::NotEqual => Some(BirdsBinaryOperatorNode::NotEqual),
                    BinaryOperatorNode::Less => Some(BirdsBinaryOperatorNode::Less),
                    BinaryOperatorNode::Greater => Some(BirdsBinaryOperatorNode::Greater),
                    BinaryOperatorNode::LessEqual => Some(BirdsBinaryOperatorNode::LessEqual),
                    BinaryOperatorNode::GreaterEqual => Some(BirdsBinaryOperatorNode::GreaterEqual),
                    // process these in the 'else' block below instead
                    BinaryOperatorNode::And | BinaryOperatorNode::Or => None,
                };
                if let Some(bird_op_found) = bird_op {
                    instructions.0.append(&mut instructions_from_right.0);
                    instructions.0.push(BirdsInstructionNode::Binary(
                        bird_op_found,
                        new_left,
                        new_right,
                        new_dst.clone(),
                    ));
                } else {
                    context.last_end_label_number += 1;
                    let end_label_name = format!("end_{}", context.last_end_label_number);

                    context.last_false_label_number += 1;
                    let false_label_name = format!("false_{}", context.last_false_label_number);

                    context.last_true_label_number += 1;
                    let true_label_name = format!("true_{}", context.last_true_label_number);
                    match op {
                        BinaryOperatorNode::Or => {
                            instructions.0.push(BirdsInstructionNode::JumpNotZero(
                                new_left,
                                false_label_name.clone(),
                            ));

                            instructions.0.append(&mut instructions_from_right.0);

                            instructions.0.append(&mut vec![
                                BirdsInstructionNode::JumpNotZero(
                                    new_right,
                                    true_label_name.clone(),
                                ),
                                BirdsInstructionNode::Copy(
                                    BirdsValueNode::Constant(Type::Integer(0)),
                                    new_dst.clone(),
                                ),
                                BirdsInstructionNode::Jump(end_label_name.clone()),
                                BirdsInstructionNode::Label(true_label_name),
                                BirdsInstructionNode::Copy(
                                    BirdsValueNode::Constant(Type::Integer(1)),
                                    new_dst.clone(),
                                ),
                                BirdsInstructionNode::Label(end_label_name),
                            ]);
                        }
                        BinaryOperatorNode::And => {
                            instructions.0.push(BirdsInstructionNode::JumpZero(
                                new_left,
                                false_label_name.clone(),
                            ));

                            instructions.0.append(&mut instructions_from_right.0);

                            instructions.0.append(&mut vec![
                                BirdsInstructionNode::JumpZero(new_right, false_label_name.clone()),
                                BirdsInstructionNode::Copy(
                                    BirdsValueNode::Constant(Type::Integer(1)),
                                    new_dst.clone(),
                                ),
                                BirdsInstructionNode::Jump(end_label_name.clone()),
                                BirdsInstructionNode::Label(false_label_name),
                                BirdsInstructionNode::Copy(
                                    BirdsValueNode::Constant(Type::Integer(0)),
                                    new_dst.clone(),
                                ),
                                BirdsInstructionNode::Label(end_label_name),
                            ]);
                        }
                        _ => panic!("Unexpected binary operator found: {:?}", op),
                    };
                }
                Ok((instructions, new_dst))
            }
        }
    }
}
