use std::error::Error;

use itertools::process_results;

use crate::compiler::parser::{
    BinaryOperatorNode, Block, BlockItemNode, DeclarationNode, ExpressionNode, ForInitialiserNode,
    FunctionDeclaration, ProgramNode, StatementNode, SwitchMapKey, UnaryOperatorNode,
    VariableDeclaration,
};

use super::{
    BirdsBinaryOperatorNode, BirdsFunctionNode, BirdsInstructionNode, BirdsProgramNode,
    BirdsUnaryOperatorNode, BirdsValueNode, Convert, ConvertContext,
};

impl Convert for ProgramNode {
    type Output = BirdsProgramNode;

    fn convert(self, context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>> {
        Ok(BirdsProgramNode {
            function: process_results(
                self.functions.into_iter().map(|f| f.convert(context)),
                // TODO: multiple functions
                |mut iter| iter.next().unwrap(),
            )?,
        })
    }
}

impl Convert for FunctionDeclaration {
    type Output = BirdsFunctionNode;

    fn convert(self, context: &mut ConvertContext) -> Result<BirdsFunctionNode, Box<dyn Error>> {
        let name = self.name;
        let mut instructions: Vec<BirdsInstructionNode> = Vec::new();
        if let Some(body) = self.body {
            instructions = body.convert(context)?;
            // add an extra "return 0" at the end because the C standard dictates that if main() exits
            // without a return statement, then it must actually return 0. If a return statement is
            // otherwise present, this instruction will never be run (dead code).
            instructions.push(BirdsInstructionNode::Return(
                BirdsValueNode::IntegerConstant(0),
            ));
        }

        Ok(BirdsFunctionNode { name, instructions })
    }
}

impl Convert for Block {
    type Output = Vec<BirdsInstructionNode>;

    fn convert(self, context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>> {
        process_results(
            self.into_iter().map(|node| match node {
                BlockItemNode::Statement(statement) => statement.convert(context),
                BlockItemNode::Declaration(declaration) => declaration.convert(context),
            }),
            |iter| iter.flatten().collect(),
        )
    }
}

impl Convert for DeclarationNode {
    type Output = Vec<BirdsInstructionNode>;

    fn convert(self, context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>> {
        match self {
            DeclarationNode::Variable(v) => Ok(v.convert(context)?),
            DeclarationNode::Function(_f) => todo!(),
        }
    }
}

impl Convert for StatementNode {
    type Output = Vec<BirdsInstructionNode>;

    fn convert(self, context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>> {
        match self {
            StatementNode::Return(expression) => {
                let (mut instructions, new_src) = expression.convert(context)?;
                let return_instruction = BirdsInstructionNode::Return(new_src);
                instructions.push(return_instruction);
                Ok(instructions)
            }
            StatementNode::Expression(expression) => {
                // expressions (including assignments) all have return values that are thrown away
                // as we move onto the next line. eg. "a = 5" returns 5 AND ALSO assigns 5 to a.
                let (instructions, _new_src) = expression.convert(context)?;
                Ok(instructions)
            }
            StatementNode::Pass => Ok::<Vec<BirdsInstructionNode>, Box<dyn Error>>(Vec::new()),
            StatementNode::If(condition, then, otherwise) => {
                if let Some(other) = *otherwise {
                    context.last_end_label_number += 1;
                    let new_end_label_name = format!("end_{}", context.last_end_label_number);
                    context.last_else_label_number += 1;
                    let new_else_label_name = format!("else_{}", context.last_else_label_number);

                    let (mut instructions, new_cond) = condition.convert(context)?;
                    instructions.push(BirdsInstructionNode::JumpZero(
                        new_cond,
                        new_else_label_name.clone(),
                    ));
                    instructions.append(&mut then.convert(context)?);
                    instructions.push(BirdsInstructionNode::Jump(new_end_label_name.clone()));
                    instructions.push(BirdsInstructionNode::Label(new_else_label_name));
                    instructions.append(&mut other.convert(context)?);
                    instructions.push(BirdsInstructionNode::Label(new_end_label_name));

                    Ok(instructions)
                } else {
                    context.last_end_label_number += 1;
                    let new_end_label_name = format!("end_{}", context.last_end_label_number);
                    let (mut instructions, new_cond) = condition.convert(context)?;
                    instructions.push(BirdsInstructionNode::JumpZero(
                        new_cond,
                        new_end_label_name.clone(),
                    ));

                    instructions.append(&mut then.convert(context)?);

                    instructions.push(BirdsInstructionNode::Label(new_end_label_name));
                    Ok(instructions)
                }
            }
            StatementNode::Label(s, statement) => {
                let mut instructions = vec![BirdsInstructionNode::Label(s)];
                instructions.append(&mut statement.convert(context)?);
                Ok(instructions)
            }
            StatementNode::Goto(s) => Ok(vec![BirdsInstructionNode::Jump(s)]),
            StatementNode::Compound(block) => Ok(block.convert(context)?),
            StatementNode::Break(label) => Ok(vec![BirdsInstructionNode::Jump(format!(
                "break_{}",
                label.ok_or::<Box<dyn Error>>("Break has no label".into())?
            ))]),
            StatementNode::Continue(label) => Ok(vec![BirdsInstructionNode::Jump(format!(
                "continue_{}",
                label.ok_or::<Box<dyn Error>>("Continue has no label".into())?
            ))]),
            StatementNode::While(expression, body, label) => {
                let this_loop_label = label.ok_or::<Box<dyn Error>>("While has no label".into())?;

                let mut instructions = vec![BirdsInstructionNode::Label(format!(
                    "continue_{}",
                    this_loop_label
                ))];
                let (mut instructions_from_condition, new_src) = expression.convert(context)?;
                instructions.append(&mut instructions_from_condition);
                instructions.push(BirdsInstructionNode::JumpZero(
                    new_src,
                    format!("break_{}", this_loop_label),
                ));

                instructions.append(&mut body.convert(context)?);

                instructions.push(BirdsInstructionNode::Jump(format!(
                    "continue_{}",
                    this_loop_label
                )));
                instructions.push(BirdsInstructionNode::Label(format!(
                    "break_{}",
                    this_loop_label
                )));

                Ok(instructions)
            }
            StatementNode::DoWhile(body, expression, label) => {
                let this_loop_label =
                    label.ok_or::<Box<dyn Error>>("Do-while has no label".into())?;

                let mut instructions = vec![BirdsInstructionNode::Label(format!(
                    "start_{}",
                    this_loop_label
                ))];
                instructions.append(&mut body.convert(context)?);

                instructions.push(BirdsInstructionNode::Label(format!(
                    "continue_{}",
                    this_loop_label
                )));
                let (mut instructions_from_condition, new_src) = expression.convert(context)?;
                instructions.append(&mut instructions_from_condition);

                instructions.push(BirdsInstructionNode::JumpNotZero(
                    new_src,
                    format!("start_{}", this_loop_label),
                ));
                instructions.push(BirdsInstructionNode::Label(format!(
                    "break_{}",
                    this_loop_label
                )));

                Ok(instructions)
            }
            StatementNode::For(init, cond, post, body, label) => {
                let this_loop_label = label.ok_or::<Box<dyn Error>>("For has no label".into())?;

                //init
                let mut instructions = init.convert(context)?;

                //cond
                instructions.push(BirdsInstructionNode::Label(format!(
                    "start_{}",
                    this_loop_label
                )));
                let (mut instructions_from_condition, new_condition_option) =
                    cond.convert(context)?;
                instructions.append(&mut instructions_from_condition);
                //only check the condition... if the condition was specified
                if let Some(new_condition) = new_condition_option {
                    instructions.push(BirdsInstructionNode::JumpZero(
                        new_condition,
                        format!("break_{}", this_loop_label),
                    ));
                }

                //body
                instructions.append(&mut body.convert(context)?);

                //post
                instructions.push(BirdsInstructionNode::Label(format!(
                    "continue_{}",
                    this_loop_label
                )));
                let (mut instructions_from_post, _) = post.convert(context)?;
                instructions.append(&mut instructions_from_post);
                instructions.push(BirdsInstructionNode::Jump(format!(
                    "start_{}",
                    this_loop_label
                )));

                //exit
                instructions.push(BirdsInstructionNode::Label(format!(
                    "break_{}",
                    this_loop_label
                )));

                Ok(instructions)
            }
            StatementNode::Switch(expression, body, label, map) => {
                let mut label_map = map.ok_or::<Box<dyn Error>>("Switch missing map".into())?;
                let this_name = label.ok_or::<Box<dyn Error>>("Switch missing label".into())?;
                let last_jump = match label_map.remove(&SwitchMapKey::Default) {
                    Some(name) => BirdsInstructionNode::Jump(name),
                    None => BirdsInstructionNode::Jump(format!("break_{}", this_name)),
                };

                let (mut instructions, new_src) = expression.convert(context)?;

                context.last_stack_number += 1;
                let new_tmp_results =
                    BirdsValueNode::Var(format!("stack.{}", context.last_stack_number));

                for (k, v) in label_map {
                    if let SwitchMapKey::Expression(e) = k {
                        let (mut instructions_from_expression, result) = e.convert(context)?;
                        instructions.append(&mut instructions_from_expression);
                        instructions.append(&mut vec![
                            BirdsInstructionNode::Binary(
                                BirdsBinaryOperatorNode::Equal,
                                new_src.clone(),
                                result,
                                new_tmp_results.clone(),
                            ),
                            BirdsInstructionNode::JumpNotZero(new_tmp_results.clone(), v.clone()),
                        ])
                    } else {
                        return Err(format!("Unexpected key in switch case map: {:?}", k).into());
                    }
                }
                instructions.push(last_jump);

                instructions.append(&mut body.convert(context)?);
                instructions.push(BirdsInstructionNode::Label(format!("break_{}", this_name)));

                Ok(instructions)
            }
            StatementNode::Case(_, statement, label) => {
                let mut instructions = vec![BirdsInstructionNode::Label(
                    label.ok_or("Case missing label")?,
                )];
                instructions.append(&mut statement.convert(context)?);

                Ok(instructions)
            }
            StatementNode::Default(statement, label) => {
                let mut instructions = vec![BirdsInstructionNode::Label(
                    label.ok_or("Default missing label")?,
                )];
                instructions.append(&mut statement.convert(context)?);

                Ok(instructions)
            }
        }
    }
}

impl Convert for ForInitialiserNode {
    type Output = Vec<BirdsInstructionNode>;

    fn convert(self, context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>> {
        match self {
            ForInitialiserNode::Declaration(d) => Ok(d.convert(context)?),
            ForInitialiserNode::Expression(optional_expression) => {
                Ok(optional_expression.convert(context)?.0)
            }
        }
    }
}

impl Convert for VariableDeclaration {
    type Output = Vec<BirdsInstructionNode>;

    fn convert(self, context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>> {
        match self.init {
            Some(e) => {
                let assignment_expression = ExpressionNode::Assignment(
                    Box::new(ExpressionNode::Var(self.name)),
                    Box::new(e),
                );
                let (instructions, _new_src) = assignment_expression.convert(context)?;
                Ok(instructions)
            }
            None => Ok(Vec::new()),
        }
    }
}

impl Convert for Option<ExpressionNode> {
    // discard the return value of this expression, since they are only
    // used in for loop expressions which don't need them
    type Output = (Vec<BirdsInstructionNode>, Option<BirdsValueNode>);

    fn convert(self, context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>> {
        match self {
            Some(e) => {
                let (instructions, value) = e.convert(context)?;
                Ok((instructions, Some(value)))
            }
            None => Ok((Vec::new(), None)),
        }
    }
}

impl Convert for ExpressionNode {
    // outputs a list of instructions, and the location of the output of the instructions.
    type Output = (Vec<BirdsInstructionNode>, BirdsValueNode);

    fn convert(
        self,
        context: &mut ConvertContext,
    ) -> Result<(Vec<BirdsInstructionNode>, BirdsValueNode), Box<dyn Error>> {
        match self {
            ExpressionNode::IntegerConstant(c) => {
                Ok((Vec::new(), BirdsValueNode::IntegerConstant(c)))
            }
            ExpressionNode::Var(name) => Ok((Vec::new(), BirdsValueNode::Var(name))),
            ExpressionNode::Assignment(left, right) => {
                // The LHS of this expression should *always* be of type ExpressionNode::Var(name), but
                // it's better to account for other cases here so that that assumption doesn't
                // cause errors in the future. At the moment, this guarantee is given to us by the
                // parser package.
                let (mut instructions, new_dst) = left.convert(context)?;
                let (mut instructions_from_src, new_src) = right.convert(context)?;
                instructions.append(&mut instructions_from_src);

                instructions.push(BirdsInstructionNode::Copy(new_src.clone(), new_dst.clone()));

                Ok((instructions, new_dst))
            }
            ExpressionNode::Unary(UnaryOperatorNode::PrefixIncrement, src) => {
                let (mut instructions, new_src) = src.convert(context)?;
                context.last_stack_number += 1;
                let new_dst = BirdsValueNode::Var(format!("stack.{}", context.last_stack_number));
                instructions.push(BirdsInstructionNode::Binary(
                    BirdsBinaryOperatorNode::Add,
                    new_src.clone(),
                    BirdsValueNode::IntegerConstant(1),
                    new_dst.clone(),
                ));
                instructions.push(BirdsInstructionNode::Copy(new_dst.clone(), new_src.clone()));
                Ok((instructions, new_dst))
            }
            ExpressionNode::Unary(UnaryOperatorNode::PrefixDecrement, src) => {
                let (mut instructions, new_src) = src.convert(context)?;
                context.last_stack_number += 1;
                let new_dst = BirdsValueNode::Var(format!("stack.{}", context.last_stack_number));
                instructions.push(BirdsInstructionNode::Binary(
                    BirdsBinaryOperatorNode::Subtract,
                    new_src.clone(),
                    BirdsValueNode::IntegerConstant(1),
                    new_dst.clone(),
                ));
                instructions.push(BirdsInstructionNode::Copy(new_dst.clone(), new_src.clone()));
                Ok((instructions, new_dst))
            }
            ExpressionNode::Unary(UnaryOperatorNode::SuffixIncrement, src) => {
                let (mut instructions, new_src) = src.convert(context)?;
                context.last_stack_number += 1;
                let new_dst = BirdsValueNode::Var(format!("stack.{}", context.last_stack_number));
                instructions.push(BirdsInstructionNode::Copy(new_src.clone(), new_dst.clone()));
                instructions.push(BirdsInstructionNode::Binary(
                    BirdsBinaryOperatorNode::Add,
                    new_dst.clone(),
                    BirdsValueNode::IntegerConstant(1),
                    new_src.clone(),
                ));
                Ok((instructions, new_dst))
            }
            ExpressionNode::Unary(UnaryOperatorNode::SuffixDecrement, src) => {
                let (mut instructions, new_src) = src.convert(context)?;
                context.last_stack_number += 1;
                let new_dst = BirdsValueNode::Var(format!("stack.{}", context.last_stack_number));
                instructions.push(BirdsInstructionNode::Copy(new_src.clone(), new_dst.clone()));
                instructions.push(BirdsInstructionNode::Binary(
                    BirdsBinaryOperatorNode::Subtract,
                    new_dst.clone(),
                    BirdsValueNode::IntegerConstant(1),
                    new_src.clone(),
                ));
                Ok((instructions, new_dst))
            }
            ExpressionNode::Unary(op, src) => {
                context.last_stack_number += 1;
                let new_dst = BirdsValueNode::Var(format!("stack.{}", context.last_stack_number));
                let bird_op = match op {
                    UnaryOperatorNode::Complement => BirdsUnaryOperatorNode::Complement,
                    UnaryOperatorNode::Negate => BirdsUnaryOperatorNode::Negate,
                    UnaryOperatorNode::Not => BirdsUnaryOperatorNode::Not,
                    UnaryOperatorNode::PrefixIncrement
                    | UnaryOperatorNode::PrefixDecrement
                    | UnaryOperatorNode::SuffixIncrement
                    | UnaryOperatorNode::SuffixDecrement => unreachable!(),
                };

                let (mut instructions, new_src) = src.convert(context)?;

                instructions.push(BirdsInstructionNode::Unary(
                    bird_op,
                    new_src,
                    new_dst.clone(),
                ));
                Ok((instructions, new_dst))
            }
            ExpressionNode::Binary(op, left, right) => {
                let (mut instructions, new_left) = left.convert(context)?;

                let (mut instructions_from_right, new_right) = right.convert(context)?;

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
                    BinaryOperatorNode::BitwiseAnd => Some(BirdsBinaryOperatorNode::BitwiseAnd),
                    BinaryOperatorNode::BitwiseXor => Some(BirdsBinaryOperatorNode::BitwiseXor),
                    BinaryOperatorNode::BitwiseOr => Some(BirdsBinaryOperatorNode::BitwiseOr),
                    BinaryOperatorNode::ShiftLeft => Some(BirdsBinaryOperatorNode::ShiftLeft),
                    BinaryOperatorNode::ShiftRight => Some(BirdsBinaryOperatorNode::ShiftRight),
                    // process these in the 'else' block below instead
                    BinaryOperatorNode::And | BinaryOperatorNode::Or => None,
                };
                if let Some(bird_op_found) = bird_op {
                    instructions.append(&mut instructions_from_right);
                    instructions.push(BirdsInstructionNode::Binary(
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
                            instructions.push(BirdsInstructionNode::JumpNotZero(
                                new_left,
                                true_label_name.clone(),
                            ));

                            instructions.append(&mut instructions_from_right);

                            instructions.append(&mut vec![
                                BirdsInstructionNode::JumpNotZero(
                                    new_right,
                                    true_label_name.clone(),
                                ),
                                BirdsInstructionNode::Copy(
                                    BirdsValueNode::IntegerConstant(0),
                                    new_dst.clone(),
                                ),
                                BirdsInstructionNode::Jump(end_label_name.clone()),
                                BirdsInstructionNode::Label(true_label_name),
                                BirdsInstructionNode::Copy(
                                    BirdsValueNode::IntegerConstant(1),
                                    new_dst.clone(),
                                ),
                                BirdsInstructionNode::Label(end_label_name),
                            ]);
                        }
                        BinaryOperatorNode::And => {
                            instructions.push(BirdsInstructionNode::JumpZero(
                                new_left,
                                false_label_name.clone(),
                            ));

                            instructions.append(&mut instructions_from_right);

                            instructions.append(&mut vec![
                                BirdsInstructionNode::JumpZero(new_right, false_label_name.clone()),
                                BirdsInstructionNode::Copy(
                                    BirdsValueNode::IntegerConstant(1),
                                    new_dst.clone(),
                                ),
                                BirdsInstructionNode::Jump(end_label_name.clone()),
                                BirdsInstructionNode::Label(false_label_name),
                                BirdsInstructionNode::Copy(
                                    BirdsValueNode::IntegerConstant(0),
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
            ExpressionNode::Ternary(condition, then, otherwise) => {
                context.last_end_label_number += 1;
                let new_end_label_name = format!("end_{}", context.last_end_label_number);
                context.last_else_label_number += 1;
                let new_else_label_name = format!("else_{}", context.last_else_label_number);
                context.last_stack_number += 1;
                let new_dst = BirdsValueNode::Var(format!("stack.{}", context.last_stack_number));

                let (mut instructions, new_cond) = condition.convert(context)?;
                instructions.push(BirdsInstructionNode::JumpZero(
                    new_cond,
                    new_else_label_name.clone(),
                ));
                let (mut instructions_from_then, new_then) = then.convert(context)?;
                instructions.append(&mut instructions_from_then);
                instructions.append(&mut vec![
                    BirdsInstructionNode::Copy(new_then.clone(), new_dst.clone()),
                    BirdsInstructionNode::Jump(new_end_label_name.clone()),
                    BirdsInstructionNode::Label(new_else_label_name),
                ]);
                let (mut instructions_from_other, new_other) = otherwise.convert(context)?;
                instructions.append(&mut instructions_from_other);
                instructions.push(BirdsInstructionNode::Copy(new_other, new_dst.clone()));
                instructions.push(BirdsInstructionNode::Label(new_end_label_name));

                Ok((instructions, new_dst))
            }
            ExpressionNode::FunctionCall(_, _) => todo!(),
        }
    }
}

impl BirdsBinaryOperatorNode {
    pub fn is_relational(&self) -> bool {
        matches!(
            self,
            BirdsBinaryOperatorNode::Equal
                | BirdsBinaryOperatorNode::NotEqual
                | BirdsBinaryOperatorNode::Greater
                | BirdsBinaryOperatorNode::GreaterEqual
                | BirdsBinaryOperatorNode::Less
                | BirdsBinaryOperatorNode::LessEqual
        )
    }
}
