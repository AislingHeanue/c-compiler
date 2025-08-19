use std::error::Error;

use crate::compiler::{
    birds::{BirdsBinaryOperatorNode, BirdsInstructionNode, BirdsValueNode},
    parser::{
        ExpressionNode, ExpressionWithoutType, ForInitialiserNode, StatementNode, SwitchMapKey,
    },
    types::Type,
};

use super::{
    expression_node::{D, E},
    new_temp_variable, Convert, ConvertContext,
};

impl Convert<Vec<BirdsInstructionNode>> for StatementNode {
    fn convert(
        self,
        context: &mut ConvertContext,
    ) -> Result<Vec<BirdsInstructionNode>, Box<dyn Error>> {
        match self {
            StatementNode::Return(expression) => {
                if let Some(e) = expression {
                    let (mut instructions, new_src) = e.convert(context)?;
                    let return_instruction = BirdsInstructionNode::Return(Some(new_src));
                    instructions.push(return_instruction);
                    Ok(instructions)
                } else {
                    let return_instruction = BirdsInstructionNode::Return(None);
                    Ok(vec![return_instruction])
                }
            }
            StatementNode::Expression(expression) => {
                // expressions (including assignments) all have return values that are thrown away
                // as we move onto the next line. eg. "a = 5" returns 5 AND ALSO assigns 5 to a.
                let (instructions, _new_src): D = expression.convert(context)?;
                Ok(instructions)
            }
            StatementNode::Pass => Ok::<Vec<BirdsInstructionNode>, Box<dyn Error>>(Vec::new()),
            StatementNode::If(condition, then, otherwise) => {
                if let Some(other) = *otherwise {
                    context.last_end_label_number += 1;
                    let new_end_label_name = format!("end_{}", context.last_end_label_number);
                    context.last_else_label_number += 1;
                    let new_else_label_name = format!("else_{}", context.last_else_label_number);

                    let mut instructions =
                        condition.convert_condition(new_else_label_name.clone(), context)?;
                    instructions.append(&mut then.convert(context)?);
                    instructions.push(BirdsInstructionNode::Jump(new_end_label_name.clone()));
                    instructions.push(BirdsInstructionNode::Label(new_else_label_name));
                    instructions.append(&mut other.convert(context)?);
                    instructions.push(BirdsInstructionNode::Label(new_end_label_name));

                    Ok(instructions)
                } else {
                    context.last_end_label_number += 1;
                    let new_end_label_name = format!("end_{}", context.last_end_label_number);
                    let mut instructions =
                        condition.convert_condition(new_end_label_name.clone(), context)?;

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
                instructions.append(
                    &mut expression
                        .convert_condition(format!("break_{}", this_loop_label), context)?,
                );

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

                instructions.append(
                    &mut expression
                        .convert_condition(format!("break_{}", this_loop_label), context)?,
                );

                instructions.push(BirdsInstructionNode::Jump(format!(
                    "start_{}",
                    this_loop_label
                )));
                instructions.push(BirdsInstructionNode::Label(format!(
                    "break_{}",
                    this_loop_label
                )));

                Ok(instructions)
            }
            StatementNode::For(init, cond, post, body, label) => {
                let this_loop_label = label.ok_or::<Box<dyn Error>>("For has no label".into())?;

                //init
                let mut instructions = match init {
                    ForInitialiserNode::Declaration(d) => d.convert(context)?,
                    ForInitialiserNode::Expression(Some(expression)) => {
                        let (instructions, _): D = expression.convert(context)?;
                        instructions
                    }
                    ForInitialiserNode::Expression(None) => Vec::new(),
                };

                //cond
                instructions.push(BirdsInstructionNode::Label(format!(
                    "start_{}",
                    this_loop_label
                )));
                //only check the condition... if the condition was specified
                if let Some(condition) = cond {
                    instructions.append(
                        &mut condition
                            .convert_condition(format!("break_{}", this_loop_label), context)?,
                    );
                }

                //body
                instructions.append(&mut body.convert(context)?);

                //post
                instructions.push(BirdsInstructionNode::Label(format!(
                    "continue_{}",
                    this_loop_label
                )));
                let maybe_post: Option<D> = post.convert(context)?;
                if let Some(mut post) = maybe_post {
                    instructions.append(&mut post.0);
                }
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

                let (mut instructions, new_src): E = expression.convert(context)?;
                let new_tmp_results = new_temp_variable(&Type::Integer, context);

                for (k, v) in label_map {
                    if let SwitchMapKey::Constant(c) = k {
                        instructions.append(&mut vec![
                            BirdsInstructionNode::Binary(
                                BirdsBinaryOperatorNode::Equal,
                                new_src.clone(),
                                BirdsValueNode::Constant(c.to_constant()),
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

impl ExpressionNode {
    fn convert_condition(
        self,
        jump_to_on_false: String,
        context: &mut ConvertContext,
    ) -> Result<Vec<BirdsInstructionNode>, Box<dyn Error>> {
        let mut instructions = Vec::new();
        match self.0 {
            ExpressionWithoutType::Binary(op, left, right)
                if op.clone().convert(context)?.is_relational() =>
            {
                let bird_op = op.convert(context)?.negated();
                let (mut instructions_from_left, new_left): E = left.convert(context)?;
                instructions.append(&mut instructions_from_left);
                let (mut instructions_from_right, new_right): E = right.convert(context)?;
                instructions.append(&mut instructions_from_right);
                instructions.push(BirdsInstructionNode::JumpCondition(
                    bird_op,
                    new_left,
                    new_right,
                    jump_to_on_false,
                ))
            }
            _ => {
                let (mut instructions_from_cond, new_cond): E = self.convert(context)?;
                instructions.append(&mut instructions_from_cond);
                instructions.push(BirdsInstructionNode::JumpZero(new_cond, jump_to_on_false));
            }
        }
        Ok(instructions)
    }
}
