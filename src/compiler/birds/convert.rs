use std::error::Error;

use itertools::process_results;

use crate::compiler::parser::{
    BinaryOperatorNode, Block, BlockItemNode, Constant, DeclarationNode, ExpressionNode,
    ExpressionWithoutType, ForInitialiserNode, FunctionDeclaration, InitialValue, ProgramNode,
    StatementNode, StaticInitial, StorageInfo, SwitchMapKey, SymbolInfo, Type, UnaryOperatorNode,
    VariableDeclaration,
};

use super::{
    BirdsBinaryOperatorNode, BirdsInstructionNode, BirdsProgramNode, BirdsTopLevel,
    BirdsUnaryOperatorNode, BirdsValueNode, Convert, ConvertContext,
};

fn new_temp_variable(type_to_store: Type, context: &mut ConvertContext) -> BirdsValueNode {
    context.last_stack_number += 1;
    let new_name = format!("stack.{}", context.last_stack_number);
    context.symbols.insert(
        new_name.clone(),
        SymbolInfo {
            symbol_type: type_to_store,
            storage: StorageInfo::Automatic,
        },
    );

    BirdsValueNode::Var(new_name)
}

fn get_typed_constant(value: u32, target: &ExpressionNode) -> BirdsValueNode {
    match target.1.as_ref().unwrap() {
        Type::Integer => BirdsValueNode::Constant(Constant::Integer(value.try_into().unwrap())),
        Type::Long => BirdsValueNode::Constant(Constant::Long(value.into())),
        Type::UnsignedInteger => BirdsValueNode::Constant(Constant::UnsignedInteger(value)),
        Type::UnsignedLong => BirdsValueNode::Constant(Constant::UnsignedLong(value.into())),
        Type::Function(_, _) => unreachable!(),
    }
}

impl Convert for ProgramNode {
    type Output = BirdsProgramNode;

    fn convert(self, context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>> {
        let mut body: Vec<BirdsTopLevel> = process_results(
            self.declarations
                .into_iter()
                .filter_map(|declaration| match declaration {
                    DeclarationNode::Variable(_) => None,
                    DeclarationNode::Function(f) => Some(f.convert(context)),
                }),
            |iter| iter.flatten().collect(),
        )?;
        body.append(
            &mut context
                .symbols
                .iter_mut()
                .filter_map(|(name, info)| match &info.storage {
                    StorageInfo::Static(init, global) => {
                        let initial = match init {
                            InitialValue::Tentative => {
                                Constant::convert_to(&Constant::Integer(0), &info.symbol_type)
                            }
                            InitialValue::Initial(i) => i.clone(),
                            InitialValue::None => return None,
                        };
                        Some(BirdsTopLevel::StaticVariable(
                            info.symbol_type.clone(),
                            name.clone(),
                            initial,
                            *global,
                        ))
                    }
                    _ => None,
                })
                .collect(),
        );
        Ok(BirdsProgramNode { body })
    }
}

impl Convert for FunctionDeclaration {
    type Output = Option<BirdsTopLevel>;

    fn convert(self, context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>> {
        let name = self.name;
        let params = self.params.to_vec();
        if let Some(body) = self.body {
            let mut instructions = body.convert(context)?;
            // add an extra "return 0;" at the end because the C standard dictates that if main() exits
            // without a return statement, then it must actually return 0. If a return statement is
            // otherwise present, this instruction will never be run (dead code).
            instructions.push(BirdsInstructionNode::Return(BirdsValueNode::Constant(
                Constant::Integer(0),
            )));
            if let StorageInfo::Function(_defined, global) =
                context.symbols.get(&name).unwrap().storage
            {
                Ok(Some(BirdsTopLevel::Function(
                    name,
                    params,
                    instructions,
                    global,
                )))
            } else {
                panic!("Symbol info missing from map for defined function")
            }
        } else {
            Ok(None)
        }
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
            // function declaration at the top scope does not use this path, since it is read
            // directly in ProgramNode.
            // function declarations without a body do not emit instructions, so this branch is
            // ignored entirely.
            DeclarationNode::Function(_f) => Ok(Vec::new()),
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
                let new_condition_option = cond.convert(context)?;
                //only check the condition... if the condition was specified
                if let Some(mut new_condition) = new_condition_option {
                    instructions.append(&mut new_condition.0);
                    instructions.push(BirdsInstructionNode::JumpZero(
                        new_condition.1,
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
                let maybe_post = post.convert(context)?;
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

                let new_dst_type = expression.1.clone().unwrap();
                let (mut instructions, new_src) = expression.convert(context)?;
                let new_tmp_results = new_temp_variable(new_dst_type, context);

                for (k, v) in label_map {
                    if let SwitchMapKey::Constant(c) = k {
                        // let (mut instructions_from_expression, result) = c.convert(context)?;
                        // instructions.append(&mut instructions_from_expression);
                        instructions.append(&mut vec![
                            BirdsInstructionNode::Binary(
                                BirdsBinaryOperatorNode::Equal,
                                new_src.clone(),
                                c.convert(context)?,
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

impl Convert for StaticInitial {
    type Output = BirdsValueNode;

    fn convert(self, _context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>> {
        match self {
            StaticInitial::Integer(i) => Ok(BirdsValueNode::Constant(Constant::Integer(i))),
            StaticInitial::Long(l) => Ok(BirdsValueNode::Constant(Constant::Long(l))),
            StaticInitial::UnsignedInteger(i) => {
                Ok(BirdsValueNode::Constant(Constant::UnsignedInteger(i)))
            }
            StaticInitial::UnsignedLong(l) => {
                Ok(BirdsValueNode::Constant(Constant::UnsignedLong(l)))
            }
        }
    }
}

impl Convert for ForInitialiserNode {
    type Output = Vec<BirdsInstructionNode>;

    fn convert(self, context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>> {
        match self {
            ForInitialiserNode::Declaration(d) => Ok(d.convert(context)?),
            ForInitialiserNode::Expression(Some(expression)) => Ok(expression.convert(context)?.0),
            ForInitialiserNode::Expression(None) => Ok(Vec::new()),
        }
    }
}

impl Convert for VariableDeclaration {
    type Output = Vec<BirdsInstructionNode>;

    fn convert(self, context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>> {
        // do not emit any instructions for static and extern variable definitions. These are
        // handled after the rest of the ProgramNode has been converted.
        if self.storage_class.is_some() {
            return Ok(Vec::new());
        }

        match self.init {
            Some(e) => {
                let output_type = e.1.clone();
                let assignment_expression = ExpressionNode(
                    ExpressionWithoutType::Assignment(
                        Box::new(ExpressionWithoutType::Var(self.name).into()),
                        Box::new(e),
                    ),
                    output_type,
                );
                let (instructions, _new_src) = assignment_expression.convert(context)?;
                Ok(instructions)
            }
            None => Ok(Vec::new()),
        }
    }
}

impl<T, U, V> Convert for Option<T>
where
    T: Convert<Output = (U, V)>,
{
    type Output = Option<(U, V)>;

    fn convert(self, context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>> {
        match self {
            Some(e) => {
                let (instructions, value) = e.convert(context)?;
                Ok(Some((instructions, value)))
            }
            None => Ok(None),
        }
    }
}
impl Convert for Vec<ExpressionNode> {
    type Output = (Vec<BirdsInstructionNode>, Vec<BirdsValueNode>);

    fn convert(self, context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>> {
        let results: Vec<(Vec<BirdsInstructionNode>, BirdsValueNode)> =
            process_results(self.into_iter().map(|a| a.convert(context)), |iter| {
                iter.collect()
            })?;

        let values = results.iter().map(|a| a.1.clone()).collect();
        let instructions = results.into_iter().flat_map(|a| a.0).collect();

        Ok((instructions, values))
    }
}

impl Convert for ExpressionNode {
    // outputs a list of instructions, and the location of the output of the instructions.
    type Output = (Vec<BirdsInstructionNode>, BirdsValueNode);

    fn convert(
        self,
        context: &mut ConvertContext,
    ) -> Result<(Vec<BirdsInstructionNode>, BirdsValueNode), Box<dyn Error>> {
        match self.0 {
            ExpressionWithoutType::Constant(c) => Ok((Vec::new(), BirdsValueNode::Constant(c))),
            ExpressionWithoutType::Var(name) => Ok((Vec::new(), BirdsValueNode::Var(name))),
            ExpressionWithoutType::Assignment(left, right) => {
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
            ExpressionWithoutType::Unary(UnaryOperatorNode::PrefixIncrement, src) => {
                let new_dst_type = src.1.clone().unwrap();
                let (mut instructions, new_src) = src.clone().convert(context)?;
                let new_dst = new_temp_variable(new_dst_type, context);

                instructions.push(BirdsInstructionNode::Binary(
                    BirdsBinaryOperatorNode::Add,
                    new_src.clone(),
                    get_typed_constant(1, &src),
                    new_dst.clone(),
                ));
                instructions.push(BirdsInstructionNode::Copy(new_dst.clone(), new_src.clone()));
                Ok((instructions, new_dst))
            }
            ExpressionWithoutType::Unary(UnaryOperatorNode::PrefixDecrement, src) => {
                let new_dst_type = src.1.clone().unwrap();
                let (mut instructions, new_src) = src.clone().convert(context)?;
                let new_dst = new_temp_variable(new_dst_type, context);

                instructions.push(BirdsInstructionNode::Binary(
                    BirdsBinaryOperatorNode::Subtract,
                    new_src.clone(),
                    get_typed_constant(1, &src),
                    new_dst.clone(),
                ));
                instructions.push(BirdsInstructionNode::Copy(new_dst.clone(), new_src.clone()));
                Ok((instructions, new_dst))
            }
            ExpressionWithoutType::Unary(UnaryOperatorNode::SuffixIncrement, src) => {
                let new_dst_type = src.1.clone().unwrap();
                let (mut instructions, new_src) = src.clone().convert(context)?;
                let new_dst = new_temp_variable(new_dst_type, context);

                instructions.push(BirdsInstructionNode::Copy(new_src.clone(), new_dst.clone()));
                instructions.push(BirdsInstructionNode::Binary(
                    BirdsBinaryOperatorNode::Add,
                    new_dst.clone(),
                    get_typed_constant(1, &src),
                    new_src.clone(),
                ));
                Ok((instructions, new_dst))
            }
            ExpressionWithoutType::Unary(UnaryOperatorNode::SuffixDecrement, src) => {
                let new_dst_type = src.1.clone().unwrap();
                let (mut instructions, new_src) = src.clone().convert(context)?;
                let new_dst = new_temp_variable(new_dst_type, context);

                instructions.push(BirdsInstructionNode::Copy(new_src.clone(), new_dst.clone()));
                instructions.push(BirdsInstructionNode::Binary(
                    BirdsBinaryOperatorNode::Subtract,
                    new_dst.clone(),
                    get_typed_constant(1, &src),
                    new_src.clone(),
                ));
                Ok((instructions, new_dst))
            }
            ExpressionWithoutType::Unary(op, src) => {
                let new_dst_type = src.1.clone().unwrap();
                let (mut instructions, new_src) = src.clone().convert(context)?;
                let new_dst = new_temp_variable(new_dst_type, context);

                let bird_op = match op {
                    UnaryOperatorNode::Complement => BirdsUnaryOperatorNode::Complement,
                    UnaryOperatorNode::Negate => BirdsUnaryOperatorNode::Negate,
                    UnaryOperatorNode::Not => BirdsUnaryOperatorNode::Not,
                    UnaryOperatorNode::PrefixIncrement
                    | UnaryOperatorNode::PrefixDecrement
                    | UnaryOperatorNode::SuffixIncrement
                    | UnaryOperatorNode::SuffixDecrement => unreachable!(),
                };

                instructions.push(BirdsInstructionNode::Unary(
                    bird_op,
                    new_src,
                    new_dst.clone(),
                ));
                Ok((instructions, new_dst))
            }
            ExpressionWithoutType::Binary(op, left, right) => {
                let mut new_dst_type = left.1.clone().unwrap();
                let (mut instructions, new_left) = left.convert(context)?;

                let (mut instructions_from_right, new_right) = right.convert(context)?;

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
                let new_dst = if let Some(bird_op_found) = bird_op {
                    let new_dst = new_temp_variable(new_dst_type, context);
                    instructions.append(&mut instructions_from_right);
                    instructions.push(BirdsInstructionNode::Binary(
                        bird_op_found,
                        new_left,
                        new_right,
                        new_dst.clone(),
                    ));
                    new_dst
                } else {
                    // 'And' and 'Or' always return ints
                    new_dst_type = Type::Integer;
                    let new_dst = new_temp_variable(new_dst_type, context);

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
                                    BirdsValueNode::Constant(Constant::Integer(0)),
                                    new_dst.clone(),
                                ),
                                BirdsInstructionNode::Jump(end_label_name.clone()),
                                BirdsInstructionNode::Label(true_label_name),
                                BirdsInstructionNode::Copy(
                                    BirdsValueNode::Constant(Constant::Integer(1)),
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
                                    BirdsValueNode::Constant(Constant::Integer(1)),
                                    new_dst.clone(),
                                ),
                                BirdsInstructionNode::Jump(end_label_name.clone()),
                                BirdsInstructionNode::Label(false_label_name),
                                BirdsInstructionNode::Copy(
                                    BirdsValueNode::Constant(Constant::Integer(0)),
                                    new_dst.clone(),
                                ),
                                BirdsInstructionNode::Label(end_label_name),
                            ]);
                        }
                        _ => panic!("Unexpected binary operator found: {:?}", op),
                    };
                    new_dst
                };
                Ok((instructions, new_dst))
            }
            ExpressionWithoutType::Ternary(condition, then, otherwise) => {
                context.last_end_label_number += 1;
                let new_end_label_name = format!("end_{}", context.last_end_label_number);
                context.last_else_label_number += 1;
                let new_else_label_name = format!("else_{}", context.last_else_label_number);

                let (mut instructions, new_cond) = condition.convert(context)?;
                instructions.push(BirdsInstructionNode::JumpZero(
                    new_cond,
                    new_else_label_name.clone(),
                ));

                let new_dst_type = then.1.clone().unwrap();
                let (mut instructions_from_then, new_then) = then.clone().convert(context)?;
                let new_dst = new_temp_variable(new_dst_type, context);

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
            ExpressionWithoutType::FunctionCall(name, args) => {
                let (mut instructions, values) = args.convert(context)?;

                let new_dst_type = if let Type::Function(ref ret, _) =
                    context.symbols.get(&name).unwrap().symbol_type
                {
                    ret.as_ref().clone()
                } else {
                    return Err(
                        "Function stored in symbol table does not have function type".into(),
                    );
                };
                let new_dst = new_temp_variable(new_dst_type, context);
                instructions.push(BirdsInstructionNode::FunctionCall(
                    name,
                    values,
                    new_dst.clone(),
                ));

                Ok((instructions, new_dst))
            }
            ExpressionWithoutType::Cast(target_type, e) => {
                match (target_type, e.1.as_ref().unwrap()) {
                    (a, b) if a == *b => Ok(e.convert(context)?),
                    (Type::Integer, Type::Long) => {
                        let (mut instructions, new_src) = e.convert(context)?;

                        let new_dst = new_temp_variable(Type::Integer, context);

                        instructions.push(BirdsInstructionNode::Truncate(new_src, new_dst.clone()));
                        Ok((instructions, new_dst))
                    }
                    (Type::Long, Type::Integer) => {
                        let (mut instructions, new_src) = e.convert(context)?;

                        let new_dst = new_temp_variable(Type::Long, context);

                        instructions
                            .push(BirdsInstructionNode::SignedExtend(new_src, new_dst.clone()));
                        Ok((instructions, new_dst))
                    }
                    (Type::Integer, Type::UnsignedInteger) => todo!(),
                    (Type::Integer, Type::UnsignedLong) => todo!(),
                    (Type::Long, Type::UnsignedInteger) => todo!(),
                    (Type::Long, Type::UnsignedLong) => todo!(),
                    (Type::UnsignedInteger, Type::Integer) => todo!(),
                    (Type::UnsignedInteger, Type::Long) => todo!(),
                    (Type::UnsignedInteger, Type::UnsignedInteger) => todo!(),
                    (Type::UnsignedInteger, Type::UnsignedLong) => todo!(),
                    (Type::UnsignedLong, Type::Integer) => todo!(),
                    (Type::UnsignedLong, Type::Long) => todo!(),
                    (Type::UnsignedLong, Type::UnsignedInteger) => todo!(),
                    (Type::UnsignedLong, Type::UnsignedLong) => todo!(),
                    (_, Type::Function(_, _)) => unreachable!(),
                    (Type::Function(_, _), _) => unreachable!(),
                    (Type::Integer, Type::Integer) => unreachable!(),
                    (Type::Long, Type::Long) => unreachable!(),
                }
            }
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
