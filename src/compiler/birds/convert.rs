use std::error::Error;

use itertools::process_results;

use crate::compiler::{
    parser::{
        BinaryOperatorNode, Block, BlockItemNode, DeclarationNode, ExpressionNode,
        ExpressionWithoutType, ForInitialiserNode, FunctionDeclaration, ProgramNode, StatementNode,
        SwitchMapKey, UnaryOperatorNode, VariableDeclaration,
    },
    types::{Constant, InitialValue, OrdinalStatic, StaticInitial, StorageInfo, SymbolInfo, Type},
};

use super::{
    BirdsBinaryOperatorNode, BirdsInstructionNode, BirdsProgramNode, BirdsTopLevel,
    BirdsUnaryOperatorNode, BirdsValueNode, Convert, ConvertContext, ConvertEvaluate, Destination,
};

fn new_temp_variable(type_to_store: &Type, context: &mut ConvertContext) -> BirdsValueNode {
    context.last_stack_number += 1;
    let new_name = format!("stack.{}", context.last_stack_number);
    context.symbols.insert(
        new_name.clone(),
        SymbolInfo {
            symbol_type: type_to_store.clone(),
            storage: StorageInfo::Automatic,
        },
    );

    BirdsValueNode::Var(new_name)
}

// purely-for-utility function for getting constants (almost always 0 or 1) in the appropriate type
fn get_typed_constant(value: u32, target: &Type) -> BirdsValueNode {
    match target {
        Type::Integer => BirdsValueNode::Constant(Constant::Integer(value.try_into().unwrap())),
        Type::Long => BirdsValueNode::Constant(Constant::Long(value.into())),
        Type::UnsignedInteger => BirdsValueNode::Constant(Constant::UnsignedInteger(value)),
        Type::UnsignedLong => BirdsValueNode::Constant(Constant::UnsignedLong(value.into())),
        Type::Double => BirdsValueNode::Constant(Constant::Double(value.into())),
        Type::Array(..) => unreachable!(),
        Type::Pointer(_) => unreachable!(),
        Type::Function(_, _) => unreachable!(),
    }
}

impl Destination {
    fn evaluate(
        self,
        target_type: &Type,
        context: &mut ConvertContext,
    ) -> (Vec<BirdsInstructionNode>, BirdsValueNode) {
        match self {
            Destination::Direct(val) => (Vec::new(), val),
            Destination::Dereference(val) => {
                let new_dst = new_temp_variable(target_type, context);
                (
                    vec![BirdsInstructionNode::LoadFromPointer(val, new_dst.clone())],
                    new_dst,
                )
            }
        }
    }
}

impl From<BirdsValueNode> for Destination {
    fn from(value: BirdsValueNode) -> Self {
        Destination::Direct(value)
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
                            InitialValue::Tentative => Constant::convert_ordinal_to(
                                &Constant::Integer(0),
                                &info.symbol_type,
                            ),
                            InitialValue::Initial(_i) => todo!(), //i.clone(),
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
                let (mut instructions, new_src) = expression.convert_and_evaluate(context)?;
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

                    let (mut instructions, new_cond) = condition.convert_and_evaluate(context)?;
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
                    let (mut instructions, new_cond) = condition.convert_and_evaluate(context)?;
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
                let (mut instructions_from_condition, new_src) =
                    expression.convert_and_evaluate(context)?;
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
                let (mut instructions_from_condition, new_src) =
                    expression.convert_and_evaluate(context)?;
                instructions.append(&mut instructions_from_condition);

                instructions.push(BirdsInstructionNode::JumpZero(
                    new_src,
                    format!("break_{}", this_loop_label),
                ));
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
                let mut instructions = init.convert(context)?;

                //cond
                instructions.push(BirdsInstructionNode::Label(format!(
                    "start_{}",
                    this_loop_label
                )));
                let new_condition_option = cond.convert_and_evaluate(context)?;
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
                let (mut instructions, new_src) = expression.convert_and_evaluate(context)?;
                let new_tmp_results = new_temp_variable(&new_dst_type, context);

                for (k, v) in label_map {
                    if let SwitchMapKey::Constant(c) = k {
                        instructions.append(&mut vec![
                            BirdsInstructionNode::Binary(
                                BirdsBinaryOperatorNode::Equal,
                                new_src.clone(),
                                StaticInitial::Ordinal(c).convert(context)?,
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
            StaticInitial::Ordinal(OrdinalStatic::Integer(i)) => {
                Ok(BirdsValueNode::Constant(Constant::Integer(i)))
            }
            StaticInitial::Ordinal(OrdinalStatic::Long(l)) => {
                Ok(BirdsValueNode::Constant(Constant::Long(l)))
            }
            StaticInitial::Ordinal(OrdinalStatic::UnsignedInteger(i)) => {
                Ok(BirdsValueNode::Constant(Constant::UnsignedInteger(i)))
            }
            StaticInitial::Ordinal(OrdinalStatic::UnsignedLong(l)) => {
                Ok(BirdsValueNode::Constant(Constant::UnsignedLong(l)))
            }
            StaticInitial::Double(d) => Ok(BirdsValueNode::Constant(Constant::Double(d))),
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

    fn convert(self, _context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>> {
        // do not emit any instructions for static and extern variable definitions. These are
        // handled after the rest of the ProgramNode has been converted.
        if self.storage_class.is_some() {
            return Ok(Vec::new());
        }

        match self.init {
            Some(_e) => {
                todo!()
                // let output_type = e.1.clone();
                // let assignment_expression = ExpressionNode(
                //     ExpressionWithoutType::Assignment(
                //         Box::new(ExpressionWithoutType::Var(self.name).into()),
                //         Box::new(e),
                //     ),
                //     output_type,
                // );
                // let (instructions, _new_src) = assignment_expression.convert(_context)?;
                // Ok(instructions)
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

impl<T, U, V> ConvertEvaluate for Option<T>
where
    T: ConvertEvaluate<Output = (U, V)>,
{
    type Output = Option<(U, V)>;

    fn convert_and_evaluate(
        self,
        context: &mut ConvertContext,
    ) -> Result<Self::Output, Box<dyn Error>> {
        match self {
            Some(e) => {
                let (instructions, value) = e.convert_and_evaluate(context)?;
                Ok(Some((instructions, value)))
            }
            None => Ok(None),
        }
    }
}
impl Convert for Vec<ExpressionNode> {
    type Output = (Vec<BirdsInstructionNode>, Vec<Destination>);

    fn convert(self, context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>> {
        let results: Vec<(Vec<BirdsInstructionNode>, Destination)> =
            process_results(self.into_iter().map(|a| a.convert(context)), |iter| {
                iter.collect()
            })?;

        let values = results.iter().map(|a| a.1.clone()).collect();
        let instructions = results.into_iter().flat_map(|a| a.0).collect();

        Ok((instructions, values))
    }
}

impl ConvertEvaluate for Vec<ExpressionNode> {
    type Output = (Vec<BirdsInstructionNode>, Vec<BirdsValueNode>);
    fn convert_and_evaluate(
        self,
        context: &mut ConvertContext,
    ) -> Result<Self::Output, Box<dyn Error>> {
        let results: Vec<(Vec<BirdsInstructionNode>, BirdsValueNode)> = process_results(
            self.into_iter().map(|a| a.convert_and_evaluate(context)),
            |iter| iter.collect(),
        )?;

        let values = results.iter().map(|a| a.1.clone()).collect();
        let instructions = results.into_iter().flat_map(|a| a.0).collect();

        Ok((instructions, values))
    }
}

impl Convert for ExpressionNode {
    // outputs a list of instructions, and the location of the output of the instructions.
    type Output = (Vec<BirdsInstructionNode>, Destination);

    fn convert(self, context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>> {
        match self.0 {
            ExpressionWithoutType::Constant(c) => {
                Ok((Vec::new(), Destination::Direct(BirdsValueNode::Constant(c))))
            }
            ExpressionWithoutType::Var(name) => {
                Ok((Vec::new(), Destination::Direct(BirdsValueNode::Var(name))))
            }
            ExpressionWithoutType::Assignment(left, right) => {
                let (mut instructions, new_dst) = left.convert(context)?;
                let (mut instructions_from_src, new_src) = right.convert_and_evaluate(context)?;
                instructions.append(&mut instructions_from_src);
                let (mut instructions_from_assign, returns) =
                    ExpressionWithoutType::assign(new_src, new_dst)?;
                instructions.append(&mut instructions_from_assign);
                Ok((instructions, returns))
            }
            ExpressionWithoutType::Unary(op, src)
                if matches!(
                    op,
                    UnaryOperatorNode::PrefixIncrement | UnaryOperatorNode::PrefixDecrement
                ) =>
            {
                let bird_op = if op == UnaryOperatorNode::PrefixIncrement {
                    BirdsBinaryOperatorNode::Add
                } else {
                    BirdsBinaryOperatorNode::Subtract
                };
                let new_dst_type = src.1.clone().unwrap();

                // How to convert and evaluate an expression which may contain a dereference
                let (mut instructions, new_src) = src.convert(context)?;
                let (mut instructions_from_evaluate, evaluated_src) =
                    new_src.clone().evaluate(&new_dst_type, context);
                instructions.append(&mut instructions_from_evaluate);

                let new_dst = new_temp_variable(&new_dst_type, context);

                instructions.push(BirdsInstructionNode::Binary(
                    bird_op,
                    evaluated_src.clone(),
                    get_typed_constant(1, &new_dst_type),
                    new_dst.clone(),
                ));

                let (mut instructions_from_assign, _returns) =
                    ExpressionWithoutType::assign(new_dst.clone(), new_src)?;
                instructions.append(&mut instructions_from_assign);

                Ok((instructions, new_dst.into()))
            }
            ExpressionWithoutType::Unary(op, src)
                if matches!(
                    op,
                    UnaryOperatorNode::SuffixIncrement | UnaryOperatorNode::SuffixDecrement
                ) =>
            {
                let bird_op = if op == UnaryOperatorNode::SuffixIncrement {
                    BirdsBinaryOperatorNode::Add
                } else {
                    BirdsBinaryOperatorNode::Subtract
                };
                let new_dst_type = src.1.clone().unwrap();

                let (mut instructions, new_src) = src.convert(context)?;
                let (mut instructions_from_evaluate, evaluated_src) =
                    new_src.clone().evaluate(&new_dst_type, context);
                instructions.append(&mut instructions_from_evaluate);

                let new_dst = new_temp_variable(&new_dst_type, context);
                instructions.push(BirdsInstructionNode::Copy(
                    evaluated_src.clone(),
                    new_dst.clone(),
                ));

                instructions.push(BirdsInstructionNode::Binary(
                    bird_op,
                    new_dst.clone(),
                    get_typed_constant(1, &new_dst_type),
                    evaluated_src.clone(),
                ));
                let (mut instructions_from_assign, _returns) =
                    ExpressionWithoutType::assign(evaluated_src.clone(), new_src)?;
                instructions.append(&mut instructions_from_assign);

                Ok((instructions, new_dst.into()))
            }
            ExpressionWithoutType::Unary(op, src) => {
                let src_type = src.1.clone().unwrap();
                let (mut instructions, new_src) = src.convert_and_evaluate(context)?;

                let bird_op = match op {
                    UnaryOperatorNode::Complement => BirdsUnaryOperatorNode::Complement,
                    UnaryOperatorNode::Negate => BirdsUnaryOperatorNode::Negate,
                    UnaryOperatorNode::Not => BirdsUnaryOperatorNode::Not,
                    UnaryOperatorNode::PrefixIncrement
                    | UnaryOperatorNode::PrefixDecrement
                    | UnaryOperatorNode::SuffixIncrement
                    | UnaryOperatorNode::SuffixDecrement => unreachable!(),
                };

                let new_dst_type = if bird_op == BirdsUnaryOperatorNode::Not {
                    Type::Integer
                } else {
                    src_type
                };

                let new_dst = new_temp_variable(&new_dst_type, context);

                instructions.push(BirdsInstructionNode::Unary(
                    bird_op,
                    new_src,
                    new_dst.clone(),
                ));
                Ok((instructions, new_dst.into()))
            }
            ExpressionWithoutType::Compound(op, left, right) => {
                // since this is an assignment, we've already carried out type checking for the
                // return of this expression in the validation step.
                //
                // Compound(Add, left, right) => {
                //      addq left, right
                //      movq right, left
                //      dst = left
                // }
                // a += 5l => a = int(long(a) + 5l)
                let final_type = left.1.clone().unwrap();
                let common_type = self.1.clone().unwrap();

                let (mut instructions, new_left) = left.convert(context)?;

                let (mut instructions_from_dereference, evaluated_left) =
                    new_left.clone().evaluate(&final_type, context);
                instructions.append(&mut instructions_from_dereference);

                let (mut instructions_from_right, new_right) =
                    right.convert_and_evaluate(context)?;
                instructions.append(&mut instructions_from_right);

                let bird_op = op.convert(context)?;
                if common_type == final_type
                    // in bit shift operations, the common type is always the LEFT type
                    || matches!(
                        bird_op,
                        BirdsBinaryOperatorNode::ShiftLeft | BirdsBinaryOperatorNode::ShiftRight
                    )
                {
                    let new_dst = new_temp_variable(&final_type, context);
                    instructions.push(BirdsInstructionNode::Binary(
                        bird_op,
                        evaluated_left,
                        new_right,
                        new_dst.clone(),
                    ));
                    let (mut instructions_from_assign, returns) =
                        ExpressionWithoutType::assign(new_dst, new_left)?;
                    instructions.append(&mut instructions_from_assign);
                    Ok((instructions, returns))
                } else {
                    let casted_left = new_temp_variable(&common_type, context);
                    let mut instructions_from_first_cast = ExpressionWithoutType::cast(
                        evaluated_left.clone(),
                        casted_left.clone(),
                        &final_type,
                        &common_type,
                    )?;
                    instructions.append(&mut instructions_from_first_cast);

                    let new_dst = new_temp_variable(&common_type, context);
                    instructions.push(BirdsInstructionNode::Binary(
                        bird_op,
                        casted_left.clone(),
                        new_right,
                        new_dst.clone(),
                    ));
                    let casted_result = new_temp_variable(&final_type, context);
                    let mut instructions_from_second_cast = ExpressionWithoutType::cast(
                        new_dst.clone(),
                        casted_result.clone(),
                        &common_type,
                        &final_type,
                    )?;
                    println!("{:?}", instructions_from_second_cast);
                    instructions.append(&mut instructions_from_second_cast);
                    let (mut instructions_from_assign, returns) =
                        ExpressionWithoutType::assign(casted_result, new_left)?;
                    instructions.append(&mut instructions_from_assign);

                    Ok((instructions, returns))
                }
            }

            ExpressionWithoutType::Binary(op, left, right)
                if matches!(op, BinaryOperatorNode::Or | BinaryOperatorNode::And) =>
            {
                let new_dst = new_temp_variable(&Type::Integer, context);
                let instructions = ExpressionNode::convert_and_or(
                    op,
                    left.convert_and_evaluate(context)?,
                    right.convert_and_evaluate(context)?,
                    new_dst.clone(),
                    context,
                )?;

                Ok((instructions, new_dst.into()))
            }
            ExpressionWithoutType::Binary(op, left, right) => {
                let left_type = left.1.clone().unwrap();
                let (mut instructions, new_left) = left.convert_and_evaluate(context)?;
                let (mut instructions_from_right, new_right) =
                    right.convert_and_evaluate(context)?;
                instructions.append(&mut instructions_from_right);

                let bird_op = op.convert(context)?;
                let new_dst_type = if bird_op.is_relational() {
                    Type::Integer
                } else {
                    left_type
                };
                let new_dst = new_temp_variable(&new_dst_type, context);
                instructions.push(BirdsInstructionNode::Binary(
                    bird_op,
                    new_left,
                    new_right,
                    new_dst.clone(),
                ));

                Ok((instructions, new_dst.into()))
            }
            ExpressionWithoutType::Ternary(condition, then, otherwise) => {
                context.last_end_label_number += 1;
                let new_end_label_name = format!("end_{}", context.last_end_label_number);
                context.last_else_label_number += 1;
                let new_else_label_name = format!("else_{}", context.last_else_label_number);

                let (mut instructions, new_cond) = condition.convert_and_evaluate(context)?;
                instructions.push(BirdsInstructionNode::JumpZero(
                    new_cond,
                    new_else_label_name.clone(),
                ));

                let new_dst_type = then.1.clone().unwrap();
                let (mut instructions_from_then, new_then) = then.convert_and_evaluate(context)?;
                let new_dst = new_temp_variable(&new_dst_type, context);

                instructions.append(&mut instructions_from_then);
                instructions.append(&mut vec![
                    BirdsInstructionNode::Copy(new_then.clone(), new_dst.clone()),
                    BirdsInstructionNode::Jump(new_end_label_name.clone()),
                    BirdsInstructionNode::Label(new_else_label_name),
                ]);
                let (mut instructions_from_other, new_other) =
                    otherwise.convert_and_evaluate(context)?;
                instructions.append(&mut instructions_from_other);
                instructions.push(BirdsInstructionNode::Copy(new_other, new_dst.clone()));
                instructions.push(BirdsInstructionNode::Label(new_end_label_name));

                Ok((instructions, new_dst.into()))
            }
            ExpressionWithoutType::FunctionCall(name, args) => {
                let (mut instructions, values) = args.convert_and_evaluate(context)?;

                let new_dst_type = if let Type::Function(ref ret, _) =
                    context.symbols.get(&name).unwrap().symbol_type
                {
                    ret.as_ref().clone()
                } else {
                    return Err(
                        "Function stored in symbol table does not have function type".into(),
                    );
                };
                let new_dst = new_temp_variable(&new_dst_type, context);
                instructions.push(BirdsInstructionNode::FunctionCall(
                    name,
                    values,
                    new_dst.clone(),
                ));

                Ok((instructions, new_dst.into()))
            }
            ExpressionWithoutType::Cast(target_type, e) => {
                let this_type = e.1.clone().unwrap();

                let (mut instructions, new_src) = e.convert_and_evaluate(context)?;
                if target_type == this_type {
                    return Ok((instructions, new_src.into()));
                }
                let new_dst = new_temp_variable(&target_type, context);
                let mut instructions_from_cast = ExpressionWithoutType::cast(
                    new_src,
                    new_dst.clone(),
                    &this_type,
                    &target_type,
                )?;
                instructions.append(&mut instructions_from_cast);

                Ok((instructions, new_dst.into()))
            }
            ExpressionWithoutType::Dereference(src) => {
                let (instructions, new_src) = src.convert_and_evaluate(context)?;
                Ok((instructions, Destination::Dereference(new_src)))
            }
            ExpressionWithoutType::AddressOf(src) => {
                let (mut instructions, new_src) = src.convert(context)?;
                match new_src {
                    Destination::Direct(val) => {
                        let new_dst = new_temp_variable(self.1.as_ref().unwrap(), context);
                        instructions.push(BirdsInstructionNode::GetAddress(val, new_dst.clone()));
                        Ok((instructions, new_dst.into()))
                    }
                    Destination::Dereference(val) => Ok((instructions, val.into())),
                }
            }
            ExpressionWithoutType::Subscript(_src, _inner) => todo!(),
        }
    }
}

impl ExpressionWithoutType {
    fn cast(
        src: BirdsValueNode,
        dst: BirdsValueNode,
        this_type: &Type,
        target_type: &Type,
    ) -> Result<Vec<BirdsInstructionNode>, Box<dyn Error>> {
        let mut instructions = Vec::new();
        if target_type == &Type::Double {
            match this_type {
                Type::Integer | Type::Long => {
                    instructions.push(BirdsInstructionNode::IntToDouble(src, dst.clone()));
                }
                Type::UnsignedInteger | Type::UnsignedLong => {
                    instructions.push(BirdsInstructionNode::UintToDouble(src, dst.clone()));
                }
                Type::Array(..) => unreachable!(),
                Type::Pointer(_) => unreachable!(),
                Type::Double => unreachable!(),
                Type::Function(_, _) => unreachable!(),
            }
        } else if this_type == &Type::Double {
            match target_type {
                Type::Integer | Type::Long => {
                    instructions.push(BirdsInstructionNode::DoubleToInt(src, dst.clone()));
                }
                Type::UnsignedInteger | Type::UnsignedLong => {
                    instructions.push(BirdsInstructionNode::DoubleToUint(src, dst.clone()));
                }
                Type::Pointer(_) => unreachable!(),
                Type::Array(..) => unreachable!(),
                Type::Double => unreachable!(),
                Type::Function(_, _) => unreachable!(),
            }
        } else if target_type.get_size() == this_type.get_size() {
            // mov the old type into the new type directly
            // C casting behaviour basically ends up saying "never alter the
            // underlying binary unless to extend or truncate it", indirectly
            instructions.push(BirdsInstructionNode::Copy(src, dst.clone()));
        } else if target_type.get_size() < this_type.get_size() {
            instructions.push(BirdsInstructionNode::Truncate(src, dst.clone()));
        } else if this_type.is_signed() {
            instructions.push(BirdsInstructionNode::SignedExtend(src, dst.clone()));
        } else {
            instructions.push(BirdsInstructionNode::ZeroExtend(src, dst.clone()));
        }
        Ok(instructions)
    }

    fn assign(
        src: BirdsValueNode,
        dst: Destination,
    ) -> Result<(Vec<BirdsInstructionNode>, Destination), Box<dyn Error>> {
        let mut instructions = Vec::new();
        match dst {
            Destination::Direct(ref val) => {
                instructions.push(BirdsInstructionNode::Copy(src.clone(), val.clone()));
                Ok((instructions, dst.clone()))
            }
            Destination::Dereference(ref val) => {
                instructions.push(BirdsInstructionNode::StoreInPointer(
                    src.clone(),
                    val.clone(),
                ));
                // since we can't easily read the contained value of dst which is normally
                // returned by the expression, we return the value of src here instead,
                // since it (by definition) is what *dst equals to at this instant.
                Ok((instructions, src.into()))
            }
        }
    }
}

impl ExpressionNode {
    fn convert_and_or(
        op: BinaryOperatorNode,
        left: (Vec<BirdsInstructionNode>, BirdsValueNode),
        right: (Vec<BirdsInstructionNode>, BirdsValueNode),
        dst: BirdsValueNode,
        context: &mut ConvertContext,
    ) -> Result<Vec<BirdsInstructionNode>, Box<dyn Error>> {
        let (mut instructions, new_left) = left;
        let (mut instructions_from_right, new_right) = right;
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
                    BirdsInstructionNode::JumpNotZero(new_right, true_label_name.clone()),
                    BirdsInstructionNode::Copy(
                        BirdsValueNode::Constant(Constant::Integer(0)),
                        dst.clone(),
                    ),
                    BirdsInstructionNode::Jump(end_label_name.clone()),
                    BirdsInstructionNode::Label(true_label_name),
                    BirdsInstructionNode::Copy(
                        BirdsValueNode::Constant(Constant::Integer(1)),
                        dst.clone(),
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
                        dst.clone(),
                    ),
                    BirdsInstructionNode::Jump(end_label_name.clone()),
                    BirdsInstructionNode::Label(false_label_name),
                    BirdsInstructionNode::Copy(
                        BirdsValueNode::Constant(Constant::Integer(0)),
                        dst.clone(),
                    ),
                    BirdsInstructionNode::Label(end_label_name),
                ]);
            }
            _ => panic!("Unexpected binary operator found: {:?}", op),
        }
        Ok(instructions)
    }
}

impl ConvertEvaluate for ExpressionNode {
    type Output = (Vec<BirdsInstructionNode>, BirdsValueNode);

    fn convert_and_evaluate(
        self,
        context: &mut ConvertContext,
    ) -> Result<Self::Output, Box<dyn Error>> {
        let target_type = self.1.clone().unwrap();
        let (mut instructions, new_src) = self.convert(context)?;
        let (mut deref_instructions, new_src) = new_src.evaluate(&target_type, context);
        instructions.append(&mut deref_instructions);

        Ok((instructions, new_src))
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

impl Convert for BinaryOperatorNode {
    type Output = BirdsBinaryOperatorNode;

    fn convert(self, _context: &mut ConvertContext) -> Result<Self::Output, Box<dyn Error>> {
        let bird_op = match self {
            BinaryOperatorNode::Add => BirdsBinaryOperatorNode::Add,
            BinaryOperatorNode::Subtract => BirdsBinaryOperatorNode::Subtract,
            BinaryOperatorNode::Multiply => BirdsBinaryOperatorNode::Multiply,
            BinaryOperatorNode::Divide => BirdsBinaryOperatorNode::Divide,
            BinaryOperatorNode::Mod => BirdsBinaryOperatorNode::Mod,
            BinaryOperatorNode::BitwiseAnd => BirdsBinaryOperatorNode::BitwiseAnd,
            BinaryOperatorNode::BitwiseXor => BirdsBinaryOperatorNode::BitwiseXor,
            BinaryOperatorNode::BitwiseOr => BirdsBinaryOperatorNode::BitwiseOr,
            BinaryOperatorNode::ShiftLeft => BirdsBinaryOperatorNode::ShiftLeft,
            BinaryOperatorNode::ShiftRight => BirdsBinaryOperatorNode::ShiftRight,
            BinaryOperatorNode::Equal => BirdsBinaryOperatorNode::Equal,
            BinaryOperatorNode::NotEqual => BirdsBinaryOperatorNode::NotEqual,
            BinaryOperatorNode::Less => BirdsBinaryOperatorNode::Less,
            BinaryOperatorNode::Greater => BirdsBinaryOperatorNode::Greater,
            BinaryOperatorNode::LessEqual => BirdsBinaryOperatorNode::LessEqual,
            BinaryOperatorNode::GreaterEqual => BirdsBinaryOperatorNode::GreaterEqual,
            // process these in the 'else' block below instead
            BinaryOperatorNode::And | BinaryOperatorNode::Or => unreachable!(),
        };
        Ok(bird_op)
    }
}
