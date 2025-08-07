use std::error::Error;

use itertools::{process_results, Itertools};

use crate::compiler::{
    parser::{
        BinaryOperatorNode, Block, BlockItemNode, DeclarationNode, ExpressionNode,
        ExpressionWithoutType, ForInitialiserNode, FunctionDeclaration, InitialiserNode,
        InitialiserWithoutType, ProgramNode, StatementNode, SwitchMapKey, UnaryOperatorNode,
        VariableDeclaration,
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
fn get_typed_constant(value: i64, target: &Type) -> BirdsValueNode {
    match target {
        Type::Integer => BirdsValueNode::Constant(Constant::Integer(value.try_into().unwrap())),
        Type::Long => BirdsValueNode::Constant(Constant::Long(value)),
        Type::UnsignedInteger => {
            BirdsValueNode::Constant(Constant::UnsignedInteger(value.try_into().unwrap()))
        }
        Type::UnsignedLong => {
            BirdsValueNode::Constant(Constant::UnsignedLong(value.try_into().unwrap()))
        }
        // adding a constant to a pointer is only reasonable if the second operand is Long
        Type::Pointer(_) => BirdsValueNode::Constant(Constant::Long(value)),
        Type::Array(..) => unreachable!(),
        Type::Function(_, _) => unreachable!(),
        Type::Char => BirdsValueNode::Constant(Constant::Char(value.try_into().unwrap())),
        Type::SignedChar => BirdsValueNode::Constant(Constant::Char(value.try_into().unwrap())),
        Type::UnsignedChar => {
            BirdsValueNode::Constant(Constant::UnsignedChar(value.try_into().unwrap()))
        }
        Type::Double => panic!("Can't use get_typed_constant to generate a double"),
    }
}

fn get_double(value: f64) -> BirdsValueNode {
    BirdsValueNode::Constant(Constant::Double(value))
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
                    StorageInfo::Static(init_value, global) => {
                        let initial = match init_value {
                            InitialValue::Tentative => vec![Constant::zero(&info.symbol_type)],
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
                    StorageInfo::Constant(init) => Some(BirdsTopLevel::StaticConstant(
                        info.symbol_type.clone(),
                        name.clone(),
                        init.clone(),
                    )),
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

                let (mut instructions, new_src) = expression.convert_and_evaluate(context)?;
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
            Ok(Vec::new())
        } else if let Some(init) = self.init {
            context.current_initialiser_offset = 0;
            init.convert(self.name, context)
        } else {
            Ok(Vec::new())
        }
    }
}

impl InitialiserNode {
    fn convert(
        self,
        dst: String,
        context: &mut ConvertContext,
    ) -> Result<Vec<BirdsInstructionNode>, Box<dyn Error>> {
        match (self.0, self.1.unwrap()) {
            (InitialiserWithoutType::Single(e), Type::Array(t, size))
                if e.is_string_literal() && t.is_character() =>
            {
                let mut instructions = Vec::new();
                let first_offset = context.current_initialiser_offset;
                if let ExpressionWithoutType::String(v) = e.0 {
                    let mut iter = v.iter().peekable();
                    while iter.peek().is_some() {
                        let count = iter.clone().count();
                        // if we can take 8 values at the same time, do so and convert them to a
                        // long
                        if count >= 8 {
                            let (one, two, three, four, five, six, seven, eight) =
                                iter.next_tuple().unwrap();
                            let bytes = [
                                *one as u8,
                                *two as u8,
                                *three as u8,
                                *four as u8,
                                *five as u8,
                                *six as u8,
                                *seven as u8,
                                *eight as u8,
                            ];
                            let converted = i64::from_le_bytes(bytes);
                            instructions.push(BirdsInstructionNode::CopyToOffset(
                                get_typed_constant(converted, &Type::Long),
                                dst.clone(),
                                context.current_initialiser_offset,
                            ));

                            context.current_initialiser_offset += 8;
                        } else if count >= 4 {
                            let (one, two, three, four) = iter.next_tuple().unwrap();
                            let bytes = [*one as u8, *two as u8, *three as u8, *four as u8];
                            let converted = i32::from_le_bytes(bytes);
                            instructions.push(BirdsInstructionNode::CopyToOffset(
                                get_typed_constant(converted.into(), &Type::Integer),
                                dst.clone(),
                                context.current_initialiser_offset,
                            ));

                            context.current_initialiser_offset += 4;
                        } else {
                            for next in iter.by_ref() {
                                instructions.push(BirdsInstructionNode::CopyToOffset(
                                    get_typed_constant((*next).into(), &Type::Char),
                                    dst.clone(),
                                    context.current_initialiser_offset,
                                ));
                                context.current_initialiser_offset += 1;
                            }
                        }
                    }
                    // for every space left in the array, add null bytes
                    let difference_in_size =
                        size as i32 - (context.current_initialiser_offset - first_offset);
                    for _ in 0..difference_in_size {
                        println!("{:?} {}", instructions, difference_in_size);
                        instructions.push(BirdsInstructionNode::CopyToOffset(
                            get_typed_constant(0, &Type::Char),
                            dst.clone(),
                            context.current_initialiser_offset,
                        ));
                        context.current_initialiser_offset += 1;
                    }
                } else {
                    unreachable!()
                }

                Ok(instructions)
            }
            (InitialiserWithoutType::Single(e), _) => {
                let offset = e.1.as_ref().unwrap().get_size();
                let (mut instructions, new_src) = e.convert_and_evaluate(context)?;
                if context.current_initialiser_offset == 0 {
                    instructions.push(BirdsInstructionNode::Copy(
                        new_src,
                        BirdsValueNode::Var(dst),
                    ));
                } else {
                    instructions.push(BirdsInstructionNode::CopyToOffset(
                        new_src,
                        dst,
                        context.current_initialiser_offset,
                    ));
                }
                context.current_initialiser_offset += offset;

                Ok(instructions)
            }
            (InitialiserWithoutType::Compound(initialisers), _) => {
                let instructions: Vec<BirdsInstructionNode> = process_results(
                    initialisers
                        .into_iter()
                        .map(|init| init.convert(dst.clone(), context)),
                    |iter| iter.flatten().collect(),
                )?;
                Ok(instructions)
            }
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
                let new_dst_type = src.1.clone().unwrap();

                // How to convert and evaluate an expression which may contain a dereference
                let (mut instructions, new_src) = src.convert(context)?;
                let (mut instructions_from_evaluate, evaluated_src) =
                    new_src.clone().evaluate(&new_dst_type, context);
                instructions.append(&mut instructions_from_evaluate);

                let new_dst = new_temp_variable(&new_dst_type, context);
                if let Type::Pointer(ref t) = new_dst_type {
                    let constant = if op == UnaryOperatorNode::PrefixIncrement {
                        1
                    } else {
                        -1
                    };
                    instructions.push(BirdsInstructionNode::AddPointer(
                        evaluated_src.clone(),
                        get_typed_constant(constant, &Type::Pointer(t.clone())),
                        t.get_size(),
                        new_dst.clone(),
                    ));
                } else {
                    let bird_op = if op == UnaryOperatorNode::PrefixIncrement {
                        BirdsBinaryOperatorNode::Add
                    } else {
                        BirdsBinaryOperatorNode::Subtract
                    };
                    if new_dst_type == Type::Double {
                        instructions.push(BirdsInstructionNode::Binary(
                            bird_op,
                            evaluated_src.clone(),
                            get_double(1.),
                            new_dst.clone(),
                        ));
                    } else {
                        instructions.push(BirdsInstructionNode::Binary(
                            bird_op,
                            evaluated_src.clone(),
                            get_typed_constant(1, &new_dst_type),
                            new_dst.clone(),
                        ));
                    }
                }

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

                if let Type::Pointer(ref t) = new_dst_type {
                    let constant = if op == UnaryOperatorNode::SuffixIncrement {
                        1
                    } else {
                        -1
                    };
                    instructions.push(BirdsInstructionNode::AddPointer(
                        new_dst.clone(),
                        get_typed_constant(constant, &Type::Pointer(t.clone())),
                        t.get_size(),
                        evaluated_src.clone(),
                    ));
                } else {
                    let bird_op = if op == UnaryOperatorNode::SuffixIncrement {
                        BirdsBinaryOperatorNode::Add
                    } else {
                        BirdsBinaryOperatorNode::Subtract
                    };
                    if new_dst_type == Type::Double {
                        instructions.push(BirdsInstructionNode::Binary(
                            bird_op,
                            new_dst.clone(),
                            get_double(1.),
                            evaluated_src.clone(),
                        ));
                    } else {
                        instructions.push(BirdsInstructionNode::Binary(
                            bird_op,
                            new_dst.clone(),
                            get_typed_constant(1, &new_dst_type),
                            evaluated_src.clone(),
                        ));
                    }
                }
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
                let left_type = left.1.clone().unwrap();
                let right_type = right.1.clone().unwrap();
                let common_type = self.1.clone().unwrap();

                let (mut instructions, new_left) = left.convert(context)?;

                let (mut instructions_from_dereference, evaluated_left) =
                    new_left.clone().evaluate(&left_type, context);
                instructions.append(&mut instructions_from_dereference);

                let (mut instructions_from_right, new_right) =
                    right.convert_and_evaluate(context)?;
                instructions.append(&mut instructions_from_right);

                let bird_op = op.convert(context)?;

                match (&bird_op, &left_type, &right_type) {
                    (BirdsBinaryOperatorNode::Add, Type::Pointer(ref left_t), right_t)
                        if right_t.is_integer() =>
                    {
                        let new_dst_type = Type::Long;
                        let new_dst = new_temp_variable(&new_dst_type, context);
                        instructions.push(BirdsInstructionNode::AddPointer(
                            evaluated_left,
                            new_right,
                            left_t.get_size(),
                            new_dst.clone(),
                        ));

                        let (mut instructions_from_assign, returns) =
                            ExpressionWithoutType::assign(new_dst, new_left)?;
                        instructions.append(&mut instructions_from_assign);
                        Ok((instructions, returns))
                    }
                    (BirdsBinaryOperatorNode::Subtract, Type::Pointer(ref left_t), right_t)
                        if right_t.is_integer() =>
                    {
                        let negate_right = new_temp_variable(right_t, context);
                        instructions.push(BirdsInstructionNode::Unary(
                            BirdsUnaryOperatorNode::Negate,
                            new_right,
                            negate_right.clone(),
                        ));

                        let new_dst_type = Type::Long;
                        let new_dst = new_temp_variable(&new_dst_type, context);
                        instructions.push(BirdsInstructionNode::AddPointer(
                            evaluated_left,
                            negate_right,
                            left_t.get_size(),
                            new_dst.clone(),
                        ));

                        let (mut instructions_from_assign, returns) =
                            ExpressionWithoutType::assign(new_dst, new_left)?;
                        instructions.append(&mut instructions_from_assign);
                        Ok((instructions, returns))
                    }
                    _ if common_type == left_type
                        || matches!(
                            bird_op,
                            BirdsBinaryOperatorNode::ShiftLeft
                                | BirdsBinaryOperatorNode::ShiftRight
                        ) =>
                    {
                        let new_dst = new_temp_variable(&left_type, context);
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
                    }
                    _ => {
                        let casted_left = new_temp_variable(&common_type, context);
                        let mut instructions_from_first_cast = ExpressionWithoutType::cast(
                            evaluated_left.clone(),
                            casted_left.clone(),
                            &left_type,
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
                        let casted_result = new_temp_variable(&left_type, context);
                        let mut instructions_from_second_cast = ExpressionWithoutType::cast(
                            new_dst.clone(),
                            casted_result.clone(),
                            &common_type,
                            &left_type,
                        )?;
                        instructions.append(&mut instructions_from_second_cast);

                        let (mut instructions_from_assign, returns) =
                            ExpressionWithoutType::assign(casted_result, new_left)?;
                        instructions.append(&mut instructions_from_assign);
                        Ok((instructions, returns))
                    }
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
                let right_type = right.1.clone().unwrap();

                let (mut instructions, new_left) = left.convert_and_evaluate(context)?;
                let (mut instructions_from_right, new_right) =
                    right.convert_and_evaluate(context)?;
                instructions.append(&mut instructions_from_right);
                let bird_op = op.convert(context)?;

                match (&bird_op, &left_type, &right_type) {
                    (BirdsBinaryOperatorNode::Add, Type::Pointer(ref left_t), right_t)
                        if right_t.is_integer() =>
                    {
                        let new_dst_type = Type::Long;
                        let new_dst = new_temp_variable(&new_dst_type, context);
                        instructions.push(BirdsInstructionNode::AddPointer(
                            new_left,
                            new_right,
                            left_t.get_size(),
                            new_dst.clone(),
                        ));

                        Ok((instructions, new_dst.into()))
                    }
                    (BirdsBinaryOperatorNode::Add, left_t, Type::Pointer(ref right_t))
                        if left_t.is_integer() =>
                    {
                        let new_dst_type = Type::Long;
                        let new_dst = new_temp_variable(&new_dst_type, context);
                        instructions.push(BirdsInstructionNode::AddPointer(
                            new_right,
                            new_left,
                            right_t.get_size(),
                            new_dst.clone(),
                        ));

                        Ok((instructions, new_dst.into()))
                    }
                    (BirdsBinaryOperatorNode::Subtract, Type::Pointer(ref left_t), right_t)
                        if right_t.is_integer() =>
                    {
                        let negate_right = new_temp_variable(right_t, context);
                        instructions.push(BirdsInstructionNode::Unary(
                            BirdsUnaryOperatorNode::Negate,
                            new_right,
                            negate_right.clone(),
                        ));

                        let new_dst_type = Type::Long;
                        let new_dst = new_temp_variable(&new_dst_type, context);
                        instructions.push(BirdsInstructionNode::AddPointer(
                            new_left,
                            negate_right,
                            left_t.get_size(),
                            new_dst.clone(),
                        ));

                        Ok((instructions, new_dst.into()))
                    }
                    (
                        BirdsBinaryOperatorNode::Subtract,
                        Type::Pointer(ref left_t),
                        Type::Pointer(_right_t),
                    ) => {
                        let diff = new_temp_variable(&Type::Long, context);
                        instructions.push(BirdsInstructionNode::Binary(
                            BirdsBinaryOperatorNode::Subtract,
                            new_left,
                            new_right,
                            diff.clone(),
                        ));
                        // ptrdiff_t
                        let new_dst = new_temp_variable(&Type::Long, context);
                        instructions.push(BirdsInstructionNode::Binary(
                            BirdsBinaryOperatorNode::Divide,
                            diff,
                            get_typed_constant(left_t.get_size().into(), &Type::Long),
                            new_dst.clone(),
                        ));

                        Ok((instructions, new_dst.into()))
                    }

                    _ => {
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
                }
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
            ExpressionWithoutType::Subscript(src, inner) => {
                let left_type = src.1.clone().unwrap();
                let right_type = inner.1.clone().unwrap();

                let (mut instructions, new_left) = src.convert_and_evaluate(context)?;
                let (mut instructions_from_right, new_right) =
                    inner.convert_and_evaluate(context)?;

                let (inner_type, pointer_is_on_left) = if let Type::Pointer(t) = left_type {
                    (t, true)
                } else if let Type::Pointer(t) = right_type {
                    (t, false)
                } else {
                    unreachable!()
                };

                instructions.append(&mut instructions_from_right);
                let new_dst = new_temp_variable(&Type::Pointer(inner_type.clone()), context);
                if pointer_is_on_left {
                    instructions.push(BirdsInstructionNode::AddPointer(
                        new_left,
                        new_right,
                        inner_type.get_size(),
                        new_dst.clone(),
                    ));
                } else {
                    instructions.push(BirdsInstructionNode::AddPointer(
                        new_right,
                        new_left,
                        inner_type.get_size(),
                        new_dst.clone(),
                    ));
                }
                Ok((instructions, Destination::Dereference(new_dst)))
            }
            ExpressionWithoutType::String(s) => {
                // Strings in initialisers for char arrays do not use this code path
                context.num_block_strings += 1;

                let new_name = format!("string.block.{}", context.num_block_strings);
                context.symbols.insert(
                    new_name.clone(),
                    SymbolInfo {
                        symbol_type: self.1.unwrap(),
                        // null terminator has not been appended yet, this happens later
                        storage: StorageInfo::Static(
                            InitialValue::Initial(vec![StaticInitial::Ordinal(
                                OrdinalStatic::String(s, true),
                            )]),
                            false,
                        ),
                    },
                );

                let new_dst = BirdsValueNode::Var(new_name);
                Ok((Vec::new(), new_dst.into()))
            }
        }
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
                Type::Integer | Type::Long | Type::Char | Type::SignedChar => {
                    instructions.push(BirdsInstructionNode::IntToDouble(src, dst.clone()));
                }
                Type::UnsignedInteger | Type::UnsignedLong | Type::UnsignedChar => {
                    instructions.push(BirdsInstructionNode::UintToDouble(src, dst.clone()));
                }
                Type::Array(..) => unreachable!(),
                Type::Pointer(_) => unreachable!(),
                Type::Double => unreachable!(),
                Type::Function(_, _) => unreachable!(),
            }
        } else if this_type == &Type::Double {
            match target_type {
                Type::Integer | Type::Long | Type::Char | Type::SignedChar => {
                    instructions.push(BirdsInstructionNode::DoubleToInt(src, dst.clone()));
                }
                Type::UnsignedInteger | Type::UnsignedLong | Type::UnsignedChar => {
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
