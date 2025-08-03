use std::collections::HashMap;

use itertools::Itertools;

use super::{
    AssemblySymbolInfo, AssemblyType, BinaryOperator, ConditionCode, ImmediateValue, Instruction,
    Operand, Program, Register, TopLevel, Validate, ValidateContext, ValidationPass,
};

fn align_stack_size(initial: i32, alignment: i32) -> i32 {
    initial + (alignment - initial).rem_euclid(alignment)
}

fn match_in_memory(operand: &Operand) -> bool {
    matches!(operand, &Operand::Stack(_) | &Operand::Data(_))
}

impl Validate for Program {
    fn validate(
        &mut self,
        context: &mut ValidateContext,
    ) -> Result<(), Box<dyn std::error::Error>> {
        for d in self.body.iter_mut() {
            d.validate(context)?;
        }
        Ok(())
    }
}

impl Validate for TopLevel {
    fn validate(
        &mut self,
        context: &mut ValidateContext,
    ) -> Result<(), Box<dyn std::error::Error>> {
        match self {
            TopLevel::Function(name, instructions, _global) => {
                context.current_function_name = Some(name.to_string());
                instructions.validate(context)?;
                context.current_function_name = None
            }
            TopLevel::StaticVariable(_name, _global, _alignment, _init) => {}
            TopLevel::StaticConstant(_name, _alignment, _init) => {}
        }
        Ok(())
    }
}

impl Validate for Vec<Instruction> {
    fn validate(
        &mut self,
        context: &mut ValidateContext,
    ) -> Result<(), Box<dyn std::error::Error>> {
        // empty functions
        if self.is_empty() {
            return Ok(());
        }
        match context.pass.clone().unwrap() {
            ValidationPass::ReplaceMockRegisters => {
                // SECOND PASS: replace every mock register with an entry on the stack, making sure to keep
                // a map from register names to stack locations
                context.current_stack_locations = HashMap::new();
                context.current_stack_size = 0;
                *self = Instruction::update_instructions(
                    self.drain(..).collect_vec(),
                    context,
                    Instruction::replace_mock_registers,
                );
                context.stack_sizes.insert(
                    context.current_function_name.clone().unwrap(),
                    context.current_stack_size,
                );
            }
            ValidationPass::AllocateFunctionStack => {
                let stack_size = context
                    .stack_sizes
                    .get(context.current_function_name.as_ref().unwrap())
                    .unwrap();
                // THIRD PASS: use the value of current_max_pointer to add an instruction to the start of
                // the function
                *self = vec![
                    // push the address of RBP to the stack (at RSP).
                    Instruction::Push(Operand::Reg(Register::BP)),
                    // move RBP to RSP's current location.
                    Instruction::Mov(
                        AssemblyType::Quadword,
                        Operand::Reg(Register::SP),
                        Operand::Reg(Register::BP),
                    ),
                    // allocate space for local variables in the space before RSP in the stack.
                    Instruction::Binary(
                        BinaryOperator::Sub,
                        AssemblyType::Quadword,
                        Operand::Imm(ImmediateValue::Signed(
                            align_stack_size(*stack_size, 16).into(),
                        )),
                        Operand::Reg(Register::SP),
                    ),
                ]
                .into_iter()
                .chain(self.drain(..))
                .collect();
            }
            ValidationPass::FixShiftOperatorRegister => {
                *self = Instruction::update_instructions(
                    self.drain(..).collect_vec(),
                    context,
                    Instruction::fix_shift_operation_register,
                );
            }
            ValidationPass::FixTwoMemoryAccesses => {
                *self = Instruction::update_instructions(
                    self.drain(..).collect_vec(),
                    context,
                    Instruction::fix_instructions_with_two_memory_accesses,
                );
            }
            ValidationPass::FixBadImmValues => {
                *self = Instruction::update_instructions(
                    self.drain(..).collect_vec(),
                    context,
                    Instruction::fix_immediate_values_in_bad_places,
                );
            }
            ValidationPass::FixMemoryAsDst => {
                *self = Instruction::update_instructions(
                    self.drain(..).collect_vec(),
                    context,
                    Instruction::fix_memory_address_as_dst,
                );
            }
            ValidationPass::FixConstantAsDst => {
                *self = Instruction::update_instructions(
                    self.drain(..).collect_vec(),
                    context,
                    Instruction::fix_constant_as_dst,
                );
            }
            ValidationPass::FixLargeInts => {
                *self = Instruction::update_instructions(
                    self.drain(..).collect_vec(),
                    context,
                    Instruction::fix_large_ints,
                );
            }
            ValidationPass::RewriteMovZeroExtend => {
                *self = Instruction::update_instructions(
                    self.drain(..).collect_vec(),
                    context,
                    Instruction::rewrite_mov_zero_extend,
                );
            }
            ValidationPass::CheckNaNComparisons => {
                *self = Instruction::update_instructions(
                    self.drain(..).collect_vec(),
                    context,
                    Instruction::check_unordered_comparisons,
                );
            }
        };

        Ok(())
    }
}

impl Instruction {
    fn update_instructions(
        instructions: Vec<Instruction>,
        context: &mut ValidateContext,
        f: fn(Instruction, &mut ValidateContext) -> Vec<Instruction>,
    ) -> Vec<Instruction> {
        instructions
            .into_iter()
            .flat_map(|i| f(i, context))
            .collect()
    }

    fn replace_mock_registers(mut self, context: &mut ValidateContext) -> Vec<Instruction> {
        match self {
            Instruction::Mov(_, ref mut src, ref mut dst) => {
                src.replace_mock_register(context);
                dst.replace_mock_register(context);
            }
            Instruction::Movsx(ref mut src, ref mut dst) => {
                src.replace_mock_register(context);
                dst.replace_mock_register(context);
            }
            Instruction::MovZeroExtend(ref mut src, ref mut dst) => {
                src.replace_mock_register(context);
                dst.replace_mock_register(context);
            }
            Instruction::Cvttsd2si(_, ref mut src, ref mut dst) => {
                src.replace_mock_register(context);
                dst.replace_mock_register(context);
            }
            Instruction::Cvtsi2sd(_, ref mut src, ref mut dst) => {
                src.replace_mock_register(context);
                dst.replace_mock_register(context);
            }
            Instruction::Unary(_, _, ref mut dst) => {
                dst.replace_mock_register(context);
            }
            Instruction::Binary(_, _, ref mut src, ref mut dst) => {
                src.replace_mock_register(context);
                dst.replace_mock_register(context);
            }
            Instruction::Idiv(_, ref mut src) => {
                src.replace_mock_register(context);
            }
            Instruction::Div(_, ref mut src) => {
                src.replace_mock_register(context);
            }
            Instruction::Cmp(_, ref mut src, ref mut dst) => {
                src.replace_mock_register(context);
                dst.replace_mock_register(context);
            }
            Instruction::Cdq(_) => {}
            Instruction::Jmp(_) => {}
            Instruction::JmpCondition(_, _) => {}
            Instruction::SetCondition(_, ref mut dst) => {
                dst.replace_mock_register(context);
            }
            Instruction::Label(_) => {}
            Instruction::Ret => {}
            Instruction::Push(ref mut src) => {
                src.replace_mock_register(context);
            }
            Instruction::Call(_) => {}
        };
        vec![self]
    }

    fn fix_instructions_with_two_memory_accesses(
        self,
        _context: &mut ValidateContext,
    ) -> Vec<Instruction> {
        let (src, dst) = match self {
            Instruction::Mov(_, ref src, ref dst) => (src, dst),
            Instruction::Binary(_, _, ref src, ref dst) => (src, dst),
            Instruction::Cmp(_, ref left, ref right) => (left, right),
            // Instruction::Movsx(ref src, ref dst) => (src, dst),
            _ => return vec![self],
        };
        if match_in_memory(src) && match_in_memory(dst) {
            match self {
                Instruction::Mov(AssemblyType::Double, src, dst) => vec![
                    Instruction::Mov(AssemblyType::Double, src, Operand::Reg(Register::XMM14)),
                    Instruction::Mov(AssemblyType::Double, Operand::Reg(Register::XMM14), dst),
                ],
                Instruction::Mov(t, src, dst) => vec![
                    Instruction::Mov(t, src, Operand::Reg(Register::R10)),
                    Instruction::Mov(t, Operand::Reg(Register::R10), dst),
                ],
                Instruction::Binary(op, t, src, dst) => vec![
                    Instruction::Mov(t, src, Operand::Reg(Register::R10)),
                    Instruction::Binary(op, t, Operand::Reg(Register::R10), dst),
                ],
                Instruction::Cmp(t, left, right) => vec![
                    Instruction::Mov(t, left, Operand::Reg(Register::R10)),
                    Instruction::Cmp(t, Operand::Reg(Register::R10), right),
                ],
                _ => unreachable!(),
            }
        } else {
            vec![self]
        }
    }

    fn fix_immediate_values_in_bad_places(
        self,
        _context: &mut ValidateContext,
    ) -> Vec<Instruction> {
        match self {
            Instruction::Idiv(t, Operand::Imm(value)) => vec![
                Instruction::Mov(
                    t,
                    Operand::Imm(value),
                    // we use r10d to fix src's
                    Operand::Reg(Register::R10),
                ),
                Instruction::Idiv(t, Operand::Reg(Register::R10)),
            ],
            Instruction::Div(t, Operand::Imm(value)) => vec![
                Instruction::Mov(
                    t,
                    Operand::Imm(value),
                    // we use r10d to fix src's
                    Operand::Reg(Register::R10),
                ),
                Instruction::Div(t, Operand::Reg(Register::R10)),
            ],
            Instruction::Movsx(Operand::Imm(value), dst) => vec![
                Instruction::Mov(
                    AssemblyType::Longword,
                    Operand::Imm(value),
                    Operand::Reg(Register::R10),
                ),
                Instruction::Movsx(Operand::Reg(Register::R10), dst),
            ],
            Instruction::Cvtsi2sd(src_t, Operand::Imm(value), dst) => vec![
                Instruction::Mov(src_t, Operand::Imm(value), Operand::Reg(Register::R10)),
                Instruction::Cvtsi2sd(src_t, Operand::Reg(Register::R10), dst),
            ],
            _ => vec![self],
        }
    }

    fn fix_memory_address_as_dst(self, _context: &mut ValidateContext) -> Vec<Instruction> {
        match self {
            Instruction::Binary(BinaryOperator::Mult, t, src, dst) if match_in_memory(&dst) => {
                let scratch_register = if t == AssemblyType::Double {
                    Operand::Reg(Register::XMM15)
                } else {
                    Operand::Reg(Register::R11)
                };
                vec![
                    Instruction::Mov(t, dst.clone(), scratch_register.clone()),
                    Instruction::Binary(BinaryOperator::Mult, t, src, scratch_register.clone()),
                    Instruction::Mov(t, scratch_register, dst),
                ]
            }
            Instruction::Movsx(src, dst) if match_in_memory(&dst) => vec![
                Instruction::Movsx(src, Operand::Reg(Register::R11)),
                Instruction::Mov(AssemblyType::Quadword, Operand::Reg(Register::R11), dst),
            ],
            Instruction::Cvttsd2si(dst_t, src, dst) if match_in_memory(&dst) => {
                vec![
                    Instruction::Cvttsd2si(dst_t, src, Operand::Reg(Register::R11)),
                    Instruction::Mov(dst_t, Operand::Reg(Register::R11), dst),
                ]
            }
            Instruction::Cvtsi2sd(src_t, src, dst) if match_in_memory(&dst) => {
                vec![
                    Instruction::Cvtsi2sd(src_t, src, Operand::Reg(Register::XMM15)),
                    Instruction::Mov(AssemblyType::Double, Operand::Reg(Register::XMM15), dst),
                ]
            }
            Instruction::Cmp(AssemblyType::Double, left, right) if match_in_memory(&right) => {
                vec![
                    // do Mov and then Cmp here and not the other way around. This is because we
                    // typically need to read the flags set by Cmp immediately after the
                    // instruction is run, so we can't append extra instructions after it.
                    Instruction::Mov(AssemblyType::Double, right, Operand::Reg(Register::XMM15)),
                    Instruction::Cmp(AssemblyType::Double, left, Operand::Reg(Register::XMM15)),
                ]
            }
            Instruction::Binary(op, AssemblyType::Double, src, dst)
                if matches!(
                    op,
                    BinaryOperator::Add
                        | BinaryOperator::Sub
                        | BinaryOperator::Mult
                        | BinaryOperator::DivDouble
                        | BinaryOperator::Xor
                ) =>
            {
                vec![
                    Instruction::Mov(
                        AssemblyType::Double,
                        dst.clone(),
                        Operand::Reg(Register::XMM15),
                    ),
                    Instruction::Binary(
                        op,
                        AssemblyType::Double,
                        src,
                        Operand::Reg(Register::XMM15),
                    ),
                    Instruction::Mov(AssemblyType::Double, Operand::Reg(Register::XMM15), dst),
                ]
            }
            _ => vec![self],
        }
    }

    fn fix_shift_operation_register(self, _context: &mut ValidateContext) -> Vec<Instruction> {
        match self {
            Instruction::Binary(op, t, left, right)
                if matches!(
                    op,
                    BinaryOperator::ShiftLeft
                        | BinaryOperator::ShiftRight
                        | BinaryOperator::UnsignedShiftLeft
                        | BinaryOperator::UnsignedShiftRight
                ) && t != AssemblyType::Double =>
            {
                vec![
                    Instruction::Mov(t, left, Operand::Reg(Register::CX)),
                    Instruction::Binary(op, t, Operand::Reg(Register::CX), right),
                ]
            }
            _ => vec![self],
        }
    }

    fn fix_constant_as_dst(self, _context: &mut ValidateContext) -> Vec<Instruction> {
        match self {
            Instruction::Cmp(t, left, Operand::Imm(value)) => {
                vec![
                    Instruction::Mov(t, Operand::Imm(value), Operand::Reg(Register::R11)),
                    Instruction::Cmp(t, left, Operand::Reg(Register::R11)),
                ]
            }
            _ => vec![self],
        }
    }

    fn fix_large_ints(self, _context: &mut ValidateContext) -> Vec<Instruction> {
        match self {
            Instruction::Binary(op, AssemblyType::Quadword, Operand::Imm(value), dst)
                if !value.can_fit_in_longword()
                    && matches!(
                        op,
                        BinaryOperator::Add
                            | BinaryOperator::Sub
                            | BinaryOperator::Mult
                            | BinaryOperator::And
                            | BinaryOperator::Xor
                            | BinaryOperator::Or
                    ) =>
            {
                vec![
                    Instruction::Mov(
                        AssemblyType::Quadword,
                        Operand::Imm(value),
                        Operand::Reg(Register::R10),
                    ),
                    Instruction::Binary(
                        op,
                        AssemblyType::Quadword,
                        Operand::Reg(Register::R10),
                        dst,
                    ),
                ]
            }
            Instruction::Cmp(AssemblyType::Quadword, Operand::Imm(value), dst)
                if !value.can_fit_in_longword() =>
            {
                vec![
                    Instruction::Mov(
                        AssemblyType::Quadword,
                        Operand::Imm(value),
                        Operand::Reg(Register::R10),
                    ),
                    Instruction::Cmp(AssemblyType::Quadword, Operand::Reg(Register::R10), dst),
                ]
            }
            Instruction::Push(Operand::Imm(value)) if !value.can_fit_in_longword() => {
                vec![
                    Instruction::Mov(
                        AssemblyType::Quadword,
                        Operand::Imm(value),
                        Operand::Reg(Register::R10),
                    ),
                    Instruction::Push(Operand::Reg(Register::R10)),
                ]
            }
            Instruction::Mov(AssemblyType::Quadword, Operand::Imm(value), dst)
                if match_in_memory(&dst) && !value.can_fit_in_longword() =>
            {
                vec![
                    Instruction::Mov(
                        AssemblyType::Quadword,
                        Operand::Imm(value),
                        Operand::Reg(Register::R10),
                    ),
                    Instruction::Mov(AssemblyType::Quadword, Operand::Reg(Register::R10), dst),
                ]
            }
            // truncation instruction can generate this kind of move. This check isn't strictly
            // needed but still may prevent some very hard-to-find errors otherwise
            Instruction::Mov(AssemblyType::Longword, Operand::Imm(mut value), dst)
                if !value.can_fit_in_longword() =>
            {
                value.truncate();
                vec![Instruction::Mov(
                    AssemblyType::Longword,
                    Operand::Imm(value),
                    dst,
                )]
            }

            _ => vec![self],
        }
    }

    fn rewrite_mov_zero_extend(self, _context: &mut ValidateContext) -> Vec<Instruction> {
        match self {
            Instruction::MovZeroExtend(src, dst) if match_in_memory(&dst) => {
                vec![
                    Instruction::Mov(AssemblyType::Longword, src, Operand::Reg(Register::R11)),
                    Instruction::Mov(AssemblyType::Quadword, Operand::Reg(Register::R11), dst),
                ]
            }
            Instruction::MovZeroExtend(src, dst) => {
                vec![Instruction::Mov(AssemblyType::Longword, src, dst)]
            }
            _ => vec![self],
        }
    }

    // ge sf = of       -> do cmp 0,1 !!
    // e ZF             -> do cmp 1,0
    // l sf != of       -> do cmp 1,0
    // le ZF or sf = of -> do cmp 1,0
    // b CF             -> do cmp 1,0
    // be CF or ZF      -> do cmp 1,0
    fn check_unordered_comparisons(self, context: &mut ValidateContext) -> Vec<Instruction> {
        match self {
            Instruction::SetCondition(c, dst)
                // these ones will normally give a positive result on an unordered operation, which
                // means they all need to be overwritten if PF,ZF and CF are set
                if matches!(
                    c,
                    ConditionCode::Ge
                        | ConditionCode::E
                        | ConditionCode::L
                        | ConditionCode::Le
                        | ConditionCode::B
                        | ConditionCode::Be
                        // Should always resolve to True when NaN is found
                        | ConditionCode::Ne
                ) =>
            {
                context.num_labels += 1;
                let label_name_nan = format!("nan_{}", context.num_labels);
                let label_name_not_nan = format!("not_nan_{}", context.num_labels);
                let (first_value,second_value) = if c == ConditionCode::Ge {
                    (1,0)
                } else {
                    (0,1)
                };
                vec![
                    // check ZF not set
                    Instruction::JmpCondition(ConditionCode::Ne, label_name_not_nan.clone()),
                    // check CF not set
                    Instruction::JmpCondition(ConditionCode::Ae, label_name_not_nan.clone()),
                    // check PF is set
                    Instruction::JmpCondition(ConditionCode::P, label_name_nan.clone()),
                    Instruction::Jmp(label_name_not_nan.clone()),
                    Instruction::Label(label_name_nan),
                    Instruction::Mov(AssemblyType::Longword, Operand::Imm(ImmediateValue::Signed(second_value)), Operand::Reg(Register::R11)),
                    Instruction::Cmp(AssemblyType::Longword, Operand::Imm(ImmediateValue::Signed(first_value)), Operand::Reg(Register::R11)),
                    Instruction::Label(label_name_not_nan),
                    Instruction::SetCondition(c, dst)
                ]
            },
            Instruction::JmpCondition(c, label)
                // these ones will normally give a positive result on an unordered operation, which
                // means they all need to be overwritten if PF,ZF and CF are set
                if matches!(
                    c,
                    ConditionCode::Ge
                        | ConditionCode::E
                        | ConditionCode::L
                        | ConditionCode::Le
                        | ConditionCode::B
                        | ConditionCode::Be
                        | ConditionCode::Ne
                ) =>
            {
                context.num_labels += 1;
                let label_name_nan = format!("nan_{}", context.num_labels);
                let label_name_not_nan = format!("not_nan_{}", context.num_labels);
                let (first_value,second_value) = if c == ConditionCode::Ge {
                    (0,1)
                } else {
                    (1,0)
                };
                vec![
                    // check ZF not set
                    Instruction::JmpCondition(ConditionCode::Ne, label_name_not_nan.clone()),
                    // check CF not set
                    Instruction::JmpCondition(ConditionCode::Ae, label_name_not_nan.clone()),
                    // check PF is set
                    Instruction::JmpCondition(ConditionCode::P, label_name_nan.clone()),
                    Instruction::Jmp(label_name_not_nan.clone()),
                    Instruction::Label(label_name_nan),
                    Instruction::Mov(AssemblyType::Longword, Operand::Imm(ImmediateValue::Signed(second_value)), Operand::Reg(Register::R11)),
                    Instruction::Cmp(AssemblyType::Longword, Operand::Imm(ImmediateValue::Signed(first_value)), Operand::Reg(Register::R11)),
                    Instruction::Label(label_name_not_nan),
                    Instruction::JmpCondition(c, label)
                ]
            }
            _ => vec![self],
        }
    }
}

impl Operand {
    fn replace_mock_register(&mut self, context: &mut ValidateContext) {
        if let Operand::MockReg(name) = self {
            let existing_location = context.current_stack_locations.get(name);
            if let Some(loc) = existing_location {
                *self = Operand::Stack(*loc);
                return;
            }
            match context.symbols.get(name) {
                Some(AssemblySymbolInfo::Object(_, true, _)) => {
                    // static objects go to data
                    *self = Operand::Data(name.clone());
                }
                Some(AssemblySymbolInfo::Object(t, false, false)) => {
                    let alignment = t.get_alignment();
                    // align to 'alignment', eg if alignment = 8 make sure that stack_size is a
                    // multiple of 8
                    context.current_stack_size = align_stack_size(
                        context.current_stack_size + alignment as i32,
                        alignment as i32,
                    );
                    let new_location = -context.current_stack_size;
                    context
                        .current_stack_locations
                        .insert(name.clone(), new_location);
                    *self = Operand::Stack(new_location);
                }
                None => panic!("Could not find matching declaration for {:?}", name),
                Some(AssemblySymbolInfo::Object(_, false, true)) => unreachable!(),
                Some(AssemblySymbolInfo::Function(_)) => unreachable!(),
            }
        }
    }
}
