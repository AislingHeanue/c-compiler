use std::collections::HashMap;

use itertools::Itertools;

use super::{
    align_stack_size, AssemblyType, BinaryOperator, ConditionCode, ImmediateValue, Instruction,
    Operand, Register, Validate, ValidateContext, ValidationPass,
};

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
                        Operand::Imm(ImmediateValue::Unsigned(
                            align_stack_size((*stack_size).into(), 16).into(),
                        )),
                        Operand::Reg(Register::SP),
                    ),
                ]
                .into_iter()
                .chain(self.drain(..))
                .collect();
            }
            ValidationPass::CheckNaNComparisons => {
                *self = Instruction::update_instructions(
                    self.drain(..).collect_vec(),
                    context,
                    Instruction::check_unordered_comparisons,
                );
            }
            ValidationPass::FixShiftOperatorRegister => {
                *self = Instruction::update_instructions(
                    self.drain(..).collect_vec(),
                    context,
                    Instruction::fix_shift_operation_register,
                );
            }
            ValidationPass::RewriteMovZeroExtend => {
                *self = Instruction::update_instructions(
                    self.drain(..).collect_vec(),
                    context,
                    Instruction::rewrite_mov_zero_extend,
                );
            }
            ValidationPass::FixBadDst => {
                *self = Instruction::update_instructions(
                    self.drain(..).collect_vec(),
                    context,
                    Instruction::fix_bad_dst,
                );
            }
            ValidationPass::FixBadSrc => {
                *self = Instruction::update_instructions(
                    self.drain(..).collect_vec(),
                    context,
                    Instruction::fix_bad_src,
                );
            }
            ValidationPass::FixTwoMemoryAccesses => {
                *self = Instruction::update_instructions(
                    self.drain(..).collect_vec(),
                    context,
                    Instruction::fix_instructions_with_two_memory_accesses,
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
            Instruction::Movsx(_, _, ref mut src, ref mut dst) => {
                src.replace_mock_register(context);
                dst.replace_mock_register(context);
            }
            Instruction::MovZeroExtend(_, _, ref mut src, ref mut dst) => {
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
            Instruction::Lea(ref mut src, ref mut dst) => {
                src.replace_mock_register(context);
                dst.replace_mock_register(context);
            }
        };
        vec![self]
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

    fn rewrite_mov_zero_extend(self, _context: &mut ValidateContext) -> Vec<Instruction> {
        match self {
            Instruction::MovZeroExtend(ref src_type, _, ref src, ref dst) => {
                if src_type == &AssemblyType::Longword {
                    if dst.is_in_memory() {
                        vec![
                            Instruction::Mov(
                                AssemblyType::Longword,
                                src.clone(),
                                Operand::Reg(Register::R11),
                            ),
                            Instruction::Mov(
                                AssemblyType::Quadword,
                                Operand::Reg(Register::R11),
                                dst.clone(),
                            ),
                        ]
                    } else {
                        vec![Instruction::Mov(
                            AssemblyType::Longword,
                            src.clone(),
                            dst.clone(),
                        )]
                    }
                } else {
                    vec![self]
                }
            }
            _ => vec![self],
        }
    }

    fn fix_bad_src(self, _context: &mut ValidateContext) -> Vec<Instruction> {
        match self {
            // FIX IMMEDIATE VALUES IN BAD PLACES
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
            Instruction::Movsx(src_type, dst_type, Operand::Imm(value), dst) => vec![
                Instruction::Mov(src_type, Operand::Imm(value), Operand::Reg(Register::R10)),
                Instruction::Movsx(src_type, dst_type, Operand::Reg(Register::R10), dst),
            ],
            Instruction::MovZeroExtend(src_type, dst_type, Operand::Imm(value), dst) => vec![
                Instruction::Mov(src_type, Operand::Imm(value), Operand::Reg(Register::R10)),
                Instruction::MovZeroExtend(src_type, dst_type, Operand::Reg(Register::R10), dst),
            ],
            Instruction::Cvtsi2sd(src_t, Operand::Imm(value), dst) => vec![
                Instruction::Mov(src_t, Operand::Imm(value), Operand::Reg(Register::R10)),
                Instruction::Cvtsi2sd(src_t, Operand::Reg(Register::R10), dst),
            ],
            // FIX INSTRUCTIONS THAT CAN'T TAKE LARGE INTS
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
                if dst.is_in_memory() && !value.can_fit_in_longword() =>
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
            Instruction::Mov(AssemblyType::Longword, Operand::Imm(mut value), dst)
                if !value.can_fit_in_longword() =>
            {
                // truncation instruction can generate this kind of move. This check isn't strictly
                // needed but still may prevent some very hard-to-find errors otherwise
                value.truncate();
                vec![Instruction::Mov(
                    AssemblyType::Longword,
                    Operand::Imm(value),
                    dst,
                )]
            }
            Instruction::Mov(AssemblyType::Byte, Operand::Imm(mut value), dst)
                if !value.can_fit_in_byte() =>
            {
                value.truncate_to_byte();
                vec![Instruction::Mov(
                    AssemblyType::Byte,
                    Operand::Imm(value),
                    dst,
                )]
            }
            // FIX PUSHING AN XMM REGISTER
            Instruction::Push(op) if op.is_double_register() => {
                vec![
                    Instruction::Binary(
                        BinaryOperator::Sub,
                        AssemblyType::Quadword,
                        Operand::Imm(ImmediateValue::Signed(8)),
                        Operand::Reg(Register::SP),
                    ),
                    Instruction::Mov(AssemblyType::Double, op, Operand::Memory(Register::SP, 0)),
                ]
            }
            _ => vec![self],
        }
    }

    fn fix_bad_dst(self, _context: &mut ValidateContext) -> Vec<Instruction> {
        match self {
            // FIX ILLEGAL MEMORY ADDRESSES AS DST
            Instruction::Binary(BinaryOperator::Mult, t, src, dst) if dst.is_in_memory() => {
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
            Instruction::Movsx(src_type, dst_type, src, dst) if dst.is_in_memory() => vec![
                Instruction::Movsx(src_type, dst_type, src, Operand::Reg(Register::R11)),
                Instruction::Mov(dst_type, Operand::Reg(Register::R11), dst),
            ],
            Instruction::MovZeroExtend(src_type, dst_type, src, dst) if dst.is_in_memory() => {
                vec![
                    Instruction::MovZeroExtend(
                        src_type,
                        dst_type,
                        src,
                        Operand::Reg(Register::R11),
                    ),
                    Instruction::Mov(dst_type, Operand::Reg(Register::R11), dst),
                ]
            }
            Instruction::Cvttsd2si(dst_t, src, dst) if dst.is_in_memory() => {
                vec![
                    Instruction::Cvttsd2si(dst_t, src, Operand::Reg(Register::R11)),
                    Instruction::Mov(dst_t, Operand::Reg(Register::R11), dst),
                ]
            }
            Instruction::Cvtsi2sd(src_t, src, dst) if dst.is_in_memory() => {
                vec![
                    Instruction::Cvtsi2sd(src_t, src, Operand::Reg(Register::XMM15)),
                    Instruction::Mov(AssemblyType::Double, Operand::Reg(Register::XMM15), dst),
                ]
            }
            Instruction::Cmp(AssemblyType::Double, left, right) if right.is_in_memory() => {
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
            // FIX CONSTANT VALUE AS DST
            Instruction::Cmp(t, left, Operand::Imm(value)) => {
                vec![
                    Instruction::Mov(t, Operand::Imm(value), Operand::Reg(Register::R11)),
                    Instruction::Cmp(t, left, Operand::Reg(Register::R11)),
                ]
            }
            // LEA MUST WRITE TO A REGISTER
            Instruction::Lea(src, dst) if !matches!(dst, Operand::Reg(_)) => {
                vec![
                    Instruction::Lea(src, Operand::Reg(Register::R11)),
                    Instruction::Mov(AssemblyType::Quadword, Operand::Reg(Register::R11), dst),
                ]
            }
            _ => vec![self],
        }
    }

    fn fix_instructions_with_two_memory_accesses(
        self,
        _context: &mut ValidateContext,
    ) -> Vec<Instruction> {
        let (src, dst) = match self {
            Instruction::Mov(_, ref src, ref dst) => (src, dst),
            Instruction::Binary(_, _, ref src, ref dst) => (src, dst),
            Instruction::Cmp(_, ref left, ref right) => (left, right),
            _ => return vec![self],
        };
        if src.is_in_memory() && dst.is_in_memory() {
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
