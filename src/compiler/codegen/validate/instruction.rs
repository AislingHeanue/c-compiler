use std::collections::{HashMap, VecDeque};

use itertools::Itertools;

use crate::compiler::codegen::{align_stack_size, AssemblySymbolInfo};

use super::{
    allocation::AllocateRegisters, AssemblyType, BinaryOperator, ConditionCode, ImmediateValue,
    Instruction, Operand, Register, Validate, ValidateContext, ValidationPass,
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
            ValidationPass::AllocateRegisters => {
                context.aliased_variables = address_taken_analysis(self)
                    .into_iter()
                    .unique()
                    .collect_vec();
                context.static_variables =
                    static_variables(context).into_iter().unique().collect_vec();
                self.allocate_registers(context)?;
                self.allocate_registers_for_double(context)?;
            }
            ValidationPass::ReplaceMockRegisters => {
                // SECOND PASS: replace every mock register with an entry on the stack, making sure to keep
                // a map from register names to stack locations
                context.current_stack_locations = HashMap::new();
                context.current_stack_size = 0;
                if let AssemblySymbolInfo::Function(_, true, _, _) = context
                    .symbols
                    .get(context.current_function_name.as_ref().unwrap())
                    .unwrap()
                {
                    // start the stack size at 8, so that we can read the location to store the
                    // return value from the first 8 bytes on the stack
                    context.current_stack_size = 8;
                }
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
                let callee_registers = context
                    .function_callee_saved_registers
                    .get(context.current_function_name.as_ref().unwrap())
                    .cloned()
                    .unwrap_or(Vec::new());
                let callee_registers_size = (8 * callee_registers.len()) as u32;
                let aligned_total_stack_size =
                    align_stack_size((stack_size + callee_registers_size).into(), 16);
                // THIRD PASS: use the value of current_max_pointer to add an instruction to the start of
                // the function
                let mut initial_vec = vec![
                    // push the address of RBP to the stack (at RSP).
                    Instruction::Push(Operand::Reg(Register::BP)),
                    // move RBP to RSP's current location.
                    Instruction::Mov(
                        AssemblyType::Quadword,
                        Operand::Reg(Register::SP),
                        Operand::Reg(Register::BP),
                    ),
                ];
                let offset = aligned_total_stack_size - callee_registers_size as u64;
                if offset != 0 {
                    initial_vec.push(
                        // allocate space for local variables in the space before RSP in the stack.
                        Instruction::Binary(
                            BinaryOperator::Sub,
                            AssemblyType::Quadword,
                            Operand::Imm(ImmediateValue::Unsigned(
                                aligned_total_stack_size - callee_registers_size as u64,
                            )),
                            Operand::Reg(Register::SP),
                        ),
                    )
                }
                for r in callee_registers.into_iter() {
                    initial_vec.push(Instruction::Push(Operand::Reg(r)));
                }
                *self = initial_vec.into_iter().chain(self.drain(..)).collect();
            }
            ValidationPass::RewriteRet => {
                *self = Instruction::update_instructions(
                    self.drain(..).collect_vec(),
                    context,
                    Instruction::rewrite_ret,
                );
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

    fn rewrite_ret(self, context: &mut ValidateContext) -> Vec<Instruction> {
        match self {
            Instruction::Ret => {
                let mut instructions = VecDeque::new();
                instructions.append(
                    &mut vec![
                        Instruction::Mov(
                            AssemblyType::Quadword,
                            Operand::Reg(Register::BP),
                            Operand::Reg(Register::SP),
                        ),
                        Instruction::Pop(Register::BP),
                        Instruction::Ret,
                    ]
                    .into(),
                );

                let callee_registers = context
                    .function_callee_saved_registers
                    .get(context.current_function_name.as_ref().unwrap())
                    .cloned()
                    .unwrap_or(Vec::new());

                for r in callee_registers.into_iter() {
                    instructions.push_front(Instruction::Pop(r));
                }
                instructions.into()
            }
            _ => vec![self],
        }
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
            Instruction::JmpCondition(_, _, _) => {}
            Instruction::SetCondition(_, ref mut dst, _) => {
                dst.replace_mock_register(context);
            }
            Instruction::Label(_) => {}
            Instruction::Ret => {}
            Instruction::Push(ref mut src) => {
                src.replace_mock_register(context);
            }
            Instruction::Pop(_) => {} // pop only points to real registers
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
                ) && t != AssemblyType::Double
                    && !matches!(left, Operand::Imm(_)) =>
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

    // ge sf = of
    // e ZF
    // l sf != of
    // le ZF or sf = of
    // b CF
    // be CF or ZF
    fn check_unordered_comparisons(self, context: &mut ValidateContext) -> Vec<Instruction> {
        match self {
            Instruction::SetCondition(c, dst, true)
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
                if c == ConditionCode::Ne {
                    vec![
                        Instruction::SetCondition(ConditionCode::P, dst.clone(), false),
                        Instruction::JmpCondition(ConditionCode::P, label_name_nan.clone(), false),
                        Instruction::SetCondition(c, dst, false),
                        Instruction::Label(label_name_nan),
                    ]
                } else {
                    vec![
                        // check PF is set
                        Instruction::JmpCondition(ConditionCode::P, label_name_nan.clone(), false),
                        Instruction::SetCondition(c, dst, false),
                        Instruction::Label(label_name_nan),
                    ]

                }
            }
            Instruction::JmpCondition(c, label, true)
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
                if c == ConditionCode::Ne {
                    vec![
                        Instruction::JmpCondition(ConditionCode::P, label.clone(), false),
                        Instruction::JmpCondition(c, label, false),
                    ]
                } else {
                    vec![
                        // check PF is set
                        Instruction::JmpCondition(ConditionCode::P, label_name_nan.clone(), false),
                        Instruction::JmpCondition(c, label, false),
                        Instruction::Label(label_name_nan),
                    ]

                }
            }
            _ => vec![self],
        }
    }

    pub fn get_used_and_updated_operands(
        &self,
        context: &mut ValidateContext,
    ) -> (Vec<Operand>, Vec<Operand>) {
        match self {
            Instruction::Mov(_, src, dst) => {
                convert_uses_and_updates((vec![src.clone()], vec![dst.clone()]))
            }
            Instruction::Movsx(_, _, src, dst) => {
                convert_uses_and_updates((vec![src.clone()], vec![dst.clone()]))
            }
            Instruction::MovZeroExtend(_, _, src, dst) => {
                convert_uses_and_updates((vec![src.clone()], vec![dst.clone()]))
            }
            Instruction::Lea(_only_data_here, dst) => {
                convert_uses_and_updates((vec![], vec![dst.clone()]))
            }
            Instruction::Cvttsd2si(_, src, dst) => {
                convert_uses_and_updates((vec![src.clone()], vec![dst.clone()]))
            }
            Instruction::Cvtsi2sd(_, src, dst) => {
                convert_uses_and_updates((vec![src.clone()], vec![dst.clone()]))
            }
            Instruction::Unary(_, _, src) => {
                convert_uses_and_updates((vec![src.clone()], vec![src.clone()]))
            }
            Instruction::Binary(op, t, left, right) => {
                if matches!(
                    op,
                    BinaryOperator::ShiftLeft
                        | BinaryOperator::ShiftRight
                        | BinaryOperator::UnsignedShiftLeft
                        | BinaryOperator::UnsignedShiftRight
                ) && *t != AssemblyType::Double
                    && !matches!(left, Operand::Imm(_))
                {
                    // in these specific circumstances, shift operations copy their value to CX and
                    // then reads it.
                    convert_uses_and_updates((
                        vec![left.clone(), right.clone()],
                        vec![right.clone(), Operand::Reg(Register::CX)],
                    ))
                } else {
                    convert_uses_and_updates((
                        vec![left.clone(), right.clone()],
                        vec![right.clone()],
                    ))
                }
            }
            Instruction::Cmp(_, left, right) => {
                convert_uses_and_updates((vec![left.clone(), right.clone()], vec![]))
            }
            Instruction::Idiv(_, src) => convert_uses_and_updates((
                vec![
                    src.clone(),
                    Operand::Reg(Register::AX),
                    Operand::Reg(Register::DX),
                ],
                vec![Operand::Reg(Register::AX), Operand::Reg(Register::DX)],
            )),
            Instruction::Div(_, src) => convert_uses_and_updates((
                vec![
                    src.clone(),
                    Operand::Reg(Register::AX),
                    Operand::Reg(Register::DX),
                ],
                vec![Operand::Reg(Register::AX), Operand::Reg(Register::DX)],
            )),
            Instruction::Cdq(_) => convert_uses_and_updates((
                vec![Operand::Reg(Register::AX)],
                vec![Operand::Reg(Register::DX)],
            )),
            Instruction::SetCondition(_, dst, _) => {
                convert_uses_and_updates((vec![], vec![dst.clone()]))
            }
            Instruction::Push(src) => convert_uses_and_updates((vec![src.clone()], vec![])),
            Instruction::Call(name) => {
                let symbol = context.symbols.get(name).unwrap();
                let used = if let AssemblySymbolInfo::Function(_, _, registers_used_for_params, _) =
                    symbol
                {
                    // println!("{:?}", registers_used_for_params);
                    registers_used_for_params
                        .iter()
                        .map(|r| Operand::Reg(r.clone()))
                        .collect()
                } else {
                    unreachable!()
                };
                let updated = vec![
                    Operand::Reg(Register::DI),
                    Operand::Reg(Register::SI),
                    Operand::Reg(Register::DX),
                    Operand::Reg(Register::CX),
                    Operand::Reg(Register::R8),
                    Operand::Reg(Register::R9),
                    Operand::Reg(Register::AX),
                    Operand::Reg(Register::XMM0),
                    Operand::Reg(Register::XMM1),
                    Operand::Reg(Register::XMM2),
                    Operand::Reg(Register::XMM3),
                    Operand::Reg(Register::XMM4),
                    Operand::Reg(Register::XMM5),
                    Operand::Reg(Register::XMM6),
                    Operand::Reg(Register::XMM7),
                    Operand::Reg(Register::XMM7),
                    Operand::Reg(Register::XMM8),
                    Operand::Reg(Register::XMM9),
                    Operand::Reg(Register::XMM10),
                    Operand::Reg(Register::XMM11),
                    Operand::Reg(Register::XMM12),
                    Operand::Reg(Register::XMM13),
                    Operand::Reg(Register::XMM14),
                    Operand::Reg(Register::XMM15),
                ];
                convert_uses_and_updates((used, updated))
            }
            Instruction::Jmp(_)
            | Instruction::JmpCondition(_, _, _)
            | Instruction::Label(_)
            | Instruction::Pop(_)
            | Instruction::Ret => (vec![], vec![]),
        }
    }
}

fn convert_uses_and_updates(
    (uses, updates): (Vec<Operand>, Vec<Operand>),
) -> (Vec<Operand>, Vec<Operand>) {
    let mut uses_out = Vec::new();
    let mut updates_out = Vec::new();
    for this_use in uses.into_iter() {
        match this_use {
            Operand::Imm(_) => {}
            Operand::Reg(_) => uses_out.push(this_use),
            Operand::MockReg(_) => uses_out.push(this_use),
            Operand::Memory(r, _) => uses_out.push(Operand::Reg(r)),
            Operand::MockMemory(name, _) => uses_out.push(Operand::MockReg(name)),
            Operand::Data(_, _) => {}
            Operand::Indexed(base, index, _) => {
                uses_out.push(Operand::Reg(base));
                uses_out.push(Operand::Reg(index))
            }
        }
    }
    for update in updates.into_iter() {
        match update {
            Operand::Imm(_) => {}
            Operand::Reg(_) => updates_out.push(update),
            Operand::MockReg(_) => updates_out.push(update),
            // these Operands do not actually cause an update, but they do read
            Operand::Memory(r, _) => uses_out.push(Operand::Reg(r)),
            Operand::MockMemory(name, _) => uses_out.push(Operand::MockReg(name)),
            Operand::Data(_, _) => {}
            Operand::Indexed(base, index, _) => {
                uses_out.push(Operand::Reg(base));
                uses_out.push(Operand::Reg(index))
            }
        }
    }
    (uses_out, updates_out)
}

fn address_taken_analysis(v: &[Instruction]) -> Vec<Operand> {
    v.iter()
        .filter_map(|instruction| match instruction {
            Instruction::Lea(src, _) => match src {
                Operand::MockReg(name) => Some(vec![Operand::MockReg(name.clone())]),
                Operand::MockMemory(name, _) => Some(vec![Operand::MockReg(name.clone())]),
                _ => None,
            },
            Instruction::Mov(
                _,
                Operand::MockMemory(name, _),
                Operand::MockMemory(other_name, _),
            ) => Some(vec![
                Operand::MockReg(name.to_string()).clone(),
                Operand::MockReg(other_name.to_string()).clone(),
            ]),
            Instruction::Mov(_, Operand::MockMemory(name, _), _) => {
                Some(vec![Operand::MockReg(name.to_string()).clone()])
            }
            Instruction::Mov(_, _, Operand::MockMemory(name, _)) => {
                Some(vec![Operand::MockReg(name.to_string()).clone()])
            }
            _ => None,
        })
        .flatten()
        .collect()
}

fn static_variables(context: &mut ValidateContext) -> Vec<Operand> {
    context
        .symbols
        .iter()
        .filter_map(|s| {
            if matches!(
                s.1,
                AssemblySymbolInfo::Object(_, true, _)
                    | AssemblySymbolInfo::Object(AssemblyType::ByteArray(_, _), _, _)
            ) {
                Some(Operand::MockReg(s.0.clone()))
            } else {
                None
            }
        })
        .collect()
}
