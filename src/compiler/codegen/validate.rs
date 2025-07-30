use std::collections::HashMap;

use itertools::Itertools;

use super::{
    AssemblySymbolInfo, AssemblyType, BinaryOperator, Instruction, Operand, Program, Register,
    TopLevel, Validate, ValidateContext, ValidationPass,
};

fn align_stack_size(initial: i32, alignment: i32) -> i32 {
    initial + (alignment - initial).rem_euclid(alignment)
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
                        Operand::Imm(align_stack_size(*stack_size, 16).into()),
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
                    Instruction::fix_instructions_with_two_stack_entries,
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

    fn fix_instructions_with_two_stack_entries(
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
        if matches!(src, &Operand::Stack(_) | &Operand::Data(_))
            && matches!(dst, &Operand::Stack(_) | &Operand::Data(_))
        {
            match self {
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
            Instruction::Movsx(Operand::Imm(value), dst) => vec![
                Instruction::Mov(
                    AssemblyType::Longword,
                    Operand::Imm(value),
                    Operand::Reg(Register::R10),
                ),
                Instruction::Movsx(Operand::Reg(Register::R10), dst),
            ],
            _ => vec![self],
        }
    }

    fn fix_memory_address_as_dst(self, _context: &mut ValidateContext) -> Vec<Instruction> {
        match self {
            Instruction::Binary(BinaryOperator::Mult, t, src, Operand::Stack(value)) => vec![
                Instruction::Mov(t, Operand::Stack(value), Operand::Reg(Register::R11)),
                Instruction::Binary(
                    BinaryOperator::Mult,
                    t,
                    src.clone(),
                    Operand::Reg(Register::R11), // we use r11d to fix dst's
                ),
                Instruction::Mov(t, Operand::Reg(Register::R11), Operand::Stack(value)),
            ],
            Instruction::Movsx(src, Operand::Stack(value)) => vec![
                Instruction::Movsx(src, Operand::Reg(Register::R11)),
                Instruction::Mov(
                    AssemblyType::Quadword,
                    Operand::Reg(Register::R11),
                    Operand::Stack(value),
                ),
            ],
            _ => vec![self],
        }
    }

    fn fix_shift_operation_register(self, _context: &mut ValidateContext) -> Vec<Instruction> {
        match self {
            Instruction::Binary(BinaryOperator::ShiftLeft, t, left, right) => {
                vec![
                    Instruction::Mov(t, left, Operand::Reg(Register::CX)),
                    Instruction::Binary(
                        BinaryOperator::ShiftLeft,
                        t,
                        Operand::Reg(Register::CX),
                        right,
                    ),
                ]
            }
            Instruction::Binary(BinaryOperator::ShiftRight, t, left, right) => {
                vec![
                    Instruction::Mov(t, left, Operand::Reg(Register::CX)),
                    Instruction::Binary(
                        BinaryOperator::ShiftRight,
                        t,
                        Operand::Reg(Register::CX),
                        right,
                    ),
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
                if value > i32::MAX.into()
                    && matches!(
                        op,
                        BinaryOperator::Add
                            | BinaryOperator::Sub
                            | BinaryOperator::Mult
                            | BinaryOperator::BitwiseAnd
                            | BinaryOperator::BitwiseXor
                            | BinaryOperator::BitwiseOr
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
                if value > i32::MAX.into() =>
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
            Instruction::Push(Operand::Imm(value)) if value > i32::MAX.into() => {
                vec![
                    Instruction::Mov(
                        AssemblyType::Quadword,
                        Operand::Imm(value),
                        Operand::Reg(Register::R10),
                    ),
                    Instruction::Push(Operand::Reg(Register::R10)),
                ]
            }
            Instruction::Mov(AssemblyType::Quadword, Operand::Imm(value), Operand::Stack(loc))
                if value > i32::MAX.into() =>
            {
                vec![
                    Instruction::Mov(
                        AssemblyType::Quadword,
                        Operand::Imm(value),
                        Operand::Reg(Register::R10),
                    ),
                    Instruction::Mov(
                        AssemblyType::Quadword,
                        Operand::Reg(Register::R10),
                        Operand::Stack(loc),
                    ),
                ]
            }
            Instruction::Mov(AssemblyType::Quadword, Operand::Imm(value), Operand::Data(loc))
                if value > i32::MAX.into() =>
            {
                vec![
                    Instruction::Mov(
                        AssemblyType::Quadword,
                        Operand::Imm(value),
                        Operand::Reg(Register::R10),
                    ),
                    Instruction::Mov(
                        AssemblyType::Quadword,
                        Operand::Reg(Register::R10),
                        Operand::Data(loc),
                    ),
                ]
            }
            // truncation instruction can generate this kind of move. This check isn't strictly
            // needed but still may prevent some very hard-to-find errors otherwise
            Instruction::Mov(AssemblyType::Longword, Operand::Imm(value), dst)
                if value > i32::MAX.into() =>
            {
                vec![Instruction::Mov(
                    AssemblyType::Quadword,
                    Operand::Imm((value as i32).into()),
                    dst,
                )]
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
                Some(AssemblySymbolInfo::Object(_, true)) => {
                    // static objects go to data
                    *self = Operand::Data(name.clone());
                }
                Some(AssemblySymbolInfo::Object(t, false)) => {
                    let alignment = t.get_alignment();
                    // align to 'alignment', eg if alignment = 8 make sure that stack_size is a
                    // multiple of 8
                    context.current_stack_size =
                        align_stack_size(context.current_stack_size + alignment, alignment);
                    let new_location = -context.current_stack_size;
                    context
                        .current_stack_locations
                        .insert(name.clone(), new_location);
                    *self = Operand::Stack(new_location);
                }
                None | Some(AssemblySymbolInfo::Function(_)) => {}
            }
        }
    }
}
