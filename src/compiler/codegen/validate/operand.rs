use crate::compiler::codegen::align_stack_size;

use super::{AssemblySymbolInfo, Operand, Register, ValidateContext};

impl Operand {
    pub fn is_in_memory(&self) -> bool {
        matches!(
            self,
            &Operand::Memory(_, _) | &Operand::Data(_, _) | &Operand::Indexed(..)
        )
    }

    pub fn is_double_register(&self) -> bool {
        if let Operand::Reg(r) = self {
            matches!(
                r,
                Register::XMM0
                    | Register::XMM1
                    | Register::XMM2
                    | Register::XMM3
                    | Register::XMM4
                    | Register::XMM5
                    | Register::XMM6
                    | Register::XMM7
                    | Register::XMM14
                    | Register::XMM15
            )
        } else {
            false
        }
    }

    pub fn replace_mock_register(&mut self, context: &mut ValidateContext) {
        let (name, index) = match self {
            Operand::MockReg(name) => (name.clone(), &mut 0),
            Operand::MockMemory(name, index) => (name.clone(), index),
            _ => return,
        };
        let existing_location = context.current_stack_locations.get(&name);
        if let Some(loc) = existing_location {
            *self = Operand::Memory(Register::BP, *loc + *index);
            return;
        }
        match context.symbols.get(&name) {
            Some(AssemblySymbolInfo::Object(_, true, _)) => {
                // static objects go to data
                *self = Operand::Data(name.clone(), *index);
            }
            None => {
                // extern objects also go in data i suppose
                *self = Operand::Data(name.clone(), *index);
            }
            Some(AssemblySymbolInfo::Object(t, false, false)) => {
                let size = t.get_size();
                // align to 'alignment', eg if alignment = 8 make sure that stack_size is a
                // multiple of 8
                let alignment = t.get_alignment();
                context.current_stack_size =
                    align_stack_size(context.current_stack_size as u64 + size, alignment.into())
                        as u32;
                let new_location = -(context.current_stack_size as i32);
                context
                    .current_stack_locations
                    .insert(name.clone(), new_location);
                *self = Operand::Memory(Register::BP, new_location + *index);
            }
            // None => {
            //     panic!("Could not find matching declaration for {:?}", name)
            // }
            Some(AssemblySymbolInfo::Object(_, false, true)) => unreachable!(),
            Some(AssemblySymbolInfo::Function(_, _, _, _)) => unreachable!(),
        }
    }
}
