use std::error::Error;

use crate::compiler::{
    codegen::align_stack_size,
    parser::{MemberEntry, StructDeclaration, StructInfo},
};

use super::{CheckTypes, ValidateContext};

impl CheckTypes for StructDeclaration {
    fn check_types(&mut self, context: &mut ValidateContext) -> Result<(), Box<dyn Error>> {
        let members = if let Some(ref mut members) = self.members {
            members
        } else {
            // // assume that if we have entered this block and the corresponding struct is not in the
            // // structs map, then this is a reference to an incomplete type (bad)
            // if !context.structs.contains_key(&self.name) {
            //     return Err(format!("Struct {} used before it's defined", self.name).into());
            // }
            return Ok(());
        };

        if context.structs.contains_key(&self.name) {
            return Err(format!("Struct {} is defined twice in the same scope", self.name).into());
        }

        let mut seen = Vec::new();
        for m in members.iter_mut() {
            if seen.contains(&m.name) {
                return Err("Repeated name in struct definition".into());
            }
            seen.push(m.name.clone());

            if let Some(ref mut s) = m.struct_declaration {
                // if m is itself a struct definition, bring said definition into scope
                s.check_types(context)?;
            }

            m.member_type.check_types(context)?;

            // this also prevents recursive structs, since structs that have not been fully
            // resolved into the structs map are not complete
            if !m.member_type.is_complete(&mut context.structs) {
                return Err("Structs may only be composed of complete types".into());
            }
        }

        let mut entries = Vec::new();
        let mut size = 0;
        let mut alignment = 1;
        for m in members.iter_mut() {
            let this_alignment = m.member_type.get_alignment(&mut context.structs);
            let this_offset = align_stack_size(size, this_alignment);
            entries.push(MemberEntry {
                member_type: m.member_type.clone(),
                name: m.name.clone(),
                offset: this_offset,
            });
            alignment = u64::max(alignment, this_alignment);
            size = this_offset + m.member_type.get_size(&mut context.structs);
        }
        size = align_stack_size(size, alignment);
        context.structs.insert(
            self.name.clone(),
            StructInfo {
                alignment,
                size,
                members: entries,
            },
        );

        Ok(())
    }
}
