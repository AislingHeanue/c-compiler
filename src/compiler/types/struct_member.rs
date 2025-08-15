use std::collections::HashMap;

use crate::compiler::parser::StructInfo;

use super::{Count, FindMemberName, Flatten, FlattenAndExpandUnions, MemberEntry, Type};

impl Count for Vec<MemberEntry> {
    // counting for initialisers, so unions only contribute one value
    fn count(&self, self_is_union: bool, structs: &mut HashMap<String, StructInfo>) -> usize {
        self.flatten(self_is_union, structs).len()
    }
}

impl FindMemberName for Vec<MemberEntry> {
    fn find_name(
        &self,
        name: &str,
        self_is_union: bool,
        structs: &mut HashMap<String, StructInfo>,
    ) -> Option<(MemberEntry, usize)> {
        self.flatten_and_expand_unions(self_is_union, structs)
            .iter()
            .find_map(|(m, offset)| {
                if let Some(m_name) = &m.name {
                    if m_name == name {
                        Some((m.clone(), *offset))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
    }
}

impl Flatten for Vec<MemberEntry> {
    type Output = Vec<(MemberEntry, usize)>;

    fn flatten(
        &self,
        self_is_union: bool,
        structs: &mut HashMap<String, StructInfo>,
    ) -> Self::Output {
        if self_is_union {
            // only get the first entry in a given union. This is how union initialisers work.
            self.first().unwrap().flatten(self_is_union, structs)
        } else {
            self.iter()
                .flat_map(|m| m.flatten(self_is_union, structs))
                .collect()
        }
    }
}
impl FlattenAndExpandUnions for Vec<MemberEntry> {
    type Output = Vec<(MemberEntry, usize)>;

    fn flatten_and_expand_unions(
        &self,
        self_is_union: bool,
        structs: &mut HashMap<String, StructInfo>,
    ) -> Self::Output {
        self.iter()
            .flat_map(|m| m.flatten_and_expand_unions(self_is_union, structs))
            .collect()
    }
}

impl Flatten for MemberEntry {
    type Output = Vec<(MemberEntry, usize)>;

    fn flatten(
        &self,
        self_is_union: bool,
        structs: &mut HashMap<String, StructInfo>,
    ) -> Self::Output {
        match (&self.name, self_is_union) {
            (Some(_), _) => {
                vec![(self.clone(), self.offset as usize)]
            }
            (None, true) => {
                // this is an anonymous entry within a union. Treat it as a normal sub-struct
                vec![(self.clone(), self.offset as usize)]
            }
            (None, false) => {
                // this is an embedded entry, so check there too!
                if let Type::Struct(ref s_name, member_is_union) = self.member_type {
                    let info = structs.get(s_name).unwrap().clone();
                    info.members
                        .flatten(member_is_union, structs)
                        .into_iter()
                        .map(|(member, offset)| (member, offset + self.offset as usize))
                        .collect()
                } else {
                    unreachable!()
                }
            }
        }
    }
}
impl FlattenAndExpandUnions for MemberEntry {
    type Output = Vec<(MemberEntry, usize)>;

    fn flatten_and_expand_unions(
        &self,
        self_is_union: bool,
        structs: &mut HashMap<String, StructInfo>,
    ) -> Self::Output {
        match (&self.name, self_is_union) {
            (Some(_), _) => {
                vec![(self.clone(), self.offset as usize)]
            }
            (None, _) => {
                // this is an embedded entry, so check there too! This function is used for
                // dot and arrow operations, and therefore we need every union member as well.
                if let Type::Struct(ref s_name, member_is_union) = self.member_type {
                    let info = structs.get(s_name).unwrap().clone();
                    info.members
                        .flatten_and_expand_unions(member_is_union, structs)
                        .into_iter()
                        .map(|(member, offset)| (member, offset + self.offset as usize))
                        .collect()
                } else {
                    unreachable!()
                }
            }
        }
    }
}
