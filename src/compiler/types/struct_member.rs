use std::collections::HashMap;

use crate::compiler::parser::StructInfo;

use super::{Count, FindMemberName, Flatten, MemberEntry, Type};

impl Count for Vec<MemberEntry> {
    fn count(&self, structs: &mut HashMap<String, StructInfo>) -> usize {
        self.iter().map(|m| m.count(structs)).sum()
    }
}

impl Flatten for Vec<MemberEntry> {
    type Output = Vec<(MemberEntry, usize)>;

    fn flatten(&self, structs: &mut HashMap<String, StructInfo>) -> Self::Output {
        self.iter().flat_map(|m| m.flatten(structs)).collect()
    }
}

impl Flatten for MemberEntry {
    type Output = Vec<(MemberEntry, usize)>;

    fn flatten(&self, structs: &mut HashMap<String, StructInfo>) -> Self::Output {
        match &self.name {
            Some(_) => {
                vec![(self.clone(), self.offset as usize)]
            }
            None => {
                // this is an embedded entry, so check there too!
                if let Type::Struct(ref s_name) = self.member_type {
                    let info = structs.get(s_name).unwrap().clone();
                    info.members
                        .flatten(structs)
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

impl FindMemberName for Vec<MemberEntry> {
    fn find_name(
        &self,
        name: &str,
        structs: &mut HashMap<String, StructInfo>,
    ) -> Option<(MemberEntry, u64)> {
        self.iter().find_map(|m| m.find_name(name, structs))
    }
}

impl MemberEntry {
    fn find_name(
        &self,
        name: &str,
        structs: &mut HashMap<String, StructInfo>,
    ) -> Option<(MemberEntry, u64)> {
        match &self.name {
            Some(n) => {
                if n == name {
                    Some((self.clone(), self.offset))
                } else {
                    None
                }
            }
            None => {
                // this is an embedded entry, so check there too!
                if let Type::Struct(ref s_name) = self.member_type {
                    let info = structs.get(s_name).unwrap().clone();
                    info.members
                        .find_name(name, structs)
                        .map(|(member, offset)| (member, offset + self.offset))
                } else {
                    unreachable!()
                }
            }
        }
    }

    fn count(&self, structs: &mut HashMap<String, StructInfo>) -> usize {
        match &self.name {
            Some(_) => 1,
            None => {
                // this is an embedded entry, so count there too!
                if let Type::Struct(ref s_name) = self.member_type {
                    let info = structs.get(s_name).unwrap().clone();
                    info.members.count(structs)
                } else {
                    unreachable!()
                }
            }
        }
    }
}
