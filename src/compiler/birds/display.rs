use std::fmt::{Debug, Formatter};

use super::BirdsTopLevel;

struct WithoutAlternate<T>(T);

impl<T: Debug> Debug for WithoutAlternate<T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

struct List<'a, T>(&'a Vec<T>);

impl<'a, T: Debug> Debug for List<'a, T> {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        // write!(f, "{:?}", self.0)
        f.debug_list()
            .entries(self.0.iter().map(WithoutAlternate))
            .finish()
    }
}

impl Debug for BirdsTopLevel {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            BirdsTopLevel::Function(name, params, instructions, global) => f
                .debug_struct("BirdsTopLevel::Function")
                .field("name", name)
                .field("params", params)
                .field("instructions", &List(instructions))
                .field("global", global)
                .finish(),
            BirdsTopLevel::StaticVariable(name, init, global) => f
                .debug_struct("BirdTopLevel::StaticVariable")
                .field("name", name)
                .field("init", init)
                .field("global", global)
                .finish(),
        }
    }
}
