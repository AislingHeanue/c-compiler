use std::fmt::{Debug, Formatter};

use super::BirdsFunctionNode;

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

impl Debug for BirdsFunctionNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("BirdsFunctionNode")
            .field("name", &self.name)
            .field("instructions", &List(&self.instructions))
            .finish()
    }
}
