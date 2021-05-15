// Copyright 2021 Sergey Mechtaev

// This file is part of Modus.

// Modus is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// Modus is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with Modus.  If not, see <https://www.gnu.org/licenses/>.

use std::collections::HashSet;

use crate::logic;
use logic::{ Term, Rule };

#[derive(Hash, Eq, PartialEq)]
pub enum ConcreteType {
    Integer,
    String,
    Version,
    Image,
    Atom
}

pub struct Type(HashSet<ConcreteType>);

lazy_static! {
    static ref TOP: Type = {
        let mut m = HashSet::new();
        m.insert(ConcreteType::Integer);
        m.insert(ConcreteType::String);
        m.insert(ConcreteType::Version);
        m.insert(ConcreteType::Image);
        m.insert(ConcreteType::Atom);
        Type(m)
    };
    static ref BOTTOM: Type = Type(HashSet::new());
}

impl Type {
    pub fn concrete(t: ConcreteType) -> Self {
        let mut m = HashSet::new();
        m.insert(t);
        Type(m)
    }

    pub fn unite(&self, other: Self) -> Self {
        todo!();
    }

    pub fn intersect(&self, other: Self) -> Self {
        todo!();
    }
}