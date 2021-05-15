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


use crate::logic;
use logic::{ Rule };
use crate::unification::{Substitution, Rename};


pub trait Variable<C, V>: Rename<C, V> {
    fn aux() -> Self; 
}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, sync::atomic::{AtomicU32, Ordering}};

    use super::*;

    static AVAILABLE_INDEX: AtomicU32 = AtomicU32::new(0);

    /// Assume that underscore is not used in normal variables

    impl Rename<logic::Atom, logic::toy::Variable> for logic::toy::Variable {
        type Output = logic::toy::Variable;
        fn rename(&self) -> (Self::Output, Substitution<logic::Atom, logic::toy::Variable>) {
            let index = AVAILABLE_INDEX.fetch_add(1, Ordering::SeqCst);
            let prefix = self.split('_').next().unwrap();
            let renamed = format!("{}_{}", prefix, index);
            let mut s = HashMap::<logic::toy::Variable, logic::Term<logic::Atom, logic::toy::Variable>>::new();
            s.insert(self.clone(), logic::Term::Variable(renamed.clone()));
            (renamed, s)
        }
    }

    impl Variable<logic::Atom, logic::toy::Variable> for logic::toy::Variable {
        fn aux() -> logic::toy::Variable {
            let index = AVAILABLE_INDEX.fetch_add(1, Ordering::SeqCst);
            format!("Aux{}", index)
        }
    }
   
}