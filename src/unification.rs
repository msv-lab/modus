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


use std::{collections::HashMap, hash::Hash};

use crate::logic;
use logic::{ Atom, Term, Rule, Literal, Groundness };

pub struct Substitution<C, V>(HashMap<V, Term<C, V>>);

impl<C, V> Groundness for Substitution<C, V> {
    fn is_grounded() -> bool {
        todo!()
    }
}

trait Substitutable<C, V> {
    type Output;

    fn substitute(&self, s: &Substitution<C, V>) -> Self::Output;
} 

impl<C: Clone, V: Eq + Hash + Clone> Substitutable<C, V> for Term<C, V> {
    type Output = Term<C, V>;
    fn substitute(&self, s: &Substitution<C, V>) -> Self::Output {
        match &self {
            Term::Variable(v) => s.0.get(v).unwrap_or(self).clone(),
            Term::Compound(atom, args) => todo!(),
            _ => self.clone()
        }
    }
}

impl<C: Clone, V: Eq + Hash + Clone> Substitutable<C, V> for Literal<C, V> {
    type Output = Literal<C, V>;
    fn substitute(&self, s: &Substitution<C, V>) -> Self::Output {
        Literal { atom: self.atom.clone(), args: self.args.iter().map(|t| t.substitute(s)).collect() }
    }
}

impl<C: Clone, V: Eq + Hash + Clone> Substitutable<C, V> for Vec<Literal<C, V>> {
    type Output = Vec<Literal<C, V>>;
    fn substitute(&self, s: &Substitution<C, V>) -> Self::Output {
        self.iter().map(|l| l.substitute(s)).collect()
    }
}

pub fn unify<C, V>(l: &Literal<C, V>, m: &Literal<C, V>) -> Option<Substitution<C, V>>
where
    C: PartialEq + Clone,
    V: PartialEq + Eq + Hash + Clone
{
    if l.singature() != m.singature() {
        return None;
    }
    let mut s = HashMap::<V, Term<C, V>>::new();
    for (i, tl) in l.args.iter().enumerate() {
        let tm = &m.args[i];
        if tl != tm {
            match tl {
                Term::Variable(v) => { s.insert(v.clone(), tm.clone()); },
                _ => match tm {
                    Term::Variable(v) => { s.insert(v.clone(), tl.clone()); },
                    _ => return None
                }
            }
        }
    }
    Some(Substitution(s))
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn simple_unifier() {
        let l = Literal::<u32, String>{ atom: Atom("a".into()),
                                        args: vec![Term::Variable("x".into()), Term::Constant(1)] };
        let m = Literal::<u32, String>{ atom: Atom("a".into()),
                                        args: vec![Term::Constant(2), Term::Variable("y".into())] };
        let result = unify(&l, &m);
        assert!(result.is_some());
        let mgu = result.unwrap();
        assert_eq!(l.substitute(&mgu), m.substitute(&mgu))
    }
}