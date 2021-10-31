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

use std::collections::HashMap;

use crate::logic;
use crate::logic::{ModusConstant, ModusVariable};
use logic::{Clause, Ground, Literal, Term, Predicate};

pub type Substitution<C, V> = HashMap<V, Term<C, V>>;

impl<C: ModusConstant, V: ModusVariable> Ground for Substitution<C, V> {
    fn is_ground(&self) -> bool {
        let mut result = true;
        for v in self.values() {
            if !v.variables().is_empty() {
                result = false
            }
        }
        result
    }
}

pub trait Substitute<C: ModusConstant, V: ModusVariable> {
    type Output;

    fn substitute(&self, s: &Substitution<C, V>) -> Self::Output;
}

pub trait Rename<V: ModusVariable> {
    fn rename(&self) -> V;
}

pub trait RenameWithSubstitution<C: ModusConstant, V: ModusVariable> {
    type Output;

    fn rename(&self) -> (Self::Output, Substitution<C, V>);
}

impl<C: ModusConstant, V: ModusVariable> Substitute<C, V> for Term<C, V> {
    type Output = Term<C, V>;
    fn substitute(&self, s: &Substitution<C, V>) -> Self::Output {
        match &self {
            Term::Variable(v) => s.get(v).unwrap_or(self).clone(),
            _ => self.clone(),
        }
    }
}

impl<C: ModusConstant, V: ModusVariable> RenameWithSubstitution<C, V> for Term<C, V> {
    type Output = Term<C, V>;
    fn rename(&self) -> (Self::Output, Substitution<C, V>) {
        let s: Substitution<C, V> = self
            .variables()
            .iter()
            .map(|r| {
                [(r.clone(), Term::Variable(r.rename()))]
                    .iter()
                    .cloned()
                    .collect::<Substitution<C, V>>()
            })
            .reduce(|mut l, r| {
                l.extend(r);
                l
            })
            .unwrap();
        (self.substitute(&s), s)
    }
}

impl<C: ModusConstant, V: ModusVariable> Substitute<C, V> for Literal<C, V> {
    type Output = Literal<C, V>;
    fn substitute(&self, s: &Substitution<C, V>) -> Self::Output {
        Literal {
            atom: self.atom.clone(),
            args: self.args.iter().map(|t| t.substitute(s)).collect(),
        }
    }
}

impl<C: ModusConstant, V: ModusVariable> RenameWithSubstitution<C, V> for Literal<C, V> {
    type Output = Literal<C, V>;
    fn rename(&self) -> (Self::Output, Substitution<C, V>) {
        let s: Substitution<C, V> = self
            .variables()
            .iter()
            .map(|r| {
                [(r.clone(), Term::Variable(r.rename()))]
                    .iter()
                    .cloned()
                    .collect::<Substitution<C, V>>()
            })
            .reduce(|mut l, r| {
                l.extend(r);
                l
            })
            .unwrap();
        (self.substitute(&s), s)
    }
}

impl<C: ModusConstant, V: ModusVariable> Substitute<C, V> for Vec<Literal<C, V>> {
    type Output = Vec<Literal<C, V>>;
    fn substitute(&self, s: &Substitution<C, V>) -> Self::Output {
        self.iter().map(|l| l.substitute(s)).collect()
    }
}

impl<C: ModusConstant, V: ModusVariable> RenameWithSubstitution<C, V> for Vec<Literal<C, V>> {
    type Output = Vec<Literal<C, V>>;
    fn rename(&self) -> (Self::Output, Substitution<C, V>) {
        let s: Substitution<C, V> = self
            .iter()
            .flat_map(|e| e.variables())
            .map(|r| {
                [(r.clone(), Term::Variable(r.rename()))]
                    .iter()
                    .cloned()
                    .collect::<Substitution<C, V>>()
            })
            .reduce(|mut l, r| {
                l.extend(r);
                l
            })
            .unwrap();
        (self.substitute(&s), s)
    }
}

impl<C: ModusConstant, V: ModusVariable> Substitute<C, V> for Clause<C, V> {
    type Output = Clause<C, V>;
    fn substitute(&self, s: &Substitution<C, V>) -> Self::Output {
        Clause {
            head: self.head.substitute(s),
            body: self.body.iter().map(|t| t.substitute(s)).collect(),
        }
    }
}

impl<C: ModusConstant, V: ModusVariable> RenameWithSubstitution<C, V> for Clause<C, V> {
    type Output = Clause<C, V>;
    fn rename(&self) -> (Self::Output, Substitution<C, V>) {
        let s: Substitution<C, V> = self
            .variables()
            .iter()
            .map(|r| {
                [(r.clone(), Term::Variable(r.rename()))]
                    .iter()
                    .cloned()
                    .collect::<Substitution<C, V>>()
            })
            .reduce(|mut l, r| {
                l.extend(r);
                l
            })
            .unwrap_or_default();
        (self.substitute(&s), s)
    }
}

pub fn compose_no_extend<C: ModusConstant, V: ModusVariable>(
    l: &Substitution<C, V>,
    r: &Substitution<C, V>,
) -> Substitution<C, V> {
    let mut result = HashMap::<V, Term<C, V>>::new();
    for (k, v) in l {
        result.insert(k.clone(), v.substitute(r));
    }
    result
}

pub fn compose_extend<C: ModusConstant, V: ModusVariable>(
    l: &Substitution<C, V>,
    r: &Substitution<C, V>,
) -> Substitution<C, V> {
    let mut result = compose_no_extend(l, r);
    result.extend(r.clone());
    result
}

impl<C: ModusConstant, V: ModusVariable> Literal<C, V> {
    pub fn unify(&self, other: &Literal<C, V>) -> Option<Substitution<C, V>> {
        if self.signature() != other.signature() {
            return None;
        }
        let mut s = HashMap::<V, Term<C, V>>::new();
        for (i, self_term) in self.args.iter().enumerate() {
            let other_term = &other.args[i];
            let self_term_subs = self_term.substitute(&s);
            let other_term_subs = other_term.substitute(&s);
            if self_term_subs != other_term_subs {
                match self_term_subs {
                    Term::Variable(v) => {
                        let mut upd = HashMap::<V, Term<C, V>>::new();
                        upd.insert(v.clone(), other_term_subs.clone());
                        s = compose_extend(&s, &upd);
                    }
                    _ => match other_term_subs {
                        Term::Variable(v) => {
                            let mut upd = HashMap::<V, Term<C, V>>::new();
                            upd.insert(v.clone(), self_term_subs.clone());
                            s = compose_extend(&s, &upd);
                        }
                        _ => return None,
                    },
                }
            }
        }
        Some(s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_unifier() {
        let l: logic::toy::Literal = "a(X, c)".parse().unwrap();
        let m: logic::toy::Literal = "a(d, Y)".parse().unwrap();
        let result = l.unify(&m);
        assert!(result.is_some());
        let mgu = result.unwrap();
        assert_eq!(l.substitute(&mgu), m.substitute(&mgu))
    }

    #[test]
    fn complex_unifier() {
        let l: logic::toy::Literal = "p(Y, Y, V, W)".parse().unwrap();
        let m: logic::toy::Literal = "p(X, Z, a, U)".parse().unwrap();
        let result = l.unify(&m);
        assert!(result.is_some());
        let mgu = result.unwrap();
        assert_eq!(l.substitute(&mgu), m.substitute(&mgu));
        assert_eq!(mgu.get("Y".into()), Some(&Term::Variable("Z".into())));
        assert_eq!(mgu.get("X".into()), Some(&Term::Variable("Z".into())));
        assert_eq!(
            mgu.get("V".into()),
            Some(&Term::Constant(Predicate("a".into())))
        );
        assert_eq!(mgu.get("W".into()), Some(&Term::Variable("U".into())));
    }

    #[test]
    fn simple_non_unifiable() {
        let l: logic::toy::Literal = "a(X, b)".parse().unwrap();
        let m: logic::toy::Literal = "a(Y)".parse().unwrap();
        let result = l.unify(&m);
        assert!(result.is_none());
    }

    #[test]
    fn complex_non_unifiable() {
        let l: logic::toy::Literal = "q(X, a, X, b)".parse().unwrap();
        let m: logic::toy::Literal = "q(Y, a, a, Y)".parse().unwrap();
        let result = l.unify(&m);
        assert!(result.is_none());
    }

    #[test]
    fn simple_renaming() {
        let l: logic::toy::Literal = "a(X, X, Y)".parse().unwrap();
        let (m, _) = l.rename();
        assert!(l != m);
        assert!(m.args[0] == m.args[1]);
        assert!(m.args[0] != m.args[2]);
    }
}
