// Modus, a language for building container images
// Copyright (C) 2022 University College London

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.

// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

use std::collections::HashMap;

use crate::logic;
use logic::{Clause, Ground, IRTerm, Literal, Predicate};

pub type Substitution<T = IRTerm> = HashMap<T, T>;

impl Ground for Substitution {
    fn is_ground(&self) -> bool {
        self.values().all(|v| v.is_ground())
    }
}

pub trait Substitute<T> {
    type Output;

    fn substitute(&self, s: &Substitution<T>) -> Self::Output;
}

pub trait Rename<T> {
    fn rename(&self) -> T;
}

pub trait RenameWithSubstitution<T> {
    type Output;

    fn rename_with_sub(&self) -> (Self::Output, Substitution<T>);
}

impl Substitute<IRTerm> for IRTerm {
    type Output = IRTerm;
    fn substitute(&self, s: &Substitution<IRTerm>) -> Self::Output {
        // check hashmap first, in case intention was to replace entire
        // the IRTerm::Array
        if let Some(v) = s.get(self) {
            v.clone()
        } else if let IRTerm::Array(ts) = self {
            IRTerm::Array(ts.iter().map(|t| t.substitute(s)).collect())
        } else {
            self.clone()
        }
    }
}

impl RenameWithSubstitution<IRTerm> for IRTerm {
    type Output = IRTerm;
    fn rename_with_sub(&self) -> (Self::Output, Substitution<IRTerm>) {
        let s: Substitution<IRTerm> = self
            .variables(true)
            .iter()
            .map(|r| {
                [(r.clone(), r.rename())]
                    .iter()
                    .cloned()
                    .collect::<Substitution<IRTerm>>()
            })
            .reduce(|mut l, r| {
                l.extend(r);
                l
            })
            .unwrap();
        (self.substitute(&s), s)
    }
}

impl Substitute<IRTerm> for Literal<IRTerm> {
    type Output = Literal;
    fn substitute(&self, s: &Substitution<IRTerm>) -> Self::Output {
        Literal {
            positive: self.positive,
            position: self.position.clone(),
            predicate: self.predicate.clone(),
            args: self.args.iter().map(|t| t.substitute(s)).collect(),
        }
    }
}

impl RenameWithSubstitution<IRTerm> for Literal<IRTerm> {
    type Output = Literal<IRTerm>;
    fn rename_with_sub(&self) -> (Self::Output, Substitution<IRTerm>) {
        let s: Substitution = self
            .variables(true)
            .iter()
            .map(|r| {
                [(r.clone(), r.rename())]
                    .iter()
                    .cloned()
                    .collect::<Substitution>()
            })
            .reduce(|mut l, r| {
                l.extend(r);
                l
            })
            .unwrap();
        (self.substitute(&s), s)
    }
}

impl Substitute<IRTerm> for Vec<Literal<IRTerm>> {
    type Output = Vec<Literal<IRTerm>>;
    fn substitute(&self, s: &Substitution<IRTerm>) -> Self::Output {
        self.iter().map(|l| l.substitute(s)).collect()
    }
}

impl RenameWithSubstitution<IRTerm> for Vec<Literal<IRTerm>> {
    type Output = Vec<Literal<IRTerm>>;
    fn rename_with_sub(&self) -> (Self::Output, Substitution<IRTerm>) {
        let s: Substitution<IRTerm> = self
            .iter()
            .flat_map(|e| e.variables(true))
            .map(|r| {
                [(r.clone(), r.rename())]
                    .iter()
                    .cloned()
                    .collect::<Substitution<IRTerm>>()
            })
            .reduce(|mut l, r| {
                l.extend(r);
                l
            })
            .unwrap();
        (self.substitute(&s), s)
    }
}

impl Substitute<IRTerm> for Clause<IRTerm> {
    type Output = Clause<IRTerm>;
    fn substitute(&self, s: &Substitution<IRTerm>) -> Self::Output {
        Clause {
            head: self.head.substitute(s),
            body: self.body.iter().map(|t| t.substitute(s)).collect(),
        }
    }
}

impl RenameWithSubstitution<IRTerm> for Clause<IRTerm> {
    type Output = Clause<IRTerm>;
    fn rename_with_sub(&self) -> (Self::Output, Substitution<IRTerm>) {
        let s: Substitution<IRTerm> = self
            .variables(true)
            .iter()
            .map(|r| {
                [(r.clone(), r.rename())]
                    .iter()
                    .cloned()
                    .collect::<Substitution<IRTerm>>()
            })
            .reduce(|mut l, r| {
                l.extend(r);
                l
            })
            .unwrap_or_default();
        (self.substitute(&s), s)
    }
}

pub fn compose_no_extend(
    l: &Substitution<IRTerm>,
    r: &Substitution<IRTerm>,
) -> Substitution<IRTerm> {
    let mut result = Substitution::<IRTerm>::new();
    for (k, v) in l {
        result.insert(k.clone(), v.substitute(r));
    }
    result
}

pub fn compose_extend(l: &Substitution<IRTerm>, r: &Substitution<IRTerm>) -> Substitution<IRTerm> {
    let mut result = compose_no_extend(l, r);
    result.extend(r.clone());
    result
}

impl Literal<IRTerm> {
    pub fn unify(&self, other: &Literal<IRTerm>) -> Option<Substitution<IRTerm>> {
        fn unify_arglist(current: &[IRTerm], other: &[IRTerm]) -> Option<Substitution<IRTerm>> {
            let mut s = Substitution::<IRTerm>::new();
            for (self_term, other_term) in current.iter().zip(other) {
                let self_term_subs = self_term.substitute(&s);
                let other_term_subs = other_term.substitute(&s);
                if self_term_subs != other_term_subs {
                    match (self_term_subs.clone(), other_term_subs.clone()) {
                        // cannot unify if they are both different constants
                        (IRTerm::Constant(_), IRTerm::Constant(_)) => return None,

                        (IRTerm::Array(terms1), IRTerm::Array(terms2)) => {
                            if terms1.len() == terms2.len() {
                                if let Some(new_sub) = unify_arglist(&terms1, &terms2) {
                                    s = compose_extend(&s, &new_sub);
                                } else {
                                    return None;
                                }
                            } else {
                                return None;
                            }
                        }
                        // only valid branch of an array is if they're both arrays, as caught above
                        (IRTerm::Array(_), _) | (_, IRTerm::Array(_)) => return None,

                        (IRTerm::Constant(_), v) => {
                            let mut upd = Substitution::<IRTerm>::new();
                            upd.insert(v.clone(), self_term_subs.clone());
                            s = compose_extend(&s, &upd);
                        }
                        (v1, v2) => {
                            let mut upd = Substitution::<IRTerm>::new();
                            upd.insert(v1.clone(), v2.clone());
                            s = compose_extend(&s, &upd);
                        }
                    }
                }
            }
            Some(s)
        }

        if self.signature() != other.signature() {
            return None;
        }
        unify_arglist(&self.args, &other.args)
    }
}

#[cfg(test)]
mod tests {
    use serial_test::serial;

    use super::*;

    #[test]
    fn simple_unifier() {
        let l: logic::Literal = "a(X, \"c\")".parse().unwrap();
        let m: logic::Literal = "a(\"d\", Y)".parse().unwrap();
        let result = l.unify(&m);
        assert!(result.is_some());
        let mgu = result.unwrap();
        assert_eq!(l.substitute(&mgu), m.substitute(&mgu))
    }

    #[test]
    fn complex_unifier() {
        let l: logic::Literal = "p(Y, Y, V, W)".parse().unwrap();
        let m: logic::Literal = "p(X, Z, \"a\", U)".parse().unwrap();
        let result = l.unify(&m);
        assert!(result.is_some());
        let mgu = result.unwrap();
        assert!(l.substitute(&mgu).eq_ignoring_position(&m.substitute(&mgu)));
        assert_eq!(
            mgu.get(&logic::IRTerm::UserVariable("Y".into())),
            Some(&logic::IRTerm::UserVariable("Z".into()))
        );
        assert_eq!(
            mgu.get(&logic::IRTerm::UserVariable("X".into())),
            Some(&logic::IRTerm::UserVariable("Z".into()))
        );
        assert_eq!(
            mgu.get(&logic::IRTerm::UserVariable("V".into())),
            Some(&logic::IRTerm::Constant("a".into()))
        );
        assert_eq!(
            mgu.get(&logic::IRTerm::UserVariable("W".into())),
            Some(&logic::IRTerm::UserVariable("U".into()))
        );
    }

    #[test]
    fn array_unifier() {
        let l: logic::Literal = "p([X, \"b\"], X)".parse().unwrap();
        let m: logic::Literal = "p([\"a\", Y], X)".parse().unwrap();
        let result = l.unify(&m);
        assert!(result.is_some());
        let mgu = result.unwrap();
        assert!(l.substitute(&mgu).eq_ignoring_position(&m.substitute(&mgu)));
        assert_eq!(
            mgu.get(&logic::IRTerm::UserVariable("X".into())),
            Some(&logic::IRTerm::Constant("a".into()))
        );
        assert_eq!(
            mgu.get(&logic::IRTerm::UserVariable("Y".into())),
            Some(&logic::IRTerm::Constant("b".into()))
        );
    }

    #[test]
    fn simple_non_unifiable() {
        let l: logic::Literal = "a(X, \"b\")".parse().unwrap();
        let m: logic::Literal = "a(Y)".parse().unwrap();
        let result = l.unify(&m);
        assert!(result.is_none());
    }

    #[test]
    fn complex_non_unifiable() {
        let l: logic::Literal = "q(X, \"a\", X, \"b\")".parse().unwrap();
        let m: logic::Literal = "q(Y, \"a\", \"a\", Y)".parse().unwrap();
        let result = l.unify(&m);
        assert!(result.is_none());
    }

    #[test]
    #[serial]
    fn simple_renaming() {
        let l: logic::Literal = "a(X, X, Y)".parse().unwrap();
        let (m, _) = l.rename_with_sub();
        assert!(l != m);
        assert!(m.args[0] == m.args[1]);
        assert!(m.args[0] != m.args[2]);
    }

    #[test]
    #[serial]
    fn complex_renaming() {
        let l: logic::Literal = "a([ X, Y ], X)".parse().unwrap();
        let (m, _) = l.rename_with_sub();
        assert!(l != m);
        if let IRTerm::Array(ts) = &m.args[0] {
            assert_eq!(ts[0], m.args[1]);
            assert_ne!(ts[1], m.args[1]);
        } else {
            panic!()
        }
    }
}
