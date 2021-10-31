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

use crate::logic::{Literal, Term};

pub trait BuiltinPredicate<C, V> {
    fn name(&self) -> &'static str;
    fn arg_groundness(&self) -> &'static [bool];

    fn select(&self, lit: &Literal<C, V>) -> bool {
        let Literal { ref atom, ref args } = lit;
        if &atom.0 != self.name() {
            return false;
        }
        args.iter()
            .zip(self.arg_groundness().into_iter())
            .all(|pair| !matches!(pair, (Term::Variable(_), false)))
    }

    /// Return a new literal specifically constructed to unify with the input
    /// literal. The returned literal will essentially be used as the head of a
    /// new "hidden" rule, which will hopefully unify with the input literal.
    /// The rule will contain no body literals.
    ///
    /// For example, the implementation of run should simply return the input
    /// literal, after checking that it only contains a constant. (Returning any
    /// unresolved variables can make the actual generation of build
    /// instructions impossible)
    ///
    /// Renaming will not be done on this literal, so if variables are needed
    /// they must all be either auxillary or some existing variables from the
    /// input.
    fn apply(&self, lit: &Literal<C, V>) -> Option<Literal<C, V>>;
}

trait MaybeStringConst {
    fn as_str_const(&self) -> Option<String>;
}

impl<C: ToString, V> MaybeStringConst for Term<C, V> {
    fn as_str_const(&self) -> Option<String> {
        match &self {
            Term::Constant(c) => Some(c.to_string()),
            Term::Atom(a) => Some(a.0.clone()),
            _ => None,
        }
    }
}

mod string_concat {
    use super::{BuiltinPredicate, MaybeStringConst};
    use crate::logic::{Atom, Literal, Term};

    fn string_concat_result<C: From<String>, V>(
        a: String,
        b: String,
        c: String,
    ) -> Option<Literal<C, V>> {
        Some(Literal {
            atom: Atom("string_concat".to_owned()),
            args: vec![
                Term::Constant(C::from(a)),
                Term::Constant(C::from(b)),
                Term::Constant(C::from(c)),
            ],
        })
    }

    pub struct StringConcat1;
    impl<C: ToString + From<String>, V> BuiltinPredicate<C, V> for StringConcat1 {
        fn name(&self) -> &'static str {
            "string_concat"
        }

        fn arg_groundness(&self) -> &'static [bool] {
            &[false, false, true]
        }

        fn apply(&self, lit: &Literal<C, V>) -> Option<Literal<C, V>> {
            let a = lit.args[0].as_str_const()?;
            let b = lit.args[1].as_str_const()?;
            let c = a.clone() + &b;
            string_concat_result(a, b, c)
        }
    }

    pub struct StringConcat2;
    impl<C: ToString + From<String>, V> BuiltinPredicate<C, V> for StringConcat2 {
        fn name(&self) -> &'static str {
            "string_concat"
        }

        fn arg_groundness(&self) -> &'static [bool] {
            &[true, false, false]
        }

        fn apply(&self, lit: &Literal<C, V>) -> Option<Literal<C, V>> {
            let b = lit.args[1].as_str_const()?;
            let c = lit.args[2].as_str_const()?;
            if let Some(a) = c.strip_suffix(&b) {
                string_concat_result(a.to_string(), b, c)
            } else {
                None
            }
        }
    }

    pub struct StringConcat3;
    impl<C: ToString + From<String>, V> BuiltinPredicate<C, V> for StringConcat3 {
        fn name(&self) -> &'static str {
            "string_concat"
        }

        fn arg_groundness(&self) -> &'static [bool] {
            &[false, true, false]
        }

        fn apply(&self, lit: &Literal<C, V>) -> Option<Literal<C, V>> {
            let a = lit.args[0].as_str_const()?;
            let c = lit.args[2].as_str_const()?;
            if let Some(b) = c.strip_prefix(&a) {
                string_concat_result(a, b.to_string(), c)
            } else {
                None
            }
        }
    }
}

mod run {
    use crate::logic::{Literal, Term};

    use super::BuiltinPredicate;

    pub struct Run;
    impl<C: Clone, V: Clone> BuiltinPredicate<C, V> for Run {
        fn name(&self) -> &'static str {
            "run"
        }

        fn arg_groundness(&self) -> &'static [bool] {
            &[false]
        }

        fn apply(&self, lit: &Literal<C, V>) -> Option<Literal<C, V>> {
            let l = (*lit).clone();
            match lit.args[0] {
                // it's valid as long as we're given a constant
                Term::Constant(_) => Some(l),
                _ => None,
            }
        }
    }
}

mod from {
    use crate::logic::{Literal, Term};

    use super::BuiltinPredicate;

    pub struct From;
    impl<C: Clone, V: Clone> BuiltinPredicate<C, V> for From {
        fn name(&self) -> &'static str {
            "from"
        }

        fn arg_groundness(&self) -> &'static [bool] {
            &[false]
        }

        fn apply(&self, lit: &Literal<C, V>) -> Option<Literal<C, V>> {
            let l = (*lit).clone();
            match lit.args[0] {
                // it's valid as long as we're given a constant
                Term::Constant(_) => Some(l),
                _ => None,
            }
        }
    }
}

/// Convenience macro that returns Some(b) for the first b that can be selected.
macro_rules! select_builtins {
    ( $lit:expr, $x1:expr, $( $x:expr ),* ) => {
        if $x1.select($lit) {
            return Some(&$x1);
        }
        $(
            else if $x.select($lit) {
                return Some(&$x);
            }
        )*
    };
}

pub fn select_builtin<'a, C, V>(lit: &Literal<C, V>) -> Option<&'a dyn BuiltinPredicate<C, V>>
where
    C: 'a + ToString + From<String> + std::clone::Clone,
    V: 'a + std::clone::Clone,
{
    select_builtins!(
        lit,
        string_concat::StringConcat1,
        string_concat::StringConcat2,
        string_concat::StringConcat3,
        run::Run,
        from::From
    );
    None
}

#[cfg(test)]
mod test {
    #[test]
    pub fn test_select() {
        use crate::logic::toy::{Clause, Literal, Term, Variable};
        use crate::logic::Atom;

        let lit = Literal {
            atom: Atom("run".to_owned()),
            args: vec![Term::Constant(Atom("hello".to_owned()))],
        };
        let b = super::select_builtin(&lit);
        assert!(b.is_some());
        let b = b.unwrap();
        assert_eq!(b.name(), "run");
        assert_eq!(b.apply(&lit), Some(lit));

        let lit = Literal {
            atom: Atom("string_concat".to_owned()),
            args: vec![
                Term::Constant(Atom("hello".to_owned())),
                Term::Constant(Atom("world".to_owned())),
                Term::Variable("X".to_owned()),
            ],
        };
        let b = super::select_builtin(&lit);
        assert!(b.is_some());
        let b = b.unwrap();
        assert_eq!(b.name(), "string_concat");
        assert_eq!(
            b.apply(&lit),
            Some(Literal {
                atom: Atom("string_concat".to_owned()),
                args: vec![
                    Term::Constant(Atom("hello".to_owned())),
                    Term::Constant(Atom("world".to_owned())),
                    Term::Constant(Atom("helloworld".to_owned())),
                ]
            })
        );

        let lit = Literal {
            atom: Atom("xxx".to_owned()),
            args: vec![Term::Constant(Atom("hello".to_owned()))],
        };
        let b = super::select_builtin(&lit);
        assert!(b.is_none());
    }

    #[test]
    pub fn test_from_run() {
        use crate::logic::Atom;
        use crate::modusfile::{Constant as C, Literal, Term, Variable as V};
        type Clause = crate::logic::Clause<C, V>;

        let rules = vec![Clause {
            head: Literal {
                atom: Atom("a".to_owned()),
                args: vec![],
            },
            body: vec![
                Literal {
                    atom: Atom("from".to_owned()),
                    args: vec![Term::Constant(C::String("ubuntu".to_owned()))],
                },
                Literal {
                    atom: Atom("run".to_owned()),
                    args: vec![Term::Constant(C::String("rm -rf /".to_owned()))],
                },
            ],
        }];
        let goals = vec![Literal {
            atom: Atom("a".to_owned()),
            args: vec![],
        }];
        let tree = crate::sld::sld(&rules, &goals, 100).unwrap();
        let solutions = crate::sld::solutions(&tree);
        assert_eq!(solutions.len(), 1);
        assert!(solutions.contains(&goals));
        let proof = crate::sld::proofs(&tree, &rules, &goals);
        assert_eq!(proof.len(), 1);
        println!("{:?}", proof[0]);
    }
}
