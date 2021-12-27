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

use crate::logic::{Clause, IRTerm, Literal, Predicate};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SelectBuiltinResult {
    Match,
    GroundnessMismatch,
    NoMatch,
}

impl SelectBuiltinResult {
    pub fn is_match(&self) -> bool {
        match self {
            SelectBuiltinResult::Match => true,
            _ => false,
        }
    }
}

pub trait BuiltinPredicate {
    fn name(&self) -> &'static str;

    /// Return if the argument is allowed to be ungrounded. This means that a "false" here will force a constant.
    fn arg_groundness(&self) -> &'static [bool];

    fn select(&self, lit: &Literal) -> SelectBuiltinResult {
        let Literal {
            ref predicate,
            ref args,
        } = lit;
        if &predicate.0 != self.name() {
            return SelectBuiltinResult::NoMatch;
        }
        if args
            .iter()
            .zip(self.arg_groundness().into_iter())
            .all(|pair| matches!(pair, (_, true) | (IRTerm::Constant(_), false)))
        {
            SelectBuiltinResult::Match
        } else {
            SelectBuiltinResult::GroundnessMismatch
        }
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
    fn apply(&self, lit: &Literal) -> Option<Literal>;
}

mod string_concat {
    use super::BuiltinPredicate;
    use crate::logic::{IRTerm, Literal, Predicate};

    fn string_concat_result(a: &str, b: &str, c: &str) -> Option<Literal> {
        Some(Literal {
            predicate: Predicate("string_concat".to_owned()),
            args: vec![
                IRTerm::Constant(a.to_owned()),
                IRTerm::Constant(b.to_owned()),
                IRTerm::Constant(c.to_owned()),
            ],
        })
    }

    pub struct StringConcat1;
    impl BuiltinPredicate for StringConcat1 {
        fn name(&self) -> &'static str {
            "string_concat"
        }

        fn arg_groundness(&self) -> &'static [bool] {
            &[false, false, true]
        }

        fn apply(&self, lit: &Literal) -> Option<Literal> {
            let a = lit.args[0].as_constant()?;
            let b = lit.args[1].as_constant()?;
            let c = a.to_owned() + b;
            string_concat_result(a, b, &c)
        }
    }

    pub struct StringConcat2;
    impl BuiltinPredicate for StringConcat2 {
        fn name(&self) -> &'static str {
            "string_concat"
        }

        fn arg_groundness(&self) -> &'static [bool] {
            &[true, false, false]
        }
        fn apply(&self, lit: &Literal) -> Option<Literal> {
            let b = lit.args[1].as_constant()?;
            let c = lit.args[2].as_constant()?;
            if let Some(a) = c.strip_suffix(&b) {
                string_concat_result(a, b, c)
            } else {
                None
            }
        }
    }

    pub struct StringConcat3;
    impl BuiltinPredicate for StringConcat3 {
        fn name(&self) -> &'static str {
            "string_concat"
        }

        fn arg_groundness(&self) -> &'static [bool] {
            &[false, true, false]
        }

        fn apply(&self, lit: &Literal) -> Option<Literal> {
            let a = lit.args[0].as_constant()?;
            let c = lit.args[2].as_constant()?;
            if let Some(b) = c.strip_prefix(&a) {
                string_concat_result(a, b, c)
            } else {
                None
            }
        }
    }
}

macro_rules! intrinsic_predicate {
    ($name:ident, $($arg_groundness:expr),*) => {
        #[allow(non_camel_case_types)]
        pub struct $name;
        impl BuiltinPredicate for $name {
            fn name(&self) -> &'static str {
                stringify!($name)
            }

            fn arg_groundness(&self) -> &'static [bool] {
                &[$($arg_groundness),*]
            }

            fn apply(&self, lit: &Literal) -> Option<Literal> {
                Some(lit.clone())
            }
        }
    };
}

intrinsic_predicate!(run, false);
intrinsic_predicate!(from, false);
intrinsic_predicate!(_operator_copy_begin, false, false, false);
intrinsic_predicate!(_operator_copy_end, false, false, false);
intrinsic_predicate!(_operator_in_workdir_begin, false, false);
intrinsic_predicate!(_operator_in_workdir_end, false, false);
intrinsic_predicate!(_operator_set_workdir_begin, false, false);
intrinsic_predicate!(_operator_set_workdir_end, false, false);
intrinsic_predicate!(_operator_set_entrypoint_begin, false, false);
intrinsic_predicate!(_operator_set_entrypoint_end, false, false);
intrinsic_predicate!(copy, false, false);

/// Convenience macro that returns Some(b) for the first b that can be selected.
macro_rules! select_builtins {
    ( $lit:expr, $( $x:expr ),+ ) => {{
        let mut has_ground_mismatch = false;
        $(
            match $x.select($lit) {
                SelectBuiltinResult::Match => return (SelectBuiltinResult::Match, Some(&$x)),
                SelectBuiltinResult::GroundnessMismatch => {
                    has_ground_mismatch = true;
                },
                _ => {}
            }
        );+
        if has_ground_mismatch {
            return (SelectBuiltinResult::GroundnessMismatch, None);
        } else {
            return (SelectBuiltinResult::NoMatch, None);
        }
    }};
}

pub fn select_builtin<'a>(
    lit: &Literal,
) -> (SelectBuiltinResult, Option<&'a dyn BuiltinPredicate>) {
    select_builtins!(
        lit,
        string_concat::StringConcat1,
        string_concat::StringConcat2,
        string_concat::StringConcat3,
        run,
        from,
        _operator_copy_begin,
        _operator_copy_end,
        _operator_in_workdir_begin,
        _operator_in_workdir_end,
        _operator_set_workdir_begin,
        _operator_set_workdir_end,
        _operator_set_entrypoint_begin,
        _operator_set_entrypoint_end,
        copy
    )
}

#[cfg(test)]
mod test {
    use crate::{builtin::SelectBuiltinResult, logic::IRTerm};

    #[test]
    pub fn test_select() {
        use crate::logic::{Literal, Predicate};

        let lit = Literal {
            predicate: Predicate("run".to_owned()),
            args: vec![IRTerm::Constant("hello".to_owned())],
        };
        let b = super::select_builtin(&lit);
        assert!(b.0.is_match());
        let b = b.1.unwrap();
        assert_eq!(b.name(), "run");
        assert_eq!(b.apply(&lit), Some(lit));

        let lit = Literal {
            predicate: Predicate("string_concat".to_owned()),
            args: vec![
                IRTerm::Constant("hello".to_owned()),
                IRTerm::Constant("world".to_owned()),
                IRTerm::UserVariable("X".to_owned()),
            ],
        };
        let b = super::select_builtin(&lit);
        assert!(b.0.is_match());
        let b = b.1.unwrap();
        assert_eq!(b.name(), "string_concat");
        assert_eq!(
            b.apply(&lit),
            Some(Literal {
                predicate: Predicate("string_concat".to_owned()),
                args: vec![
                    IRTerm::Constant("hello".to_owned()),
                    IRTerm::Constant("world".to_owned()),
                    IRTerm::Constant("helloworld".to_owned()),
                ]
            })
        );

        let lit = Literal {
            predicate: Predicate("xxx".to_owned()),
            args: vec![IRTerm::Constant("hello".to_owned())],
        };
        let b = super::select_builtin(&lit);
        assert_eq!(b.0, SelectBuiltinResult::NoMatch);
    }

    #[test]
    pub fn test_from_run() {
        use crate::logic::{Clause, Literal, Predicate};

        let rules = vec![Clause {
            head: Literal {
                predicate: Predicate("a".to_owned()),
                args: vec![],
            },
            body: vec![
                Literal {
                    predicate: Predicate("from".to_owned()),
                    args: vec![IRTerm::Constant("ubuntu".to_owned())],
                },
                Literal {
                    predicate: Predicate("run".to_owned()),
                    args: vec![IRTerm::Constant("rm -rf /".to_owned())],
                },
            ],
        }];
        let goals = vec![Literal {
            predicate: Predicate("a".to_owned()),
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
