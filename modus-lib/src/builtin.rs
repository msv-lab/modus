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

use crate::{
    analysis::Kind,
    logic::{Clause, IRTerm, Literal, Predicate},
};

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

    /// The kind of this predicate or operator.
    /// Should match https://github.com/modus-continens/docs/blob/main/src/library/README.md
    fn kind(&self) -> Kind;

    /// Return if the argument is allowed to be ungrounded. This means that a "false" here will force a constant.
    fn arg_groundness(&self) -> &'static [bool];

    fn select(&self, lit: &Literal) -> SelectBuiltinResult {
        let Literal {
            ref predicate,
            ref args,
            ..
        } = lit;
        if &predicate.0 != self.name() {
            return SelectBuiltinResult::NoMatch;
        }
        if args.len() == self.arg_groundness().len()
            && args.iter().zip(self.arg_groundness().into_iter()).all(
                |(term, allows_ungrounded)| {
                    *allows_ungrounded || term.is_constant_or_compound_constant()
                },
            )
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
    use crate::logic::{IRTerm, Literal, Predicate, SpannedPosition};

    fn string_concat_result(
        a: &str,
        b: &str,
        c: &str,
        pos: &Option<SpannedPosition>,
    ) -> Option<Literal> {
        Some(Literal {
            positive: true,
            position: pos.clone(),
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

        fn kind(&self) -> crate::analysis::Kind {
            crate::analysis::Kind::Logic
        }

        fn arg_groundness(&self) -> &'static [bool] {
            &[false, false, true]
        }

        fn apply(&self, lit: &Literal) -> Option<Literal> {
            let a = lit.args[0].as_constant()?;
            let b = lit.args[1].as_constant()?;
            let c = a.to_owned() + b;
            string_concat_result(a, b, &c, &lit.position)
        }
    }

    pub struct StringConcat2;
    impl BuiltinPredicate for StringConcat2 {
        fn name(&self) -> &'static str {
            "string_concat"
        }

        fn kind(&self) -> crate::analysis::Kind {
            crate::analysis::Kind::Logic
        }

        fn arg_groundness(&self) -> &'static [bool] {
            &[true, false, false]
        }

        fn apply(&self, lit: &Literal) -> Option<Literal> {
            let b = lit.args[1].as_constant()?;
            let c = lit.args[2].as_constant()?;
            if let Some(a) = c.strip_suffix(&b) {
                string_concat_result(a, b, c, &lit.position)
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

        fn kind(&self) -> crate::analysis::Kind {
            crate::analysis::Kind::Logic
        }

        fn arg_groundness(&self) -> &'static [bool] {
            &[false, true, false]
        }

        fn apply(&self, lit: &Literal) -> Option<Literal> {
            let a = lit.args[0].as_constant()?;
            let c = lit.args[2].as_constant()?;
            if let Some(b) = c.strip_prefix(&a) {
                string_concat_result(a, b, c, &lit.position)
            } else {
                None
            }
        }
    }
}

mod equality {
    use crate::logic::{IRTerm, Literal, Predicate};

    use super::BuiltinPredicate;

    pub struct StringEq1;
    impl BuiltinPredicate for StringEq1 {
        fn name(&self) -> &'static str {
            "string_eq"
        }

        fn kind(&self) -> crate::analysis::Kind {
            crate::analysis::Kind::Logic
        }

        fn arg_groundness(&self) -> &'static [bool] {
            &[false, true]
        }

        fn apply(&self, lit: &crate::logic::Literal) -> Option<crate::logic::Literal> {
            let a = lit.args[0].as_constant()?;
            Some(Literal {
                positive: true,
                position: lit.position.clone(),
                predicate: Predicate("string_eq".to_owned()),
                args: vec![
                    IRTerm::Constant(a.to_owned()),
                    IRTerm::Constant(a.to_owned()),
                ],
            })
        }
    }

    pub struct StringEq2;
    impl BuiltinPredicate for StringEq2 {
        fn name(&self) -> &'static str {
            "string_eq"
        }

        fn kind(&self) -> crate::analysis::Kind {
            crate::analysis::Kind::Logic
        }

        fn arg_groundness(&self) -> &'static [bool] {
            &[true, false]
        }

        fn apply(&self, lit: &crate::logic::Literal) -> Option<crate::logic::Literal> {
            let b = lit.args[1].as_constant()?;
            Some(Literal {
                positive: true,
                position: lit.position.clone(),
                predicate: Predicate("string_eq".to_owned()),
                args: vec![
                    IRTerm::Constant(b.to_owned()),
                    IRTerm::Constant(b.to_owned()),
                ],
            })
        }
    }
}

mod number {
    use super::BuiltinPredicate;

    macro_rules! define_number_comparison {
        ($name:ident, $cond:expr) => {
            #[allow(non_camel_case_types)]
            pub struct $name;
            impl BuiltinPredicate for $name {
                fn name(&self) -> &'static str {
                    stringify!($name)
                }

                fn kind(&self) -> crate::analysis::Kind {
                    crate::analysis::Kind::Logic
                }

                fn arg_groundness(&self) -> &'static [bool] {
                    &[false, false]
                }

                /// Parses and checks that arg1 > arg2.
                fn apply(&self, lit: &crate::logic::Literal) -> Option<crate::logic::Literal> {
                    let a: f64 = lit.args[0].as_constant().and_then(|s| s.parse().ok())?;
                    let b: f64 = lit.args[1].as_constant().and_then(|s| s.parse().ok())?;
                    if $cond(a, b) {
                        Some(lit.clone())
                    } else {
                        None
                    }
                }
            }
        };
    }

    define_number_comparison!(number_eq, |a, b| a == b);
    define_number_comparison!(number_gt, |a, b| a > b);
    define_number_comparison!(number_lt, |a, b| a < b);
    define_number_comparison!(number_geq, |a, b| a >= b);
    define_number_comparison!(number_leq, |a, b| a <= b);
}

mod semver {
    use super::BuiltinPredicate;
    use semver::{Comparator, Op, Version};

    fn parse_partial_version(s: &str) -> Option<Version> {
        if let Ok(v) = Version::parse(s) {
            return Some(v);
        }
        let mut s = String::from(s);
        s.push_str(".0");
        if let Ok(v) = Version::parse(&s) {
            return Some(v);
        }
        s.push_str(".0");
        if let Ok(v) = Version::parse(&s) {
            return Some(v);
        }
        return None;
    }

    macro_rules! define_semver_comparison {
        ($name:ident, $cond:expr) => {
            #[allow(non_camel_case_types)]
            pub struct $name;
            impl BuiltinPredicate for $name {
                fn name(&self) -> &'static str {
                    stringify!($name)
                }

                fn kind(&self) -> crate::analysis::Kind {
                    crate::analysis::Kind::Logic
                }

                fn arg_groundness(&self) -> &'static [bool] {
                    &[false, false]
                }

                /// Parses and checks that arg1 > arg2.
                fn apply(&self, lit: &crate::logic::Literal) -> Option<crate::logic::Literal> {
                    let a: Version = lit.args[0]
                        .as_constant()
                        .and_then(|s| parse_partial_version(s))?;
                    let b: Comparator = lit.args[1]
                        .as_constant()
                        .and_then(|s| Comparator::parse(&format!("{}{}", $cond, s)).ok())?;
                    if b.matches(&a) {
                        Some(lit.clone())
                    } else {
                        None
                    }
                }
            }
        };
    }

    define_semver_comparison!(semver_exact, "=");
    define_semver_comparison!(semver_gt, ">");
    define_semver_comparison!(semver_lt, "<");
    define_semver_comparison!(semver_geq, ">=");
    define_semver_comparison!(semver_leq, "<=");
}

macro_rules! intrinsic_predicate {
    ($name:ident, $kind:expr, $($arg_groundness:expr),*) => {
        #[allow(non_camel_case_types)]
        pub struct $name;
        impl BuiltinPredicate for $name {
            fn name(&self) -> &'static str {
                stringify!($name)
            }

            fn kind(&self) -> Kind {
                $kind
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

intrinsic_predicate!(run, crate::analysis::Kind::Layer, false);
intrinsic_predicate!(from, crate::analysis::Kind::Image, false);
intrinsic_predicate!(
    _operator_copy_begin,
    crate::analysis::Kind::Image,
    false,
    false,
    false
);
intrinsic_predicate!(
    _operator_copy_end,
    crate::analysis::Kind::Image,
    false,
    false,
    false
);
intrinsic_predicate!(
    _operator_in_workdir_begin,
    crate::analysis::Kind::Layer,
    false,
    false
);
intrinsic_predicate!(
    _operator_in_workdir_end,
    crate::analysis::Kind::Layer,
    false,
    false
);
intrinsic_predicate!(
    _operator_set_workdir_begin,
    crate::analysis::Kind::Image,
    false,
    false
);
intrinsic_predicate!(
    _operator_set_workdir_end,
    crate::analysis::Kind::Image,
    false,
    false
);
intrinsic_predicate!(
    _operator_set_entrypoint_begin,
    crate::analysis::Kind::Image,
    false,
    false
);
intrinsic_predicate!(
    _operator_set_entrypoint_end,
    crate::analysis::Kind::Image,
    false,
    false
);
intrinsic_predicate!(
    _operator_set_label_begin,
    crate::analysis::Kind::Image,
    false,
    false,
    false
);
intrinsic_predicate!(
    _operator_set_label_end,
    crate::analysis::Kind::Image,
    false,
    false,
    false
);
intrinsic_predicate!(
    _operator_set_env_begin,
    crate::analysis::Kind::Image,
    false,
    false,
    false
);
intrinsic_predicate!(
    _operator_set_env_end,
    crate::analysis::Kind::Image,
    false,
    false,
    false
);
intrinsic_predicate!(
    _operator_in_env_begin,
    crate::analysis::Kind::Layer,
    false,
    false,
    false
);
intrinsic_predicate!(
    _operator_in_env_end,
    crate::analysis::Kind::Layer,
    false,
    false,
    false
);
intrinsic_predicate!(
    _operator_append_path_begin,
    crate::analysis::Kind::Image,
    false,
    false
);
intrinsic_predicate!(
    _operator_append_path_end,
    crate::analysis::Kind::Image,
    false,
    false
);
intrinsic_predicate!(copy, crate::analysis::Kind::Layer, false, false);
intrinsic_predicate!(_operator_merge_begin, crate::analysis::Kind::Layer, false);
intrinsic_predicate!(_operator_merge_end, crate::analysis::Kind::Layer, false);

/// Convenience macro that returns Some(b) for the first b that can be selected.
macro_rules! select_builtins {
    ( $lit:expr, $( $x:expr ),+, ) => {{
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
        _operator_set_label_begin,
        _operator_set_label_end,
        _operator_set_env_begin,
        _operator_set_env_end,
        _operator_in_env_begin,
        _operator_in_env_end,
        _operator_append_path_begin,
        _operator_append_path_end,
        copy,
        equality::StringEq1,
        equality::StringEq2,
        _operator_merge_begin,
        _operator_merge_end,
        number::number_eq,
        number::number_gt,
        number::number_lt,
        number::number_geq,
        number::number_leq,
        semver::semver_exact,
        semver::semver_gt,
        semver::semver_lt,
        semver::semver_geq,
        semver::semver_leq,
    )
}

lazy_static! {
    // An operator can take an expression of one kind and produce another kind.
    pub static ref OPERATOR_KIND_MAP: HashMap<&'static str, (Kind, Kind)> = {
        let mut m = HashMap::new();
        m.insert("copy", (Kind::Image, Kind::Layer));
        m.insert("set_env", (Kind::Image, Kind::Image));
        m.insert("set_entrypoint", (Kind::Image, Kind::Image));
        m.insert("set_workdir", (Kind::Image, Kind::Image));
        m.insert("set_label", (Kind::Image, Kind::Image));
        m.insert("append_path", (Kind::Image, Kind::Image));
        m.insert("in_workdir", (Kind::Layer, Kind::Layer));
        m.insert("in_env", (Kind::Layer, Kind::Layer));
        m.insert("merge", (Kind::Layer, Kind::Layer));
        m
    };
}

#[cfg(test)]
mod test {
    use crate::{analysis::Kind, builtin::SelectBuiltinResult, logic::IRTerm};

    #[test]
    pub fn test_select() {
        use crate::logic::{Literal, Predicate};

        let lit = Literal {
            positive: true,
            position: None,
            predicate: Predicate("run".to_owned()),
            args: vec![IRTerm::Constant("hello".to_owned())],
        };
        let b = super::select_builtin(&lit);
        assert!(b.0.is_match());
        let b = b.1.unwrap();
        assert_eq!(b.name(), "run");
        assert_eq!(b.kind(), Kind::Layer);
        assert_eq!(b.apply(&lit), Some(lit));

        let lit = Literal {
            positive: true,
            position: None,
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
        assert_eq!(b.kind(), Kind::Logic);
        assert_eq!(
            b.apply(&lit),
            Some(Literal {
                positive: true,
                position: None,
                predicate: Predicate("string_concat".to_owned()),
                args: vec![
                    IRTerm::Constant("hello".to_owned()),
                    IRTerm::Constant("world".to_owned()),
                    IRTerm::Constant("helloworld".to_owned()),
                ]
            })
        );

        let lit = Literal {
            positive: true,
            position: None,
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
                positive: true,
                position: None,
                predicate: Predicate("a".to_owned()),
                args: vec![],
            },
            body: vec![
                Literal {
                    positive: true,
                    position: None,
                    predicate: Predicate("from".to_owned()),
                    args: vec![IRTerm::Constant("ubuntu".to_owned())],
                },
                Literal {
                    positive: true,
                    position: None,
                    predicate: Predicate("run".to_owned()),
                    args: vec![IRTerm::Constant("rm -rf /".to_owned())],
                },
            ],
        }];
        let goals = vec![Literal {
            positive: true,
            position: None,
            predicate: Predicate("a".to_owned()),
            args: vec![],
        }];
        let tree = crate::sld::sld(&rules, &goals, 100, true).tree;
        let solutions = crate::sld::solutions(&tree);
        assert_eq!(solutions.len(), 1);
        assert!(solutions.contains(&goals));
        let proof = crate::sld::proofs(&tree, &rules, &goals);
        assert_eq!(proof.len(), 1);
    }

    #[test]
    pub fn test_number_and_semver_compare() {
        use crate::logic::{Literal, Predicate};

        let tests = vec![
            (
                "number_eq",
                vec![
                    ("1", "1"),
                    ("1.0", "1"),
                    ("0.0", "0.0"),
                    ("0", "-0"),
                    ("0.2", "0.2"),
                    ("1e-10", "1e-10"),
                    ("1e100", "1e100"),
                    ("42.0", "42.0"),
                ],
                vec![
                    ("0", "1"),
                    ("0", "0.01"),
                    ("1", "-1"),
                    ("1e-10", "0"),
                    ("42.0", "-273.15"),
                    ("NaN", "NaN"),
                ],
            ),
            (
                "number_gt",
                vec![
                    ("1", "0"),
                    ("1e-10", "0"),
                    ("42.0", "-273.15"),
                    ("1e100", "0"),
                ],
                vec![
                    ("42.0", "42.0"),
                    ("42.0", "42.1"),
                    ("0", "1e-10"),
                    ("NaN", "NaN"),
                ],
            ),
            (
                "number_lt",
                vec![
                    ("0", "1"),
                    ("0", "1e-10"),
                    ("-273.15", "42.0"),
                    ("0", "1e100"),
                ],
                vec![
                    ("42.0", "42.0"),
                    ("42.1", "42.0"),
                    ("1e-10", "0"),
                    ("NaN", "NaN"),
                ],
            ),
            (
                "number_geq",
                vec![
                    ("1", "0"),
                    ("1e-10", "0"),
                    ("42.0", "-273.15"),
                    ("1e100", "0"),
                    ("42.0", "42.0"),
                    ("42", "42.0"),
                ],
                vec![("42.0", "42.1"), ("0", "1e-10"), ("NaN", "NaN")],
            ),
            (
                "number_leq",
                vec![
                    ("0", "1"),
                    ("0", "1e-10"),
                    ("-273.15", "42.0"),
                    ("0", "1e100"),
                    ("42.0", "42.0"),
                ],
                vec![("42.1", "42.0"), ("1e-10", "0"), ("NaN", "NaN")],
            ),
            (
                "semver_exact",
                vec![
                    ("1.0.0", "1.0.0"),
                    ("1.0.0", "1.0"),
                    ("1.0.0", "1"),
                    ("0.0.0", "0.0.0"),
                    ("0.1.0-alpha", "0.1.0-alpha"),
                    // TODO: do we want to allow things like this?
                    ("1", "1"),
                    ("1.0", "1"),
                    ("1", "1.0"),
                    ("0.2", "0.2"),
                    ("1.0.1", "1.0"),
                ],
                vec![
                    ("1.0.0", "1.0.1"),
                    ("1.0.0", "1.1"),
                    ("1.0.0", "2"),
                    ("1.0.0", "0"),
                    ("0.0.0", "0.0.1"),
                    ("1", "1.2"),
                    ("0", "-0"),
                    ("0.1.0-beta", "0.1.0-alpha"),
                ],
            ),
            (
                "semver_gt",
                vec![
                    ("3.2.1", "1.2.3"),
                    ("1.2.3", "1.2.1"),
                    ("0.1.0-beta", "0.1.0-alpha"),
                    ("1", "0"),
                    ("1.1", "1.0"),
                    ("1.0.1", "1.0.0"),
                ],
                vec![
                    ("1.1", "1"),
                    ("1.1", "1.2"),
                    ("1.1", "1.1"),
                    ("3.2.1", "3.4.1"),
                ],
            ),
            (
                "semver_lt",
                vec![
                    ("1.2.3", "3.2.1"),
                    ("3.2.1", "3.4.1"),
                    ("1.1", "1.2"),
                    ("1.1", "1.1.1"),
                ],
                vec![
                    ("1", "0"),
                    ("1.1", "1.0"),
                    ("1.1", "1"),
                    ("1.1", "1"),
                    ("1.1", "1.1"),
                    ("1.1.0", "1.1.0"),
                    ("1.1", "1.1.0"),
                    ("1.1.1", "1.1.1"),
                ],
            ),
            (
                "semver_geq",
                vec![
                    ("1.0.1", "1.0.0"),
                    ("1.0.1", "1.0"),
                    ("1.2.3", "1.2.1"),
                    ("1.1", "1.0"),
                    ("1.1", "1"),
                    ("1.1", "1.1"),
                    ("1.1.0", "1.1.0"),
                    ("1.1", "1.1.0"),
                    ("1.1.1", "1.1.1"),
                ],
                vec![("1.2.3", "3.2.1"), ("1.1", "1.2"), ("1.1", "1.1.1")],
            ),
            (
                "semver_leq",
                vec![
                    ("1.2.3", "3.2.1"),
                    ("1.1", "1.2"),
                    ("1.1", "1.1.1"),
                    ("1.1", "1"),
                    ("1.1", "1.1"),
                    ("1.1.0", "1.1.0"),
                    ("1.1", "1.1.0"),
                    ("1.1.1", "1.1.1"),
                ],
                vec![("1", "0"), ("1.1", "1.0"), ("1.2.3", "1.2.1")],
            ),
        ];

        for (name, true_cases, false_cases) in tests.into_iter() {
            for (left, right) in true_cases.into_iter() {
                let lit = Literal {
                    positive: true,
                    position: None,
                    predicate: Predicate(name.to_owned()),
                    args: vec![
                        IRTerm::Constant(left.to_owned()),
                        IRTerm::Constant(right.to_owned()),
                    ],
                };
                let b = super::select_builtin(&lit);
                assert!(b.0.is_match());
                let b = b.1.unwrap();
                assert_eq!(b.name(), name);
                assert_eq!(b.kind(), Kind::Logic);
                if b.apply(&lit).as_ref() != Some(&lit) {
                    panic!("Expected {} to resolve (got false)", lit);
                }
            }
            for (left, right) in false_cases.into_iter() {
                let lit = Literal {
                    positive: true,
                    position: None,
                    predicate: Predicate(name.to_owned()),
                    args: vec![
                        IRTerm::Constant(left.to_owned()),
                        IRTerm::Constant(right.to_owned()),
                    ],
                };
                let b = super::select_builtin(&lit);
                assert!(b.0.is_match());
                let b = b.1.unwrap();
                assert_eq!(b.name(), name);
                assert_eq!(b.kind(), Kind::Logic);
                if b.apply(&lit) != None {
                    panic!("Expected {} to fail (but resolved)", lit);
                }
            }
        }
    }
}
