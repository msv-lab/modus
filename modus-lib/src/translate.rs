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

use std::{
    collections::{HashMap, HashSet},
    sync::atomic::AtomicUsize,
};

use nom::{bytes::streaming::tag, sequence::delimited};

use crate::{
    logic::{self, parser::Span, IRTerm, Predicate, SpannedPosition},
    modusfile::{
        self,
        parser::{modus_var, outside_format_expansion, process_raw_string},
        Expression, ModusClause, ModusTerm,
    },
    sld::{self, Auxiliary},
};

/// Takes the content of a format string.
/// Returns an IRTerm to be used instead of the format string term, and a list of literals
/// needed to make this equivalent.
fn convert_format_string(
    spanned_position: &SpannedPosition,
    format_string_content: &str,
) -> (Vec<logic::Literal>, IRTerm) {
    let concat_predicate = "string_concat";
    let mut curr_string = format_string_content;
    let mut prev_variable: IRTerm = Auxiliary::aux(false);
    let mut new_literals = vec![logic::Literal {
        positive: true,
        // This initial literal is a no-op that makes the code simpler.
        // It does not have an equivalent position, but we can link to the start
        // of the f-string for clarity.
        position: Some(SpannedPosition {
            length: 1,
            offset: spanned_position.offset,
        }),
        predicate: logic::Predicate(concat_predicate.to_owned()),
        args: vec![
            IRTerm::Constant("".to_owned()),
            IRTerm::Constant("".to_owned()),
            prev_variable.clone(),
        ],
    }];

    let mut curr_string_offset = 2; // skip the `f"` at the start of the f-string.

    // Approach is to parse sections of the string and create new literals, e.g.
    // if the last var we created was R1 and we just parsed some (constant) string c, we
    // add a literal `string_concat(R1, c, R2)`, creating a new variable R2.
    while !curr_string.is_empty() {
        let (i, constant_str) = outside_format_expansion(Span::new(curr_string))
            .expect("can parse outside format expansion");
        let span_length = constant_str.len();
        if span_length > 0 {
            let constant_string = process_raw_string(constant_str.fragment()).replace("\\$", "$");
            let new_var: IRTerm = Auxiliary::aux(false);
            let new_literal = logic::Literal {
                positive: true,
                position: Some(SpannedPosition {
                    offset: spanned_position.offset + curr_string_offset,
                    length: span_length,
                }),
                predicate: logic::Predicate(concat_predicate.to_string()),
                args: vec![
                    prev_variable,
                    IRTerm::Constant(constant_string.to_owned()),
                    new_var.clone(),
                ],
            };
            curr_string_offset += span_length;
            new_literals.push(new_literal);
            prev_variable = new_var;
        }

        // this might fail, e.g. if we are at the end of the string
        let variable_res = delimited(tag("${"), modus_var, tag("}"))(i);
        if let Ok((rest, variable)) = variable_res {
            let new_var: IRTerm = Auxiliary::aux(false);
            let span_length = 2 + variable.fragment().len() + 1; // the variable string surrounded by ${...}
            let new_literal = logic::Literal {
                positive: true,
                position: Some(SpannedPosition {
                    offset: spanned_position.offset + curr_string_offset,
                    length: span_length,
                }),
                predicate: logic::Predicate(concat_predicate.to_string()),
                args: vec![
                    prev_variable,
                    IRTerm::UserVariable(variable.fragment().to_string()),
                    new_var.clone(),
                ],
            };
            curr_string_offset += span_length;
            new_literals.push(new_literal);
            prev_variable = new_var;
            curr_string = rest.fragment();
        } else {
            curr_string = "";
        }
    }
    (new_literals, prev_variable)
}

static OPERATOR_PAIR_ID: AtomicUsize = AtomicUsize::new(0);

#[cfg(test)]
pub(crate) fn reset_operator_pair_id() {
    OPERATOR_PAIR_ID.store(0, std::sync::atomic::Ordering::SeqCst);
}

/// Used to generate unique predicate names in literals that replace negated expressions.
static NEGATION_LITERAL_ID: AtomicUsize = AtomicUsize::new(0);

pub(crate) fn fetch_add_negation_literal_id() -> usize {
    NEGATION_LITERAL_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
}

#[cfg(test)]
pub(crate) fn reset_negation_literal_id() {
    NEGATION_LITERAL_ID.store(0, std::sync::atomic::Ordering::SeqCst);
}

/// Takes a ModusTerm and converts it to an IRTerm.
///
/// If any additional constraints are needed, such as when the term is a format
/// string, the logic predicates are returned in a vector. They need to be added
/// alongside whatever predicate is using this term.
fn translate_term(t: &ModusTerm) -> (IRTerm, Vec<logic::Literal>) {
    match t {
        ModusTerm::Constant(c) => (IRTerm::Constant(process_raw_string(c)), Vec::new()),
        ModusTerm::FormatString {
            position,
            format_string_literal,
        } => {
            let (new_literals, new_var) = convert_format_string(position, format_string_literal);
            (new_var, new_literals)
        }
        ModusTerm::UserVariable(v) => (IRTerm::UserVariable(v.to_owned()), Vec::new()),
        ModusTerm::AnonymousVariable => (sld::Auxiliary::aux(true), Vec::new()),
    }
}

/// Replaces negation on expressions with literals and new clauses.
fn handle_negation(modus_clause: &modusfile::ModusClause) -> Vec<modusfile::ModusClause> {
    fn handle_expression(
        expr: &modusfile::Expression,
        clauses: &mut Vec<modusfile::ModusClause>,
    ) -> modusfile::Expression {
        match expr {
            Expression::Literal(l) => Expression::Literal(l.clone()),
            Expression::OperatorApplication(s, e, op) => Expression::OperatorApplication(
                s.clone(),
                Box::new(handle_expression(e, clauses)),
                op.clone(),
            ),
            Expression::And(s, true, e1, e2) => Expression::And(
                s.clone(),
                true,
                Box::new(handle_expression(e1, clauses)),
                Box::new(handle_expression(e2, clauses)),
            ),
            Expression::Or(s, true, e1, e2) => Expression::Or(
                s.clone(),
                true,
                Box::new(handle_expression(e1, clauses)),
                Box::new(handle_expression(e2, clauses)),
            ),

            Expression::And(s, false, _, _) | Expression::Or(s, false, _, _) => {
                let new_negate_literal = logic::Literal {
                    positive: true,
                    position: None,
                    predicate: Predicate(format!("_negate_{}", fetch_add_negation_literal_id())),
                    args: vec![], // will need to expose the variables later
                };
                let new_clause = modusfile::ModusClause {
                    head: new_negate_literal.clone(),
                    body: Some(expr.negate_current()),
                };

                clauses.extend(handle_negation(&new_clause));
                Expression::Literal(logic::Literal {
                    positive: false,
                    position: s.clone(),
                    ..new_negate_literal
                })
            }
        }
    }

    let mut clauses = Vec::new();
    let new_clause = modusfile::ModusClause {
        head: modus_clause.head.clone(),
        body: modus_clause
            .body
            .as_ref()
            .map(|e| handle_expression(e, &mut clauses)),
    };
    clauses.push(new_clause);
    clauses
}

impl From<&crate::modusfile::ModusClause> for Vec<logic::Clause> {
    /// Convert a ModusClause into one supported by the IR.
    /// It converts logical or/; into multiple rules, which should be equivalent.
    fn from(modus_clause: &crate::modusfile::ModusClause) -> Self {
        fn handle_clause(modus_clause: &modusfile::ModusClause) -> Vec<logic::Clause> {
            match &modus_clause.body {
                Some(Expression::Literal(l)) => {
                    let mut literals: Vec<logic::Literal> = Vec::new();
                    let mut new_literal_args: Vec<logic::IRTerm> = Vec::new();

                    for arg in &l.args {
                        let (translated_arg, new_literals) = translate_term(arg);
                        new_literal_args.push(translated_arg);
                        literals.extend_from_slice(&new_literals);
                    }
                    literals.push(logic::Literal {
                        positive: l.positive,
                        position: l.position.clone(),
                        predicate: l.predicate.clone(),
                        args: new_literal_args,
                    });

                    vec![logic::Clause {
                        head: modus_clause.head.clone().into(),
                        body: literals,
                    }]
                }

                Some(Expression::OperatorApplication(_, expr, op)) => handle_clause(&ModusClause {
                    head: modus_clause.head.clone(),
                    body: Some(*expr.clone()),
                })
                .into_iter()
                .map(|c| {
                    let mut body = Vec::with_capacity(c.body.len() + 2);
                    let mut op_args = Vec::with_capacity(op.args.len() + 1);
                    let id = OPERATOR_PAIR_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
                    op_args.push(IRTerm::Constant(id.to_string()));
                    op_args.extend(op.args.iter().map(|t| {
                        let (t, nl) = translate_term(t);
                        body.extend_from_slice(&nl);
                        t
                    }));
                    body.push(logic::Literal {
                        positive: true,
                        position: op.position.clone(),
                        predicate: Predicate(format!("_operator_{}_begin", &op.predicate.0)),
                        args: op_args.clone(),
                    });
                    body.extend_from_slice(&c.body);
                    body.push(logic::Literal {
                        positive: true,
                        position: op.position.clone(),
                        predicate: Predicate(format!("_operator_{}_end", &op.predicate.0)),
                        args: op_args,
                    });
                    logic::Clause {
                        head: c.head.clone(),
                        body,
                    }
                })
                .collect(),

                Some(Expression::And(_, true, expr1, expr2)) => {
                    let c1 = handle_clause(&ModusClause {
                        head: modus_clause.head.clone(),
                        body: Some(*expr1.clone()),
                    });
                    let c2 = handle_clause(&ModusClause {
                        head: modus_clause.head.clone(),
                        body: Some(*expr2.clone()),
                    });

                    let mut clauses = Vec::new();
                    // If we have the possible rules for left and right sub expressions,
                    // consider the cartesian product of them.
                    for clause1 in &c1 {
                        for clause2 in &c2 {
                            clauses.push(logic::Clause {
                                head: clause1.head.clone(),
                                body: clause1
                                    .body
                                    .clone()
                                    .into_iter()
                                    .chain(clause2.body.clone().into_iter())
                                    .collect(),
                            })
                        }
                    }
                    clauses
                }

                Some(Expression::Or(_, true, expr1, expr2)) => {
                    let mut c1 = handle_clause(&ModusClause {
                        head: modus_clause.head.clone(),
                        body: Some(*expr1.clone()),
                    });
                    let mut c2 = handle_clause(&ModusClause {
                        head: modus_clause.head.clone(),
                        body: Some(*expr2.clone()),
                    });

                    c1.append(&mut c2);
                    c1
                }

                // negated expression pairs should be handled in a separate pass
                Some(Expression::And(_, false, _, _)) | Some(Expression::Or(_, false, _, _)) => {
                    unreachable!()
                }

                None => vec![logic::Clause {
                    head: modus_clause.head.clone().into(),
                    body: Vec::new(),
                }],
            }
        }

        /// Converts clauses like `_negate_id :- ...` into `_negate_id(X, Y) :- ...`.
        /// But doesn't expose anonymous variables.
        fn exposed_negate_clauses(ir_clauses: Vec<logic::Clause>) -> Vec<logic::Clause> {
            let mut negated_lit_args: HashMap<&Predicate, Vec<IRTerm>> = HashMap::new();
            for ir_clause in &ir_clauses {
                if ir_clause.head.predicate.0.starts_with("_negate_") {
                    let curr_args = negated_lit_args
                        .entry(&ir_clause.head.predicate)
                        .or_insert(Vec::new());
                    let new_args = ir_clause.variables(false);
                    for arg in new_args {
                        if !curr_args.contains(&arg) {
                            curr_args.push(arg);
                        }
                    }
                }
            }

            ir_clauses
                .iter()
                .cloned()
                .map(|clause| logic::Clause {
                    head: logic::Literal {
                        args: negated_lit_args
                            .get(&clause.head.predicate)
                            .map(|xs| {
                                let mut v: Vec<logic::IRTerm> = xs.to_vec();
                                v.sort();
                                v
                            })
                            .unwrap_or(clause.head.args),
                        ..clause.head
                    },
                    body: clause
                        .body
                        .iter()
                        .cloned()
                        .map(|lit| {
                            if let Some(args) = negated_lit_args.get(&lit.predicate) {
                                logic::Literal {
                                    args: {
                                        let mut v: Vec<logic::IRTerm> = args.to_vec();
                                        v.sort();
                                        v
                                    },
                                    ..lit
                                }
                            } else {
                                lit
                            }
                        })
                        .collect(),
                })
                .collect()
        }

        // convert negated expressions into negated literals, then perform translation as normal
        let without_expr_negation = handle_negation(modus_clause);
        let ir_clauses: Vec<logic::Clause> = without_expr_negation
            .iter()
            .flat_map(handle_clause)
            .collect();

        // We perform this exactly twice because of nested negation.
        // The second time would include variables that are newly exposed in _negate_id.
        // This isn't a fixpoint, it should require exactly two calls.
        let exposed_1 = exposed_negate_clauses(ir_clauses);
        exposed_negate_clauses(exposed_1)
    }
}

pub fn translate_modusfile(mf: &modusfile::Modusfile) -> Vec<logic::Clause> {
    mf.0.iter().flat_map(Vec::from).collect()
}

#[cfg(test)]
mod tests {
    use serial_test::serial;

    use super::*;

    /// Should be called if any tests rely on the variable index.
    /// Note that the code (currently) doesn't rely on the variable indexes, just the tests, for convenience.
    fn setup() {
        logic::AVAILABLE_VARIABLE_INDEX.store(0, std::sync::atomic::Ordering::SeqCst);
        reset_negation_literal_id();
    }

    #[test]
    fn translate_constant_term() {
        let inp1 = r#"Hello\nWorld"#;
        let modus_term1 = ModusTerm::Constant(inp1.to_owned());
        let ir_term = IRTerm::Constant("Hello\nWorld".to_owned());

        assert_eq!(ir_term, translate_term(&modus_term1).0)
    }

    #[test]
    #[serial]
    fn format_string_empty() {
        setup();

        let case = "";

        let lits = vec![logic::Literal {
            positive: true,
            position: Some(SpannedPosition {
                offset: 0,
                length: 1,
            }),
            predicate: logic::Predicate("string_concat".to_owned()),
            args: vec![
                IRTerm::Constant("".to_owned()),
                IRTerm::Constant("".to_owned()),
                IRTerm::AuxiliaryVariable(0),
            ],
        }];

        assert_eq!(
            (lits, IRTerm::AuxiliaryVariable(0)),
            convert_format_string(
                &SpannedPosition {
                    offset: 0,
                    length: case.len() + 3
                },
                case
            )
        );
    }

    #[test]
    #[serial]
    fn format_string_translation() {
        setup();

        let case = "${target_folder}/buildkit-frontend";

        let lits = vec![
            logic::Literal {
                positive: true,
                position: Some(SpannedPosition {
                    offset: 0,
                    length: 1,
                }),
                predicate: logic::Predicate("string_concat".to_owned()),
                args: vec![
                    IRTerm::Constant("".to_owned()),
                    IRTerm::Constant("".to_owned()),
                    IRTerm::AuxiliaryVariable(0),
                ],
            },
            logic::Literal {
                positive: true,
                position: Some(SpannedPosition {
                    offset: 2,
                    length: 16,
                }),
                predicate: logic::Predicate("string_concat".to_owned()),
                args: vec![
                    IRTerm::AuxiliaryVariable(0),
                    IRTerm::UserVariable("target_folder".to_owned()),
                    IRTerm::AuxiliaryVariable(1),
                ],
            },
            logic::Literal {
                positive: true,
                position: Some(SpannedPosition {
                    offset: 18,
                    length: 18,
                }),
                predicate: logic::Predicate("string_concat".to_owned()),
                args: vec![
                    IRTerm::AuxiliaryVariable(1),
                    IRTerm::Constant("/buildkit-frontend".to_owned()),
                    IRTerm::AuxiliaryVariable(2),
                ],
            },
        ];

        assert_eq!(
            (lits, IRTerm::AuxiliaryVariable(2)),
            convert_format_string(
                &SpannedPosition {
                    length: case.len() + 3,
                    offset: 0
                },
                case
            )
        );
    }

    #[test]
    #[serial]
    fn format_string_translation_with_escape() {
        setup();

        let case = r#"use \"${feature}\" like this \${...} \
                      foobar"#;

        let lits = vec![
            logic::Literal {
                positive: true,
                position: Some(SpannedPosition {
                    offset: 0,
                    length: 1,
                }),
                predicate: logic::Predicate("string_concat".to_owned()),
                args: vec![
                    IRTerm::Constant("".to_owned()),
                    IRTerm::Constant("".to_owned()),
                    IRTerm::AuxiliaryVariable(0),
                ],
            },
            logic::Literal {
                positive: true,
                position: Some(SpannedPosition {
                    offset: 2,
                    length: 6,
                }),
                predicate: logic::Predicate("string_concat".to_owned()),
                args: vec![
                    IRTerm::AuxiliaryVariable(0),
                    IRTerm::Constant("use \"".to_owned()),
                    IRTerm::AuxiliaryVariable(1),
                ],
            },
            logic::Literal {
                positive: true,
                position: Some(SpannedPosition {
                    offset: 8,
                    length: 10,
                }),
                predicate: logic::Predicate("string_concat".to_owned()),
                args: vec![
                    IRTerm::AuxiliaryVariable(1),
                    IRTerm::UserVariable("feature".to_owned()),
                    IRTerm::AuxiliaryVariable(2),
                ],
            },
            logic::Literal {
                positive: true,
                position: Some(SpannedPosition {
                    offset: 18,
                    length: 51,
                }),
                predicate: logic::Predicate("string_concat".to_owned()),
                args: vec![
                    IRTerm::AuxiliaryVariable(2),
                    IRTerm::Constant("\" like this ${...} foobar".to_owned()),
                    IRTerm::AuxiliaryVariable(3),
                ],
            },
        ];

        assert_eq!(
            (lits, IRTerm::AuxiliaryVariable(3)),
            convert_format_string(
                &SpannedPosition {
                    length: case.len() + 3,
                    offset: 0
                },
                case
            )
        );
    }

    #[test]
    #[serial]
    fn translates_negated_and() {
        setup();

        let modus_clause: ModusClause = "foo :- !(a, b, c).".parse().unwrap();
        let expected: Vec<logic::Clause> = vec![
            "_negate_0 :- a, b, c.".parse().unwrap(),
            "foo :- !_negate_0.".parse().unwrap(),
        ];

        let actual: Vec<logic::Clause> = (&modus_clause).into();
        assert_eq!(expected.len(), actual.len());
        assert!(expected
            .iter()
            .zip(actual)
            .all(|(a, b)| a.eq_ignoring_position(&b)));
    }

    #[test]
    #[serial]
    fn translates_negated_or() {
        setup();

        let modus_clause: ModusClause = "foo :- !(a; b; c).".parse().unwrap();
        let expected: Vec<logic::Clause> = vec![
            "_negate_0 :- a.".parse().unwrap(),
            "_negate_0 :- b.".parse().unwrap(),
            "_negate_0 :- c.".parse().unwrap(),
            "foo :- !_negate_0.".parse().unwrap(),
        ];

        let actual: Vec<logic::Clause> = (&modus_clause).into();
        assert_eq!(expected.len(), actual.len());
        assert!(expected
            .iter()
            .zip(actual)
            .all(|(a, b)| a.eq_ignoring_position(&b)));
    }

    #[test]
    #[serial]
    fn translated_negated_with_variable() {
        setup();

        let modus_clause: ModusClause = "foo :- !(a(X), b(X)), x(X).".parse().unwrap();
        let expected: Vec<logic::Clause> = vec![
            "_negate_0(X) :- a(X), b(X).".parse().unwrap(),
            "foo :- !_negate_0(X), x(X).".parse().unwrap(),
        ];

        let actual: Vec<logic::Clause> = (&modus_clause).into();
        assert_eq!(expected.len(), actual.len());
        assert!(expected
            .iter()
            .zip(actual)
            .all(|(a, b)| a.eq_ignoring_position(&b)));
    }

    #[test]
    #[serial]
    fn translation_negation_with_anonymous() {
        setup();

        let modus_clause: ModusClause = "foo :- !(a(X, _) ; b(X, Y)), x(X).".parse().unwrap();
        let expected: Vec<logic::Clause> = vec![
            logic::Clause {
                head: "_negate_0(X, Y)".parse().unwrap(),
                body: vec![logic::Literal {
                    positive: true,
                    position: None,
                    predicate: Predicate("a".into()),
                    args: vec![
                        IRTerm::UserVariable("X".into()),
                        IRTerm::AnonymousVariable(0),
                    ],
                }],
            },
            "_negate_0(X, Y) :- b(X, Y).".parse().unwrap(),
            "foo :- !_negate_0(X, Y), x(X).".parse().unwrap(),
        ];

        let actual: Vec<logic::Clause> = (&modus_clause).into();
        assert_eq!(expected.len(), actual.len());
        assert!(expected
            .iter()
            .zip(actual)
            .all(|(a, b)| a.eq_ignoring_position(&b)));
    }

    #[test]
    #[serial]
    fn translation_nested_negation() {
        setup();

        let modus_clause: ModusClause = "foo :- !(a(Z) , !(b(X), c(Y))), x(X).".parse().unwrap();
        let expected: Vec<logic::Clause> = vec![
            "_negate_1(X, Y) :- b(X), c(Y).".parse().unwrap(),
            "_negate_0(X, Y, Z) :- a(Z), !_negate_1(X, Y)."
                .parse()
                .unwrap(),
            "foo :- !_negate_0(X, Y, Z), x(X).".parse().unwrap(),
        ];

        let actual: Vec<logic::Clause> = (&modus_clause).into();
        assert_eq!(expected.len(), actual.len());
        assert!(expected
            .iter()
            .zip(actual)
            .all(|(a, b)| a.eq_ignoring_position(&b)));
    }

    #[test]
    #[serial]
    fn translates_anonymous_variable() {
        setup();

        let modus_clause: ModusClause = "foo(\"bar\", _).".parse().unwrap();
        let expected: Vec<logic::Clause> = vec![logic::Clause {
            head: logic::Literal {
                positive: true,
                position: None,
                predicate: Predicate("foo".into()),
                args: vec![
                    IRTerm::Constant("bar".to_string()),
                    IRTerm::AnonymousVariable(0),
                ],
            },
            body: vec![],
        }];
        let actual: Vec<logic::Clause> = (&modus_clause).into();

        for (a, b) in expected.iter().zip(actual) {
            assert!(a.eq_ignoring_position(&b), "{} {}", a, b);
        }
    }
}
