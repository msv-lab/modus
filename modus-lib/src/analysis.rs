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

//! This module provides helpers to perform (semantic) analysis over Modusfile ASTs.
//!
//! We use a fixpoint approach to evaluate predicate kinds, which deals with the issue of recursion.
//! Clauses are repeatedly 'applied' until a fixpoint.
//! Instead of trying to work out the proper order we should 'apply' clauses, we apply all of them,
//! and if we cannot evaluate some expression body because we haven't evaluated a future predicate kind
//! yet, we move on - eventually it will be handled.
//! However, we make an assumption on what an expression body's kind is whenever an incorrect assumption
//! must lead to an error. For example, in `e1 ; e2`, if e1 is an image kind then we'll error if e2
//! is not an image kind, so we may as well assume it is and error later if needed.

use std::collections::{HashMap, HashSet};
use std::io::Write;
use std::ops::Range;

use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};
use codespan_reporting::files::Files;
use codespan_reporting::term::{self, Config};
use petgraph::algo::find_negative_cycle;

use crate::builtin::{select_builtin, OPERATOR_KIND_MAP};
use crate::logic::{self, Literal, Predicate, SpannedPosition};
use crate::modusfile::{Expression, ModusClause, Operator};
use crate::modusfile::{ModusTerm, Modusfile};
use crate::translate::translate_modusfile;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Kind {
    Image,
    Layer,
    Logic,
}

impl Kind {
    /// Returns `true` if the kind is [`Image`].
    ///
    /// [`Image`]: Kind::Image
    pub fn is_image(&self) -> bool {
        matches!(self, Self::Image)
    }

    /// Returns `true` if the kind is [`Layer`].
    ///
    /// [`Layer`]: Kind::Layer
    pub fn is_layer(&self) -> bool {
        matches!(self, Self::Layer)
    }

    /// Returns `true` if the kind is [`Logic`].
    ///
    /// [`Logic`]: Kind::Logic
    pub fn is_logic(&self) -> bool {
        matches!(self, Self::Logic)
    }
}

#[derive(Debug, Clone)]
pub struct KindResult {
    pub pred_kind: HashMap<Predicate, Kind>,

    /// For convenience, informational diagnostic messages that describe the predicate
    /// kind using spans.
    pub messages: Vec<Diagnostic<()>>,
    pub errs: Vec<Diagnostic<()>>,
}

/// A trait for objects that have some interpretation w.r.t. the build graph.
pub trait ModusSemantics {
    fn kinds(&self) -> KindResult;
}

lazy_static! {
    /// Enumerates the possible `e1 , e2 = e3` kinds. One entry represents (e1_kind, e2_kind, resulting_kind).
    /// e.g. if two of the kinds are known, then the third kind is a valid possibility.
    static ref AND_KIND_TABLE: Vec<(Kind, Kind, Kind)> = vec![
        (Kind::Image, Kind::Logic, Kind::Image),
        (Kind::Image, Kind::Layer, Kind::Image),
        (Kind::Logic, Kind::Image, Kind::Image),

        (Kind::Layer, Kind::Layer, Kind::Layer),
        (Kind::Logic, Kind::Layer, Kind::Layer),
        (Kind::Layer, Kind::Logic, Kind::Layer),

        (Kind::Logic, Kind::Logic, Kind::Logic),
    ];

    static ref OR_KIND_TABLE: Vec<(Kind, Kind, Kind)> = vec![
        (Kind::Image, Kind::Image, Kind::Image),
        (Kind::Layer, Kind::Layer, Kind::Layer),
        (Kind::Logic, Kind::Logic, Kind::Logic),
    ];
}

impl ModusSemantics for Modusfile {
    fn kinds(&self) -> KindResult {
        fn generate_err_diagnostic(
            span: &Option<SpannedPosition>,
            e1: &Expression,
            e2: &Expression,
            sem1: &Kind,
            sem2: &Kind,
        ) -> Diagnostic<()> {
            let message = "Couldn't determine kind of `Expression`.";
            let mut labels = Vec::new();
            if span.is_some() {
                labels.push(
                    Label::secondary((), Range::from(e1.get_spanned_position().as_ref().unwrap()))
                        .with_message(format!("this is found to be a `{:?}` expression", sem1)),
                );
                labels.push(
                    Label::secondary((), Range::from(e2.get_spanned_position().as_ref().unwrap()))
                        .with_message(format!("this is found to be a `{:?}` expression", sem2)),
                );
            }

            Diagnostic::error()
                .with_message(message)
                .with_labels(labels)
        }

        fn generate_msg_diagnostic(c1: &ModusClause, k1: &Kind) -> Diagnostic<()> {
            let message = format!("{} is of kind {:?}", c1.head.predicate.0, k1);
            let labels = if let Some(span) = c1.body.as_ref().unwrap().get_spanned_position() {
                vec![Label::primary((), Range::from(span))
                    .with_message(format!("since this is found to be a {:?} expression", k1))]
            } else {
                Vec::new()
            };
            Diagnostic::note().with_message(message).with_labels(labels)
        }

        fn generate_unknown_operator_diag(op: &Operator) -> Diagnostic<()> {
            let diag =
                Diagnostic::error().with_message(format!("Unknown operator: {}", op.predicate));
            if let Some(pos) = &op.position {
                diag.with_labels(vec![Label::primary(
                    (),
                    pos.offset..pos.offset + pos.length,
                )])
            } else {
                diag
            }
        }

        fn generate_failed_assumption(
            expr: &Expression,
            expected: &Kind,
            actual: &Kind,
        ) -> Diagnostic<()> {
            let diag = Diagnostic::error().with_message(format!("Expected kind: {expected:?}"));
            let mut labels = Vec::new();

            if let Some(pos) = expr.get_spanned_position() {
                labels.push(
                    Label::primary((), pos.offset..pos.offset + pos.length)
                        .with_message(format!("expression found to be {actual:?}")),
                );
            }

            diag.with_labels(labels)
        }

        /// Attempts to get the predicate kind of this expression, based on the current findings
        /// of the predicate kind map.
        ///
        /// If assertion is Some(k), then we will either assume that it is of kind k if we don't know,
        /// or we will assert it.
        ///
        /// If head_pred is Some(p), we assume that this expression is the body of a corresponding
        /// rule and modify pred_kind accordingly. This is used to assist in some recursive cases,
        /// i.e. where we know that the top level expression outcome must be some kind k.
        fn evaluate_or_assert_expression(
            expression: &Expression,
            pred_kind: &mut HashMap<Predicate, Kind>,
            clauses: &[ModusClause],
            assertion: Option<Kind>,
            head_pred: Option<&Predicate>,
        ) -> Result<Kind, Diagnostic<()>> {
            fn get_expression_possibilities(
                table: &[(Kind, Kind, Kind)],
                e1: Option<Kind>,
                e2: Option<Kind>,
                e3: Option<Kind>,
            ) -> Vec<(Kind, Kind, Kind)> {
                let num_some = e1.is_some() as i32 + e2.is_some() as i32 + e3.is_some() as i32;
                table
                    .iter()
                    .cloned()
                    .filter(|(t1, t2, t3)| {
                        let match_count = (Some(t1) == e1.as_ref()) as i32
                            + (Some(t2) == e2.as_ref()) as i32
                            + (Some(t3) == e3.as_ref()) as i32;
                        match_count >= num_some
                    })
                    .collect()
            }

            match expression {
                Expression::Literal(lit) => {
                    match (pred_kind.get(&lit.predicate).copied(), assertion) {
                        (None, None) => {
                            if clauses
                                .iter()
                                .any(|c| c.body.is_some() && c.head.predicate == lit.predicate)
                            {
                                // If there is some rule with the desired predicate, we'll defer to evaluating it, instead of
                                // assuming that it is a logical kind.
                                //
                                // This may lead to issues with cases like:
                                // ````
                                // foo(X) :- bar(X).
                                // bar(X) :- foo(X).
                                // ````
                                // Since both will 'defer' to each other. However, this isn't a sensible program on it's own anyway.
                                Err(Diagnostic::warning()
                                    .with_message(format!("{} not determined yet.", lit.predicate)))
                            } else {
                                Ok(Kind::Logic)
                            }
                        }
                        (None, Some(kind_assumption)) => {
                            pred_kind.insert(lit.predicate.clone(), kind_assumption);
                            Ok(kind_assumption)
                        }
                        (Some(existing_k), None) => Ok(existing_k),
                        (Some(existing_k), Some(kind_assumption)) => {
                            if existing_k != kind_assumption {
                                Err(generate_failed_assumption(
                                    expression,
                                    &kind_assumption,
                                    &existing_k,
                                ))
                            } else {
                                Ok(kind_assumption)
                            }
                        }
                    }
                }
                Expression::OperatorApplication(_, expr, op) => {
                    let op_kind_map = OPERATOR_KIND_MAP.get(op.predicate.0.as_str()).copied();

                    if let Some((inp_kind, out_kind)) = op_kind_map {
                        if let Some(kind_assumption) = assertion {
                            if out_kind != kind_assumption {
                                return Err(generate_failed_assumption(
                                    expression,
                                    &kind_assumption,
                                    &out_kind,
                                ));
                            }
                        }
                        if let Some(p) = head_pred {
                            if !pred_kind.contains_key(&p) {
                                pred_kind.insert(p.clone(), out_kind);
                            } else {
                                // could enforce existing is equal to assumption here
                            }
                        }
                        // assert that the input expression is as expected and return out_kind if no errors
                        let _ = evaluate_or_assert_expression(
                            expr,
                            pred_kind,
                            clauses,
                            Some(inp_kind),
                            None,
                        )?;
                        Ok(out_kind)
                    } else {
                        // unknown operator
                        Err(generate_unknown_operator_diag(op))
                    }
                }
                Expression::And(span, true, e1, e2) => {
                    let sem1_res =
                        evaluate_or_assert_expression(e1, pred_kind, clauses, None, None);
                    let sem2_res =
                        evaluate_or_assert_expression(e2, pred_kind, clauses, None, None);

                    let possibilities = get_expression_possibilities(
                        &AND_KIND_TABLE,
                        sem1_res.clone().ok(),
                        sem2_res.clone().ok(),
                        assertion,
                    );
                    if possibilities.len() == 1 {
                        let possibility = possibilities[0];

                        if let Some(p) = head_pred {
                            if !pred_kind.contains_key(&p) {
                                pred_kind.insert(p.clone(), possibility.2);
                            } else {
                                // could enforce existing is equal to assumption here
                            }
                        }

                        let _ = evaluate_or_assert_expression(
                            e1,
                            pred_kind,
                            clauses,
                            Some(possibility.0),
                            None,
                        )?;
                        let _ = evaluate_or_assert_expression(
                            e2,
                            pred_kind,
                            clauses,
                            Some(possibility.1),
                            None,
                        )?;
                        Ok(possibility.2)
                    } else {
                        match (sem1_res, sem2_res) {
                            (Ok(sem1), Ok(sem2)) => {
                                Err(generate_err_diagnostic(span, e1, e2, &sem1, &sem2))
                            }
                            (Err(e), _) | (_, Err(e)) => Err(e),
                        }
                    }
                }
                Expression::Or(span, true, e1, e2) => {
                    let sem1_res =
                        evaluate_or_assert_expression(e1, pred_kind, clauses, assertion, None);
                    let sem2_res =
                        evaluate_or_assert_expression(e2, pred_kind, clauses, assertion, None);

                    let possibilities = get_expression_possibilities(
                        &OR_KIND_TABLE,
                        sem1_res.clone().ok(),
                        sem2_res.clone().ok(),
                        assertion,
                    );
                    if possibilities.len() == 1 {
                        let possibility = possibilities[0];

                        if let Some(p) = head_pred {
                            if !pred_kind.contains_key(&p) {
                                pred_kind.insert(p.clone(), possibility.2);
                            } else {
                                // could enforce existing is equal to assumption here
                            }
                        }

                        let _ = evaluate_or_assert_expression(
                            e1,
                            pred_kind,
                            clauses,
                            Some(possibility.0),
                            None,
                        )?;
                        let _ = evaluate_or_assert_expression(
                            e2,
                            pred_kind,
                            clauses,
                            Some(possibility.1),
                            None,
                        )?;
                        Ok(possibility.2)
                    } else {
                        match (sem1_res, sem2_res) {
                            (Ok(sem1), Ok(sem2)) => {
                                Err(generate_err_diagnostic(span, e1, e2, &sem1, &sem2))
                            }
                            (Err(e), _) | (_, Err(e)) => Err(e),
                        }
                    }
                }
                // A negated expression is a check for whether we can prove the expression,
                // so `!foo` is always a logical kind, regardless of foo.
                &Expression::And(_, false, ..) | Expression::Or(_, false, ..) => match assertion {
                    None | Some(Kind::Logic) => Ok(Kind::Logic),
                    Some(k) => Err(generate_failed_assumption(expression, &k, &Kind::Logic)),
                },
            }
        }

        let from_pred = Predicate("from".into());
        let run_pred = Predicate("run".into());
        let copy_pred = Predicate("copy".into());
        // This initializes the map with the kinds of from/run/copy.
        let mut pred_kind: HashMap<Predicate, Kind> = vec![
            (
                from_pred.clone(),
                select_builtin(&Literal {
                    positive: true,
                    position: None,
                    predicate: from_pred,
                    args: vec![logic::IRTerm::Constant("".to_string())],
                })
                .1
                .unwrap()
                .kind(),
            ),
            (
                run_pred.clone(),
                select_builtin(&Literal {
                    positive: true,
                    position: None,
                    predicate: run_pred,
                    args: vec![logic::IRTerm::Constant("".to_string())],
                })
                .1
                .unwrap()
                .kind(),
            ),
            (
                copy_pred.clone(),
                select_builtin(&Literal {
                    positive: true,
                    position: None,
                    predicate: copy_pred,
                    args: vec![
                        logic::IRTerm::Constant("".to_string()),
                        logic::IRTerm::Constant("".to_string()),
                    ],
                })
                .1
                .unwrap()
                .kind(),
            ),
        ]
        .into_iter()
        .collect();

        // Compute the index positions of the predicates
        type RuleId = usize; // index position w.r.t. clauses
        let mut pred_to_rid: HashMap<&Predicate, Vec<RuleId>> = HashMap::new();
        for (i, c) in self.0.iter().enumerate() {
            let curr = pred_to_rid.entry(&c.head.predicate).or_default();
            curr.push(i);

            if c.body.is_none() {
                // facts are logical kinds
                pred_kind.insert(c.head.predicate.clone(), Kind::Logic);
            }
        }

        loop {
            let mut new_pred = false;
            for c in self.0.iter().filter(|c| c.body.is_some()) {
                let maybe_existing_kind = pred_kind.get(&c.head.predicate).copied();
                let k_res = evaluate_or_assert_expression(
                    c.body.as_ref().unwrap(),
                    &mut pred_kind,
                    &self.0,
                    maybe_existing_kind,
                    Some(&c.head.predicate),
                );
                if let Ok(k) = k_res {
                    if maybe_existing_kind == None {
                        new_pred = true;
                        pred_kind.insert(c.head.predicate.clone(), k);
                    }
                }
            }

            if !new_pred {
                break;
            }
        }

        let mut messages = Vec::new();
        let mut errs = Vec::new();
        // evaluate all the expression bodies a final time to pick up any conflicting definitions
        for c in self.0.iter().filter(|c| c.body.is_some()) {
            let assertion = pred_kind.get(&c.head.predicate).copied();
            match evaluate_or_assert_expression(
                c.body.as_ref().unwrap(),
                &mut pred_kind,
                &self.0,
                assertion,
                Some(&c.head.predicate),
            ) {
                Ok(kind) => {
                    let pred = &c.head.predicate;
                    if let Some(k) = pred_kind.get(pred) {
                        if k != &kind {
                            // Display an error if there is a conflicting predicate kind
                            let maybe_curr_span =
                                c.body.as_ref().unwrap().get_spanned_position().as_ref();
                            let maybe_prev_span = pred_to_rid[&pred].iter().find_map(|rid| {
                                self.0[*rid]
                                    .body
                                    .as_ref()
                                    .and_then(|e| e.get_spanned_position().as_ref())
                            });
                            let labels = if let (Some(span_curr), Some(span_prev)) =
                                (maybe_curr_span, maybe_prev_span)
                            {
                                vec![
                                    Label::secondary((), Range::from(span_prev)).with_message(
                                        format!("previous kind was found to be {:?}", k),
                                    ),
                                    Label::primary((), Range::from(span_curr)).with_message(
                                        format!("but this was found to be a {:?} kind", kind),
                                    ),
                                ]
                            } else {
                                Vec::new()
                            };
                            errs.push(
                                Diagnostic::error()
                                    .with_message(
                                        "A rule with matching head predicate has a different kind.",
                                    )
                                    .with_labels(labels),
                            );
                            continue;
                        }
                    }

                    messages.push(generate_msg_diagnostic(c, &kind));
                    pred_kind.insert(pred.clone(), kind);
                }
                Err(e) => errs.push(e.with_notes(vec![format!(
                    "Therefore, couldn't evaluate clause with head {}.",
                    c.head,
                )])),
            }
        }

        KindResult {
            pred_kind,
            messages,
            errs,
        }
    }
}

trait PredicateDependency {
    /// Returns a graph where an edge, (n1, n2), means that the predicate
    /// n1 depends on n2 to compute it's type.
    ///
    /// This uses the assumption that predicate names refer to a unique groundness
    /// signature.
    fn compute_dependency(
        &self,
    ) -> (
        petgraph::Graph<&str, f32>,
        HashMap<&str, petgraph::graph::NodeIndex>,
    );

    fn stratifiable(&self) -> Result<(), Vec<&str>>;
}

impl PredicateDependency for Modusfile {
    fn compute_dependency(
        &self,
    ) -> (
        petgraph::Graph<&str, f32>,
        HashMap<&str, petgraph::graph::NodeIndex>,
    ) {
        fn get_predicate_positivity(expr: &Expression) -> Vec<(&str, bool)> {
            match expr {
                Expression::Literal(lit) => vec![(&lit.predicate.0, lit.positive)],
                Expression::OperatorApplication(_, expr, _) => get_predicate_positivity(expr),
                Expression::And(_, _, e1, e2) => {
                    let mut pred1 = get_predicate_positivity(e1);
                    pred1.append(&mut get_predicate_positivity(e2));
                    pred1
                }
                Expression::Or(_, _, e1, e2) => {
                    let mut pred1 = get_predicate_positivity(e1);
                    pred1.append(&mut get_predicate_positivity(e2));
                    pred1
                }
            }
        }

        let mut predicate_to_dependency: HashMap<&str, HashSet<(&str, bool)>> = HashMap::new();
        for clause in &self.0 {
            let head_pred = clause.head.predicate.0.as_str();
            if !predicate_to_dependency.contains_key(head_pred) {
                predicate_to_dependency.insert(head_pred, HashSet::new());
            }

            if let Some(expr) = &clause.body {
                let preds = get_predicate_positivity(expr);
                for (pred, _) in &preds {
                    if !predicate_to_dependency.contains_key(pred) {
                        predicate_to_dependency.insert(pred, HashSet::new());
                    }
                }

                let s = predicate_to_dependency.get_mut(head_pred).unwrap();
                s.extend(preds);
            }
        }

        // there may be a better way to init a petgraph, but for now use another map to
        // store indices
        let mut label_to_idx: HashMap<&str, petgraph::graph::NodeIndex> = HashMap::new();
        let mut g = petgraph::Graph::<&str, f32>::new();
        for (k, v) in predicate_to_dependency.iter() {
            if !label_to_idx.contains_key(k) {
                let idx = g.add_node(k);
                label_to_idx.insert(k, idx);
            }

            for &(pred, positivity) in v {
                if !label_to_idx.contains_key(pred) {
                    label_to_idx.insert(pred, g.add_node(pred));
                }
                g.add_edge(
                    label_to_idx[k],
                    label_to_idx[pred],
                    // this allows us to find *a cycle with a negative edge* using
                    // an algorithm for *negative cycles*
                    if positivity { 0.0 } else { -1.0 },
                );
            }
        }

        (g, label_to_idx)
    }

    /// "A logic program is stratified iff the dependency graph contains no cycles
    /// containing a negative edge."
    /// - https://core.ac.uk/download/pdf/228424655.pdf
    fn stratifiable(&self) -> Result<(), Vec<&str>> {
        let (g, node_indices) = self.compute_dependency();

        for id in node_indices.values() {
            if let Some(ids) = find_negative_cycle(&g, *id) {
                return Err(ids
                    .into_iter()
                    .map(|id| *node_indices.iter().find(|&(_, v)| *v == id).unwrap().0)
                    .collect());
            }
        }

        Ok(())
    }
}

fn check_negated_logic_kind(
    ir_clauses: &Vec<logic::Clause>,
    pred_kind: &HashMap<Predicate, Kind>,
) -> Result<(), Vec<Diagnostic<()>>> {
    let mut errs = Vec::new();
    for c in ir_clauses {
        for lit in &c.body {
            let k = pred_kind.get(&lit.predicate);
            // We move on if we don't know what kind this pred is.
            if k.is_some() && k != Some(&Kind::Logic) && !lit.positive {
                // We disallow negating non-logical predicate to encourage better written
                // Modusfiles.
                // Also, maybe SLDNF should be considered an implementation detail and
                // so this would make it easier to switch to different negation semantics.
                let mut diag = Diagnostic::error()
                    .with_message("Negating a non-logical predicate is disallowed.")
                    .with_notes(vec![format!(
                        "{} was found to be of kind {:?}.",
                        lit.predicate,
                        k.unwrap()
                    )]);
                if let Some(s) = &lit.position {
                    diag =
                        diag.with_labels(vec![Label::primary((), s.offset..(s.offset + s.length))]);
                }
                errs.push(diag);
            }
        }
    }

    if errs.is_empty() {
        Ok(())
    } else {
        Err(errs)
    }
}

fn head_term_check(mf: &Modusfile) -> Result<(), Vec<Diagnostic<()>>> {
    fn generate_f_string_diag(pos: &SpannedPosition) -> Diagnostic<()> {
        Diagnostic::error()
            .with_message("A format string was found in a head literal.")
            .with_labels(vec![Label::primary(
                (),
                pos.offset..pos.offset + pos.length,
            )])
    }

    fn generate_array_diag(pos: &SpannedPosition) -> Diagnostic<()> {
        Diagnostic::error()
            .with_message("An array was found in a head literal. This is not supported currently.")
            .with_labels(vec![Label::primary(
                (),
                pos.offset..pos.offset + pos.length,
            )])
    }

    fn check_no_f_string(terms: &[ModusTerm]) -> Result<(), Diagnostic<()>> {
        for t in terms {
            match t {
                ModusTerm::FormatString { position, .. } => {
                    return Err(generate_f_string_diag(position));
                }
                ModusTerm::Array(_, ts) => check_no_f_string(ts)?,
                _ => (),
            }
        }
        Ok(())
    }

    let mut diags = Vec::new();

    for modus_clause in &mf.0 {
        for arg in &modus_clause.head.args {
            match arg {
                ModusTerm::FormatString { position, .. } => {
                    diags.push(generate_f_string_diag(position))
                }
                ModusTerm::Array(position, ts) => {
                    diags.push(generate_array_diag(position));
                    if let Err(d) = check_no_f_string(ts) {
                        diags.push(d)
                    }
                }
                _ => (),
            }
        }
    }

    if diags.is_empty() {
        Ok(())
    } else {
        Err(diags)
    }
}

/// Returns true if the results of the check were satisfactory; we don't need to terminate.
pub fn check_and_output_analysis<
    'files,
    W: Write + codespan_reporting::term::termcolor::WriteColor,
    F: Files<'files, FileId = ()>,
>(
    kind_res: &KindResult,
    mf: &Modusfile,
    verbose: bool,
    out: &mut W,
    config: &Config,
    file: &'files F,
) -> bool {
    if verbose {
        for msg in &kind_res.messages {
            term::emit(out, config, file, &msg).expect("Error when writing to stderr.");
        }
    }

    let head_check_res = head_term_check(mf);
    let head_errors = head_check_res.err().unwrap_or_default();

    let can_translate = head_errors.is_empty();

    let negation_errors = if can_translate {
        let ir_clauses = translate_modusfile(mf);
        check_negated_logic_kind(&ir_clauses, &kind_res.pred_kind)
            .err()
            .unwrap_or_default()
    } else {
        Vec::new()
    };

    let errs = kind_res
        .errs
        .iter()
        .chain(&negation_errors)
        .chain(&head_errors)
        .collect::<Vec<_>>();
    for err in &errs {
        term::emit(out, config, file, err).expect("Error when writing to stderr.");
    }

    let is_stratifiable = mf.stratifiable();
    if let Err(path) = is_stratifiable {
        let path_string = path
            .iter()
            .map(|x| x.to_string())
            .collect::<Vec<_>>()
            .join(" -> ");
        let path_string = "Cycle: ... -> ".to_string() + &path_string + " -> ...";
        let diag = Diagnostic::error()
            .with_message("Program is not stratifiable. Recursive dependency on negation found.")
            .with_notes(vec![path_string]);
        term::emit(out, config, file, &diag).expect("Error when writing to stderr.");
        return false;
    }

    errs.iter().all(|err| err.severity != Severity::Error)
}

#[cfg(test)]
mod tests {
    use super::*;

    use petgraph::algo::{is_cyclic_directed, is_isomorphic};

    #[test]
    fn acyclic_dependency() {
        let clauses = vec!["foo(X) :- bar(X).", "bar(X) :- baz(X)."];
        let mf: Modusfile = clauses.join("\n").parse().unwrap();

        let mut expected_dep = petgraph::Graph::<&str, &str>::new();
        let f = expected_dep.add_node("foo");
        let b = expected_dep.add_node("bar");
        let bz = expected_dep.add_node("baz");
        expected_dep.extend_with_edges(&[(f, b), (b, bz)]);

        let (actual_dep, _) = mf.compute_dependency();
        assert!(is_isomorphic(&expected_dep, &actual_dep)); // isomorphism check may be expensive
        assert!(!is_cyclic_directed(&actual_dep));
    }

    #[test]
    fn cyclic_dependency() {
        let clauses = vec![
            "foo(X) :- from(\"ubuntu\"), run(\"apt-get update\"), bar(X).",
            "bar(X) :- foo(X).",
        ];
        let mf: Modusfile = clauses.join("\n").parse().unwrap();

        let mut expected_dep = petgraph::Graph::<&str, &str>::new();
        let f = expected_dep.add_node("foo");
        let fr = expected_dep.add_node("from");
        let ru = expected_dep.add_node("run");
        let ba = expected_dep.add_node("bar");
        expected_dep.extend_with_edges(&[(f, fr), (f, ru), (f, ba), (ba, f)]);

        let (actual_dep, _) = mf.compute_dependency();
        assert!(is_isomorphic(&expected_dep, &actual_dep));
        assert!(is_cyclic_directed(&actual_dep));
    }

    #[test]
    fn stratifiable_program() {
        let clauses = vec![
            "foo(X) :- from(\"ubuntu\"), run(\"apt-get update\"), bar(X).",
            "bar(X) :- foo(X).",
        ];
        let mf: Modusfile = clauses.join("\n").parse().unwrap();
        assert!(mf.stratifiable().is_ok());
    }

    #[test]
    fn unstratifiable_program() {
        let clauses = vec![
            "foo(X) :- from(\"ubuntu\"), run(\"apt-get update\"), bar(X).",
            "bar(X) :- !foo(X).",
        ];
        let mf: Modusfile = clauses.join("\n").parse().unwrap();
        assert!(mf.stratifiable().is_err());
    }

    #[test]
    fn simple_image_predicate_kind() {
        let clauses = vec!["a :- from(\"ubuntu\"), run(\"apt-get update\"), run(\"echo hello\")."];
        let mf: Modusfile = clauses.join("\n").parse().unwrap();

        let kind_res = mf.kinds();
        let pred = &Predicate("a".into());
        assert_eq!(kind_res.errs.len(), 0);
        assert!(kind_res.pred_kind.contains_key(pred));
        assert_eq!(&Kind::Image, kind_res.pred_kind.get(pred).unwrap());
    }

    #[test]
    fn layer_predicate_kind() {
        let clauses = vec![
            "a :- from(\"ubuntu\"), run(\"apt-get update\").",
            "b :- a::copy(\".\", \".\").",
        ];
        let mf: Modusfile = clauses.join("\n").parse().unwrap();

        let kind_res = mf.kinds();
        assert_eq!(kind_res.errs.len(), 0);
        assert_eq!(
            Some(&Kind::Layer),
            kind_res.pred_kind.get(&Predicate("b".into()))
        );
    }

    #[test]
    fn simple_logic_predicate_kind() {
        let clauses = vec!["b(X) :- X = \"test\"."];
        let mf: Modusfile = clauses.join("\n").parse().unwrap();

        let kind_res = mf.kinds();
        assert_eq!(kind_res.errs.len(), 0);
        assert_eq!(
            Some(&Kind::Logic),
            kind_res.pred_kind.get(&Predicate("b".into()))
        );
    }

    #[test]
    fn simple_indirect_kind() {
        let clauses = vec![
            "a(X) :- from(X), run(\"apt-get update\").",
            "b(X) :- a(X).",
            "c(X) :- b(X).",
        ];
        let mf: Modusfile = clauses.join("\n").parse().unwrap();

        let kind_res = mf.kinds();
        assert_eq!(kind_res.errs.len(), 0);
        assert_eq!(
            Some(&Kind::Image),
            kind_res.pred_kind.get(&Predicate("a".into()))
        );
        assert_eq!(
            Some(&Kind::Image),
            kind_res.pred_kind.get(&Predicate("b".into()))
        );
        assert_eq!(
            Some(&Kind::Image),
            kind_res.pred_kind.get(&Predicate("c".into()))
        );
    }

    #[test]
    fn handles_indirect_recursion() {
        // Since we can evaluate foo in the third clause, we should be able to evaluate
        // `bar`.
        let clauses = vec![
            "foo(X) :- bar(X).",
            "bar(X) :- foo(X).",
            "foo(X) :- from(X), run(\"apt-get update\").",
        ];
        let mf: Modusfile = clauses.join("\n").parse().unwrap();

        let kind_res = mf.kinds();
        assert_eq!(kind_res.errs.len(), 0);
        assert_eq!(
            Some(&Kind::Image),
            kind_res.pred_kind.get(&Predicate("foo".into()))
        );
        assert_eq!(
            Some(&Kind::Image),
            kind_res.pred_kind.get(&Predicate("bar".into()))
        );
    }

    #[test]
    fn handles_recursion_through_presumptions() {
        let clauses = vec![
            "foo(mode) :- ( \
               mode=\"prod\",from(\"alpine\"),foo(\"dev\")::copy(\".\", \".\") \
             ; \
               mode=\"dev\",from(\"gcc\"),run(\"echo hello\") \
             ).",
        ];
        let mf: Modusfile = clauses.join("\n").parse().unwrap();

        let kind_res = mf.kinds();
        assert_eq!(kind_res.errs.len(), 0);
        assert_eq!(
            Some(&Kind::Image),
            kind_res.pred_kind.get(&Predicate("foo".into()))
        );
    }

    #[test]
    fn handles_disjunct_recursion_through_presumptions() {
        let clauses = vec![
            "foo(mode) :- ( \
               mode=\"prod\",foo(\"dev\"),run(\"make prod\") \
             ; \
               mode=\"dev\",copy(\".\", \"/app\"),run(\"echo hello\") \
             ).",
        ];
        let mf: Modusfile = clauses.join("\n").parse().unwrap();

        let kind_res = mf.kinds();
        assert_eq!(kind_res.errs.len(), 0);
        assert_eq!(
            Some(&Kind::Layer),
            kind_res.pred_kind.get(&Predicate("foo".into()))
        );
    }

    #[test]
    fn warns_recursion() {
        let clauses = vec![
            "recurse :- recurse.",
            "a(X) :- from(X), run(\"apt-get update\").",
        ];
        let mf: Modusfile = clauses.join("\n").parse().unwrap();

        let kind_res = mf.kinds();
        assert_eq!(kind_res.errs.len(), 1);
        assert_eq!(kind_res.errs[0].severity, Severity::Warning);
        assert_eq!(
            Some(&Kind::Image),
            kind_res.pred_kind.get(&Predicate("a".into()))
        );
    }

    #[test]
    fn warns_indirect_recursion() {
        let clauses = vec!["foo(X) :- bar(X).", "bar(X) :- foo(X)."];
        let mf: Modusfile = clauses.join("\n").parse().unwrap();

        let kind_res = mf.kinds();
        assert_eq!(kind_res.errs.len(), 2);
        assert_eq!(kind_res.errs[0].severity, Severity::Warning);
        assert_eq!(kind_res.errs[1].severity, Severity::Warning);
    }

    #[test]
    fn errors_conflicting_predicate_kind() {
        let clauses = vec!["foo :- \"1\" = \"1\".", "foo(X) :- from(X)."];
        let mf: Modusfile = clauses.join("\n").parse().unwrap();

        let kind_res = mf.kinds();
        assert_eq!(kind_res.errs.len(), 1);
        assert_eq!(kind_res.errs[0].severity, Severity::Error);
    }

    #[test]
    fn errors_f_string_present_in_head() {
        let clauses = vec![
            "fact_f(f\"foo ${X}\", f\"${Y}\").",
            "fact_f(f\"foo\").",
            "fact_f(f\"foo ${X}\") :- bar, lar.",
        ];
        let mf: Modusfile = clauses.join("\n").parse().unwrap();

        let res = head_term_check(&mf);
        assert!(res.is_err());
        assert_eq!(4, res.err().unwrap().len()); // one for each arg
    }

    #[test]
    fn errors_f_string_present_in_head_array() {
        let clauses = vec!["fact_f([X, f\"foo ${X}\"], f\"${Y}\")."];
        let mf: Modusfile = clauses.join("\n").parse().unwrap();

        let res = head_term_check(&mf);
        assert!(res.is_err());
        assert_eq!(res.as_ref().err().unwrap()[0].severity, Severity::Error);
        assert_eq!(1 + 2, res.err().unwrap().len());
    }

    #[test]
    fn kind_errors_with_unknown_operator() {
        let clauses = vec!["head :- bar::foobar(X, Y), lar.", "lar."];
        let mf: Modusfile = clauses.join("\n").parse().unwrap();

        let kind_res = mf.kinds();
        assert_eq!(kind_res.errs[0].severity, Severity::Error);
        assert!(kind_res.errs[0].message.contains("Unknown operator"));
    }

    #[test]
    fn kind_errors_with_incorrect_operator_inp() {
        let clauses = vec![
            "run_alias :- run(\"echo foobar\").",
            "head :- run_alias::set_entrypoint(\"bash\").",
        ];
        let mf: Modusfile = clauses.join("\n").parse().unwrap();

        let kind_res = mf.kinds();
        assert_eq!(kind_res.errs[0].severity, Severity::Error);
        assert!(kind_res.errs[0].message.contains("Expected kind: Image"));
    }
}
