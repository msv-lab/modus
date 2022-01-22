//! This module provides helpers to perform (semantic) analysis over Modusfile ASTs.
//!
//! Primarily, we check predicate kinds based on predicate names and/or corresponding expression bodies.
//! To remedy the problem of recursion, we compute a 'predicate dependency graph' which helps us determine if we
//! should not bother evaluating the kind of some predicate.
//! Note that this does not necessarily mean that it is not solvable, but that it would require a different approach.

use std::collections::{HashMap, HashSet};
use std::ops::Range;

use codespan_reporting::diagnostic::{Diagnostic, Label};
use petgraph::algo::has_path_connecting;

use crate::logic::{Predicate, SpannedPosition};
use crate::modusfile::{Expression, ModusClause};
use crate::Modusfile;

#[derive(Debug, PartialEq, Clone)]
pub enum Kind {
    Image,
    Layer,
    Logic,
}

impl Kind {
    /// Returns `true` if the kind is [`Image`].
    ///
    /// [`Image`]: Kind::Image
    fn is_image(&self) -> bool {
        matches!(self, Self::Image)
    }

    /// Returns `true` if the kind is [`Layer`].
    ///
    /// [`Layer`]: Kind::Layer
    fn is_layer(&self) -> bool {
        matches!(self, Self::Layer)
    }

    /// Returns `true` if the kind is [`Logic`].
    ///
    /// [`Logic`]: Kind::Logic
    fn is_logic(&self) -> bool {
        matches!(self, Self::Logic)
    }
}

pub struct KindResult {
    #[cfg(test)]
    pred_kind: HashMap<String, Kind>,

    /// For convenience, informational diagnostic messages that describe the predicate
    /// kind using spans.
    pub messages: Vec<Diagnostic<()>>,
    pub errs: Vec<Diagnostic<()>>,
}

/// A trait for objects that have some interpretation w.r.t. the build graph.
pub trait ModusSemantics {
    fn kinds(&self) -> KindResult;
}

impl ModusSemantics for Modusfile {
    fn kinds(&self) -> KindResult {
        /// Returns the kind based on it's name.
        fn naive_predicate_kind(pred: &Predicate) -> Kind {
            match pred.0.as_str() {
                "from" => Kind::Image,
                "run" | "copy" => Kind::Layer,
                _ => Kind::Logic,
            }
        }

        fn generate_err_diagnostic(
            span: &Option<SpannedPosition>,
            e1: &Expression,
            e2: &Expression,
            sem1: &Kind,
            sem2: &Kind,
        ) -> Diagnostic<()> {
            let message = "Couldn't determine kind of `Expression`.";
            let mut labels = Vec::new();
            if let Some(span) = span {
                labels.push(
                    Label::primary((), Range::from(span))
                        .with_message("This `Expression` kind couldn't be determined."),
                );
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

        fn evaluate_kind(
            expr: &Expression,
            clauses: &[ModusClause],
            pred_rid_map: &HashMap<&str, Vec<RuleId>>,
            pred_kind: &mut HashMap<String, Kind>,
        ) -> Result<Kind, Diagnostic<()>> {
            match expr {
                Expression::Literal(lit) => {
                    // If we haven't seen this predicate kind before, evaluate it and cache it.
                    // Note that we should be able to recursively compute predicate kind because we would've
                    // already computed which predicates are problematic.
                    if let Some(kind) = pred_kind.get(lit.predicate.0.as_str()) {
                        return Ok(kind.clone());
                    }

                    // NOTE: there may be multiple expression bodies related to some predicate name.
                    // We just take the first one. We could/should emit some kind of warning maybe.
                    let maybe_expr = pred_rid_map.get(lit.predicate.0.as_str()).and_then(|rids| {
                        // if it doesn't have a body, it won't give us any new information,
                        // so skip facts
                        rids.iter().find_map(|rid| clauses[*rid].body.as_ref())
                    });
                    if let Some(expr_body) = maybe_expr {
                        let k = evaluate_kind(expr_body, clauses, pred_rid_map, pred_kind)?;
                        pred_kind.insert(lit.predicate.0.clone(), k.clone());
                        Ok(k)
                    } else {
                        Ok(naive_predicate_kind(&lit.predicate))
                    }
                }
                Expression::OperatorApplication(_, expr, op) => {
                    let expr_kind = evaluate_kind(expr, clauses, pred_rid_map, pred_kind)?;
                    if expr_kind.is_image() && op.predicate.0 == "copy" {
                        Ok(Kind::Layer)
                    } else {
                        Ok(expr_kind)
                    }
                }
                Expression::And(span, e1, e2) => {
                    // Could propogate multiple errors up instead of terminating early with '?'
                    let sem1 = evaluate_kind(e1, clauses, pred_rid_map, pred_kind)?;
                    let sem2 = evaluate_kind(e2, clauses, pred_rid_map, pred_kind)?;

                    let is_image_expr = (sem1.is_image() && !sem2.is_image())
                        || (sem1.is_logic() && sem2.is_image());
                    let is_layer_expr = sem1.is_layer() && sem2.is_layer();
                    let is_logic_expr = sem1.is_logic() && sem2.is_logic();

                    if is_image_expr {
                        Ok(Kind::Image)
                    } else if is_layer_expr {
                        Ok(Kind::Layer)
                    } else if is_logic_expr {
                        Ok(Kind::Logic)
                    } else {
                        Err(generate_err_diagnostic(span, e1, e2, &sem1, &sem2))
                    }
                }
                Expression::Or(span, e1, e2) => {
                    let sem1 = evaluate_kind(e1, clauses, pred_rid_map, pred_kind)?;
                    let sem2 = evaluate_kind(e2, clauses, pred_rid_map, pred_kind)?;

                    let matching_expr = sem1 == sem2;
                    if matching_expr {
                        Ok(sem1)
                    } else {
                        Err(generate_err_diagnostic(span, e1, e2, &sem1, &sem2))
                    }
                }
            }
        }

        let pred_graph = self.compute_dependency();

        // Every node in a non-trivial strongly connected component (scc) must be part of
        // a non-trivial cycle.
        // So if there's a path from a node n to a node that's in a non-trivial scc, then
        // node n depends on something involved in a cyclic dependency, and we shouldn't
        // bother trying to evaluate it.
        let sccs = petgraph::algo::tarjan_scc(&pred_graph);
        // TODO: This is probably slow, could optimise this by flipping edge direction and
        // computing reachibility through DFS, etc.?
        let mut problem_nodes = sccs
            .into_iter()
            .filter(|ids| ids.len() > 1)
            .flatten()
            .collect::<HashSet<_>>();
        for node in pred_graph.node_indices() {
            if problem_nodes.contains(&node) {
                continue;
            }

            if problem_nodes
                .iter()
                .any(|c_node| has_path_connecting(&pred_graph, node, *c_node, None))
            {
                problem_nodes.insert(node);
            }
        }
        let problem_preds = problem_nodes
            .into_iter()
            .map(|id| pred_graph[id])
            .collect::<HashSet<_>>();

        // Compute the index positions of the predicates
        type RuleId = usize; // index position w.r.t. clauses
        let mut pred_to_rid: HashMap<&str, Vec<RuleId>> = HashMap::new();
        for (i, c) in self.0.iter().enumerate() {
            let curr = pred_to_rid.entry(&c.head.predicate.0).or_default();
            curr.push(i);
        }

        // Then evaluate all predicate kinds that are not a problem.
        let mut pred_kind: HashMap<String, Kind> = HashMap::new();
        let mut messages = Vec::new();
        let mut errs = Vec::new();

        for c in self
            .0
            .iter()
            .filter(|c| !problem_preds.contains(c.head.predicate.0.as_str()) && c.body.is_some())
        {
            let res = evaluate_kind(
                c.body.as_ref().unwrap(),
                &self.0,
                &pred_to_rid,
                &mut pred_kind,
            );

            match res {
                Ok(kind) => {
                    let pred_name = c.head.predicate.0.as_str();
                    if let Some(k) = pred_kind.get(pred_name) {
                        if k != &kind {
                            // display a warning if the predicate kind is different using
                            // a different expr body
                            let maybe_curr_span =
                                c.body.as_ref().unwrap().get_spanned_position().as_ref();
                            let maybe_prev_span = pred_to_rid[pred_name].iter().find_map(|rid| {
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
                                Diagnostic::warning()
                                    .with_message(
                                        "A rule with matching head predicate has a different kind.",
                                    )
                                    .with_labels(labels),
                            );
                            continue;
                        }
                    }

                    messages.push(generate_msg_diagnostic(c, &kind));
                    pred_kind.insert(pred_name.to_owned(), kind);
                }
                Err(e) => errs.push(e),
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
    fn compute_dependency(&self) -> petgraph::Graph<&str, &str>;
}

impl PredicateDependency for Modusfile {
    fn compute_dependency(&self) -> petgraph::Graph<&str, &str> {
        fn get_predicate_names(expr: &Expression) -> Vec<&str> {
            match expr {
                Expression::Literal(lit) => vec![&lit.predicate.0],
                Expression::OperatorApplication(_, expr, _) => get_predicate_names(expr),
                Expression::And(_, e1, e2) => {
                    let mut pred1 = get_predicate_names(e1);
                    pred1.append(&mut get_predicate_names(e2));
                    pred1
                }
                Expression::Or(_, e1, e2) => {
                    let mut pred1 = get_predicate_names(e1);
                    pred1.append(&mut get_predicate_names(e2));
                    pred1
                }
            }
        }

        let mut predicate_to_dependency: HashMap<&str, HashSet<&str>> = HashMap::new();
        for clause in &self.0 {
            let head_pred = clause.head.predicate.0.as_str();
            if !predicate_to_dependency.contains_key(head_pred) {
                predicate_to_dependency.insert(head_pred, HashSet::new());
            }

            if let Some(expr) = &clause.body {
                let preds = get_predicate_names(expr);
                for pred in &preds {
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
        let mut g = petgraph::Graph::<&str, &str>::new();
        for (k, v) in predicate_to_dependency.iter() {
            if !label_to_idx.contains_key(k) {
                let idx = g.add_node(k);
                label_to_idx.insert(k, idx);
            }

            for pred in v.iter() {
                if !label_to_idx.contains_key(pred) {
                    label_to_idx.insert(pred, g.add_node(pred));
                }
                g.update_edge(label_to_idx[k], label_to_idx[pred], "");
            }
        }

        g
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use petgraph::algo::{is_cyclic_directed, is_isomorphic};

    use crate::{
        analysis::{ModusSemantics, PredicateDependency},
        modusfile::Modusfile,
    };

    #[test]
    fn acyclic_dependency() {
        let clauses = vec!["foo(X) :- bar(X).", "bar(X) :- baz(X)."];
        let mf: Modusfile = clauses.join("\n").parse().unwrap();

        let mut expected_dep = petgraph::Graph::<&str, &str>::new();
        let f = expected_dep.add_node("foo");
        let b = expected_dep.add_node("bar");
        let bz = expected_dep.add_node("baz");
        expected_dep.extend_with_edges(&[(f, b), (b, bz)]);

        let actual_dep = mf.compute_dependency();
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

        let actual_dep = mf.compute_dependency();
        assert!(is_isomorphic(&expected_dep, &actual_dep));
        assert!(is_cyclic_directed(&actual_dep));
    }

    #[test]
    fn simple_image_predicate_kind() {
        let clauses = vec!["a :- from(\"ubuntu\"), run(\"apt-get update\")."];
        let mf: Modusfile = clauses.join("\n").parse().unwrap();

        let kind_res = mf.kinds();
        assert!(kind_res.pred_kind.contains_key("a"));
        assert_eq!(&Kind::Image, kind_res.pred_kind.get("a").unwrap());
    }

    #[test]
    fn layer_predicate_kind() {
        let clauses = vec![
            "a :- from(\"ubuntu\"), run(\"apt-get update\").",
            "b :- a::copy(\".\", \".\").",
        ];
        let mf: Modusfile = clauses.join("\n").parse().unwrap();

        let kind_res = mf.kinds();
        assert_eq!(Some(&Kind::Layer), kind_res.pred_kind.get("b"));
    }

    #[test]
    fn simple_logic_predicate_kind() {
        let clauses = vec!["b(X) :- X = \"test\"."];
        let mf: Modusfile = clauses.join("\n").parse().unwrap();

        let kind_res = mf.kinds();
        assert_eq!(Some(&Kind::Logic), kind_res.pred_kind.get("b"));
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
        assert_eq!(Some(&Kind::Image), kind_res.pred_kind.get("a"));
        assert_eq!(Some(&Kind::Image), kind_res.pred_kind.get("b"));
        assert_eq!(Some(&Kind::Image), kind_res.pred_kind.get("c"));
    }

    #[test]
    fn ignores_recursion() {
        let clauses = vec![
            "foo(X) :- bar(X).",
            "bar(X) :- foo(X).",
            "a(X) :- from(X), run(\"apt-get update\").",
        ];
        let mf: Modusfile = clauses.join("\n").parse().unwrap();

        let kind_res = mf.kinds();
        assert_eq!(Some(&Kind::Image), kind_res.pred_kind.get("a"));
    }
}
