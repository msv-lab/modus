//! This module provides helpers to perform (semantic) analysis over Modusfile ASTs.
//!
//! Primarily, we check predicate kinds based on predicate names and/or corresponding expression bodies.
//! To remedy the problem of recursion, we compute a 'predicate dependency graph' which helps us determine if we
//! should not bother evaluating the kind of some predicate.
//! Note that this does not necessarily mean that it is not solvable, but that it would require a different approach.

use std::collections::{HashMap, HashSet};

use codespan_reporting::diagnostic::Diagnostic;

use crate::logic::Predicate;
use crate::modusfile::Expression;
use crate::Modusfile;

#[derive(Debug, PartialEq, Clone)]
enum Kind {
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

// impl ModusSemantics for Expression {
//     fn kind(&self) -> Result<Kind, Diagnostic<()>> {
//         fn generate_diagnostic(
//             span: &Option<SpannedPosition>,
//             e1: &Expression,
//             e2: &Expression,
//             sem1: &Kind,
//             sem2: &Kind,
//         ) -> Diagnostic<()> {
//             let message = "Couldn't determine type of `Expression`.";
//             let mut labels = Vec::new();
//             if let Some(span) = span {
//                 labels.push(
//                     Label::primary((), span.offset..(span.offset + span.length))
//                         .with_message("This `Expression` type couldn't be determined."),
//                 );
//                 labels.push(
//                     Label::secondary((), Range::from(e1.get_spanned_position().as_ref().unwrap()))
//                         .with_message(format!("this is found to be a `{:?}` expression", sem1)),
//                 );
//                 labels.push(
//                     Label::secondary((), Range::from(e2.get_spanned_position().as_ref().unwrap()))
//                         .with_message(format!("this is found to be a `{:?}` expression", sem2)),
//                 );
//             }

//             let diag = Diagnostic::error()
//                 .with_message(message)
//                 .with_labels(labels);
//             diag
//         }

//         match self {
//             Expression::Literal(lit) => lit.predicate.kind(),
//             Expression::OperatorApplication(_, expr, op) => {
//                 let expr_kind = expr.kind()?;
//                 if expr_kind.is_image() && op.predicate.is_specific_operator("copy") {
//                     Ok(Kind::Layer)
//                 } else if expr_kind.is_image() {
//                     Ok(Kind::Image)
//                 } else {
//                     Ok(expr_kind)
//                 }
//             }
//             Expression::And(span, e1, e2) => {
//                 // Could propogate multiple errors up instead of terminating early with '?'
//                 let sem1 = e1.kind()?;
//                 let sem2 = e2.kind()?;

//                 let is_image_expr =
//                     (sem1.is_image() && !sem2.is_image()) || (sem1.is_logic() && sem2.is_image());
//                 let is_layer_expr = sem1.is_layer() && sem2.is_layer();
//                 let is_logic_expr = sem1.is_logic() && sem2.is_logic();

//                 if is_image_expr {
//                     Ok(Kind::Image)
//                 } else if is_layer_expr {
//                     Ok(Kind::Layer)
//                 } else if is_logic_expr {
//                     Ok(Kind::Logic)
//                 } else {
//                     Err(generate_diagnostic(span, e1, e2, &sem1, &sem2))
//                 }
//             }
//             Expression::Or(span, e1, e2) => {
//                 let sem1 = e1.kind()?;
//                 let sem2 = e2.kind()?;

//                 let matching_expr = sem1 == sem2;
//                 if matching_expr {
//                     Ok(sem1)
//                 } else {
//                     Err(generate_diagnostic(span, e1, e2, &sem1, &sem2))
//                 }
//             }
//         }
//     }
// }

/// A trait for objects that have some interpretation w.r.t. the build graph.
trait ModusSemantics {
    /// Returns the kind based on it's name.
    fn naive_predicate_kind(pred: Predicate) -> Kind {
        match pred.0.as_str() {
            "from" => Kind::Image,
            "run" | "copy" => Kind::Layer,
            _ => Kind::Logic,
        }
    }

    fn kind(&self) -> Vec<Result<Kind, Diagnostic<()>>>;
}

impl ModusSemantics for Modusfile {
    fn kind(&self) -> Vec<Result<Kind, Diagnostic<()>>> {
        todo!()
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
    use petgraph::algo::{is_cyclic_directed, is_isomorphic};

    use crate::{analysis::PredicateDependency, modusfile::Modusfile};

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
        assert!(is_isomorphic(&expected_dep, &actual_dep));
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
}
