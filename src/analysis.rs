//! This module provides helpers to perform (semantic) analysis over Modusfile ASTs.

use petgraph::Graph;

use crate::logic::Predicate;
use crate::Modusfile;

enum Kind {
    Image,
    Layer,
    Logic
}

/// A trait for objects that have some interpretation w.r.t. the build graph.
trait ModusSemantics {
    fn kind(&self) -> Result<Kind, String>;
}

impl ModusSemantics for Modusfile {
    fn kind(&self) -> Result<Kind, String> {
        todo!()
    }
}

impl ModusSemantics for Predicate {
    fn kind(&self) -> Result<Kind, String> {
        match self.0.as_str() {
            "from" => Ok(Kind::Image),
            "run" | "copy" => Ok(Kind::Layer),
            _ => Ok(Kind::Logic),
        }
    }
}

trait PredicateDependency {
    /// Returns a graph where an edge, (n1, n2), means that the predicate
    /// n1 depends on n2 to compute it's type.
    ///
    /// This uses the assumption that predicate names refer to a unique groundness
    /// signature.
    fn compute_dependency(&self) -> Graph<&str, &str>;
}

impl PredicateDependency for Modusfile {
    fn compute_dependency(&self) -> Graph<&str, &str> {
        todo!()
    }
}
