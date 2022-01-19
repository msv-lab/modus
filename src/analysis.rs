//! This module provides helpers to perform (semantic) analysis over Modusfile ASTs.

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
