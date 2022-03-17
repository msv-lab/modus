// Modus, a language for building container images
// Copyright (C) 2022 ANONYMISED

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

use std::collections::{HashMap, HashSet};

use crate::logic::{Clause, IRTerm, Signature};

/// infer image predicates, i.e. those that transitively depend on image/1
/// check that image predicates depend on image/1 in each disjunct
pub fn check_image_predicates(
    clauses: &Vec<Clause<IRTerm>>,
) -> Result<HashSet<Signature>, HashSet<Signature>> {
    todo!()
}

fn combine_groundness(g1: &[bool], g2: &[bool]) -> Vec<bool> {
    debug_assert_eq!(g1.len(), g2.len());

    // NOTE: this'll never fail since the lengths match, i.e. we'll always
    // be able to combine then. The extreme case is when we enforce that
    // all arguments have to be ground.
    let mut new_g = Vec::with_capacity(g1.len());
    for (&allowed1, &allowed2) in g1.iter().zip(g2) {
        // This argument is only allowed to be ungrounded if both are allowed
        // to be ungrounded.
        new_g.push(allowed1 && allowed2);
    }
    new_g
}

// infer grounded variables, check if grounded variables are grounded in each rule
//TODO: not sure what to do if there are variables inside compound terms
pub fn check_grounded_variables(
    clauses: &[Clause<IRTerm>],
) -> Result<HashMap<Signature, Vec<bool>>, HashSet<Signature>> {
    let mut errors: HashSet<Signature> = HashSet::new();
    let mut result: HashMap<Signature, Vec<bool>> = HashMap::new();

    fn infer(c: &Clause<IRTerm>) -> Vec<bool> {
        let body_vars = c
            .body
            .iter()
            .map(|r| r.variables(true))
            .reduce(|mut l, r| {
                l.extend(r);
                l
            })
            .unwrap_or_default();
        c.head
            .args
            .iter()
            .map(|t| match t {
                IRTerm::Constant(_) => true,
                v => body_vars.contains(v),
            })
            .collect()
    }

    let signatures: HashSet<Signature> = clauses.iter().map(|c| c.head.signature()).collect();

    for c in clauses {
        let sig = c.head.signature();
        let grounded = infer(c);

        let new_groundness = if let Some(prev_groundness) = result.get(&sig) {
            combine_groundness(&prev_groundness, &grounded)
        } else {
            grounded
        };
        result.insert(sig, new_groundness);
    }

    if errors.is_empty() {
        Ok(result)
    } else {
        Err(errors)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{logic::Predicate, modusfile};
    #[test]
    fn consistently_grounded() {
        let clauses: Vec<Clause> = vec![
            "a(X, Y) :- b(X), c(X, Z).".parse().unwrap(),
            "a(X, Y) :- d(X).".parse().unwrap(),
            "b(X) :- d(X).".parse().unwrap(),
        ];
        let result = check_grounded_variables(&clauses);
        assert!(result.is_ok());
        let a_sig = Signature(Predicate("a".into()), 2);
        let a_grounded = result.unwrap().get(&a_sig).unwrap().clone();
        assert!(a_grounded[0]);
        assert!(!a_grounded[1]);
    }

    #[test]
    fn correctly_combined_grounded() {
        let clauses: Vec<Clause> = vec![
            "a(X, Y) :- b(X), c(X, Z).".parse().unwrap(),
            "a(X, Y) :- d(Y).".parse().unwrap(),
        ];
        let result = check_grounded_variables(&clauses);
        assert!(result.is_ok());
        let a_sig = Signature(Predicate("a".into()), 2);
        let a_grounded = result.unwrap().get(&a_sig).unwrap().clone();
        assert!(!a_grounded[0]);
        assert!(!a_grounded[1]);
    }

    #[test]
    fn groundness_after_translation() {
        let modus_clause: modusfile::ModusClause = "foo(X) :- bar(X) ; baz.".parse().unwrap();
        let clauses: Vec<Clause> = (&modus_clause).into();
        let result = check_grounded_variables(&clauses);
        assert!(result.is_ok());
        let foo_sig = Signature(Predicate("foo".into()), 1);
        let foo_grounded = result.unwrap().get(&foo_sig).unwrap().clone();
        assert!(!foo_grounded[0]);
    }
}
