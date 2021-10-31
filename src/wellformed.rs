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
    hash::Hash,
};

use crate::logic::{self, Predicate, Clause, Literal, Signature, Term};

/// infer image predicates, i.e. those that transitively depend on image/1
/// check that image predicates depend on image/1 in each disjunct
pub fn check_image_predicates<C, V>(
    clauses: &Vec<Clause<C, V>>,
) -> Result<HashSet<Signature>, HashSet<Signature>> {
    todo!()
}

// infer grounded variables, check if grounded variables are grounded in each rule
//TODO: not sure what to do if there are variables inside compound terms
pub fn check_grounded_variables<C, V>(
    clauses: &Vec<Clause<C, V>>,
) -> Result<HashMap<Signature, Vec<bool>>, HashSet<Signature>>
where
    C: Clone,
    V: Clone + Eq + Hash,
{
    let mut errors: HashSet<Signature> = HashSet::new();
    let mut result: HashMap<Signature, Vec<bool>> = HashMap::new();

    fn infer<C, V>(c: &Clause<C, V>) -> Vec<bool>
    where
        C: Clone,
        V: Clone + Eq + Hash,
    {
        let body_vars = c
            .body
            .iter()
            .map(|r| r.variables())
            .reduce(|mut l, r| {
                l.extend(r);
                l
            })
            .unwrap_or_default();
        c.head
            .args
            .iter()
            .map(|t| match t {
                Term::Variable(v) => body_vars.contains(v),
                _ => true,
            })
            .collect()
    }

    let signatures: HashSet<Signature> = clauses.iter().map(|c| c.head.signature()).collect();

    for c in clauses {
        let sig = c.head.signature();
        let grounded = infer(c);
        if result.contains_key(&sig) {
            if result.get(&sig).unwrap() != &grounded {
                errors.insert(sig);
            }
        } else {
            result.insert(sig, grounded);
        }
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
    #[test]
    fn consistently_grounded() {
        let clauses: Vec<logic::toy::Clause> = vec![
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
    fn inconsistently_grounded() {
        let clauses: Vec<logic::toy::Clause> = vec![
            "a(X, Y) :- b(X), c(X, Z).".parse().unwrap(),
            "a(X, Y) :- d(Y).".parse().unwrap(),
        ];
        let result = check_grounded_variables(&clauses);
        assert!(result.is_err());
        let a_sig = Signature(Predicate("a".into()), 2);
        assert!(result.unwrap_err().contains(&a_sig));
    }
}
