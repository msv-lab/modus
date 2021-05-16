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


use std::{collections::{HashMap, HashSet}, hash::Hash};

use crate::{logic, unification::Substitute};
use logic::{ Clause, Literal, Term, Atom };
use crate::unification::{Substitution, Rename, composition};


pub trait Variable<C, V>: Rename<C, V> {
    fn aux() -> Self; 
}

type ClauseId = usize;
type LiteralId = usize;
type Goal<C, V> = Vec<Literal<C, V>>;
#[derive(Clone)]
pub struct Tree<C, V> {
    goal: Goal<C, V>,
    resolvents: HashMap<(LiteralId, ClauseId), (Substitution<C, V>, Tree<C, V>)>
}

pub struct Proof<C, V> {
    fact: Literal<C, V>,
    cid: ClauseId,
    children: Vec<Proof<C, V>>
}

pub fn sld<C, V>(clauses: &Vec<Clause<C, V>>, goal: &Goal<C, V>, maxdepth: u32) -> Option<Tree<C, V>>
where
    C: Clone + PartialEq,
    V: Clone + Eq + Hash + Variable<C, V>
{
    fn select<C: Clone, V: Clone>(goal: &Goal<C, V>) -> (LiteralId, Literal<C, V>) {
        (0, goal[0].clone())
    }

    fn resolve<C, V>(lid: LiteralId, goal: &Goal<C, V>, mgu: &Substitution<C, V>, c: Clause<C, V>) -> Goal<C, V>
    where
        C: Clone + PartialEq,
        V: Clone + Eq + Hash + Variable<C, V>
    {
        let mut g = goal.clone();
        g.remove(lid);
        g.extend(c.body);
        g.substitute(mgu)
    }
    
    fn inner<C, V>(clauses: &Vec<Clause<C, V>>, goal: &Goal<C, V>, maxdepth: u32, depth: u32) -> Option<Tree<C, V>>
    where
        C: Clone + PartialEq,
        V: Clone + Eq + Hash + Variable<C, V>
    {
        if goal.is_empty() {
            Some(Tree { goal: goal.clone(), resolvents: HashMap::new() })
        } else if depth >= maxdepth {
            None
        } else {
            let (lid, l) = select(goal);
            let resolvents: HashMap<(LiteralId, ClauseId), (Substitution<C, V>, Tree<C, V>)> =
                clauses.iter().enumerate()
                       .filter(|(_, c)| c.head.signature() == l.signature())
                       .map(|(cid, c)| (cid, c.rename().0))
                       .filter_map(|(cid, c)|
                            c.head.unify(&l).and_then(|mgu| Some((cid, mgu.clone(), resolve(lid, &goal, &mgu, c)))))
                       .filter_map(|(cid, mgu, resolvent)|
                            inner(clauses, &resolvent, maxdepth, depth+1).and_then(
                                |tree| Some(((lid, cid), (mgu, tree)))))
                       .collect();
            if resolvents.is_empty() {
                None
            } else {
                Some(Tree { goal: goal.clone(), resolvents })
            }                       
        }    
    }

    inner(clauses, goal, maxdepth, 0)
}

pub fn solutions<C, V>(tree: &Tree<C, V>) -> HashSet<Goal<C, V>>
where
    C: Clone + Eq + Hash,
    V: Clone + Eq + Hash + Variable<C, V>,
{
    fn inner<C: Clone + Eq + Hash, V: Clone + Eq + Hash>(tree: &Tree<C, V>) -> Vec<Substitution<C, V>> {
        if tree.goal.is_empty() {
            let s = Substitution::<C, V>::new();
            return vec![s]
        }
        tree.resolvents.iter()
                        .map(|(_, (mgu, subtree))| (mgu, inner(subtree)))
                        .map(|(mgu, sub)| sub.iter().map(
                            |s| composition(&mgu.clone(), &s.clone())).collect::<Vec<Substitution<C, V>>>())
                        .flatten().collect()
    }
    inner(tree).iter().map(|s| tree.goal.substitute(s)).collect()
}

pub fn optimal_proofs<C, V>(tree: Tree<C, V>, clauses: &Vec<Clause<C, V>>) -> HashSet<Proof<C, V>> {
    todo!()
}

#[cfg(test)]
mod tests {
    use std::{collections::HashMap, sync::atomic::{AtomicU32, Ordering}};

    use super::*;

    static AVAILABLE_INDEX: AtomicU32 = AtomicU32::new(0);

    /// Assume that underscore is not used in normal variables
    impl Rename<logic::Atom, logic::toy::Variable> for logic::toy::Variable {
        type Output = logic::toy::Variable;
        fn rename(&self) -> (Self::Output, Substitution<logic::Atom, logic::toy::Variable>) {
            let index = AVAILABLE_INDEX.fetch_add(1, Ordering::SeqCst);
            let prefix = self.split('_').next().unwrap();
            let renamed = format!("{}_{}", prefix, index);
            let mut s = HashMap::<logic::toy::Variable, logic::Term<logic::Atom, logic::toy::Variable>>::new();
            s.insert(self.clone(), logic::Term::Variable(renamed.clone()));
            (renamed, s)
        }
    }

    impl Variable<logic::Atom, logic::toy::Variable> for logic::toy::Variable {
        fn aux() -> logic::toy::Variable {
            let index = AVAILABLE_INDEX.fetch_add(1, Ordering::SeqCst);
            format!("Aux{}", index)
        }
    }

    #[test]
    fn simple_solving() {
        let goal: Goal<logic::Atom, logic::toy::Variable> = 
            vec!["a(X)".parse().unwrap()];
        let clauses: Vec<logic::toy::Clause> = 
            vec!["a(X) :- b(X)".parse().unwrap(),
                 logic::toy::Clause{ head: "b(c)".parse().unwrap(), body: vec![] },
                 logic::toy::Clause{ head: "b(d)".parse().unwrap(), body: vec![] }];
        let result = sld(&clauses, &goal, 10);
        assert!(result.is_some());
        let solutions = solutions(&result.unwrap());
        assert_eq!(solutions.len(), 2);
        assert!(solutions.contains(&vec!["a(c)".parse::<logic::toy::Literal>().unwrap()]));
        assert!(solutions.contains(&vec!["a(d)".parse::<logic::toy::Literal>().unwrap()]));
    }

   
}