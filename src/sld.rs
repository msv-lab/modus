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
    fmt::{Debug, Display},
    hash::Hash,
};

use crate::logic::{ModusConstant, ModusVariable};
use crate::unification::RenameWithSubstitution;
use crate::{
    builtin,
    unification::{compose_extend, compose_no_extend, Rename, Substitution},
};
use crate::{
    logic::{self, Signature},
    unification::Substitute,
    wellformed,
};
use logic::{Clause, Literal, Term};

pub trait Variable<C: ModusConstant, V: ModusVariable>: Rename<V> {
    fn aux() -> Self;
}

type RuleId = usize;
type TreeLevel = usize;
pub(crate) type Goal<C: ModusConstant, V: ModusVariable> = Vec<Literal<C, V>>;

/// A clause is either a rule, or a query
#[derive(Copy, Clone, PartialEq, Eq, Hash, Debug)]
pub enum ClauseId {
    Rule(RuleId),
    Query,
    Builtin,
}

/// A literal origin can be uniquely identified through its source clause and its index in the clause body
#[derive(Clone, PartialEq, Debug)]
pub struct LiteralOrigin {
    clause: ClauseId,
    body_index: usize,
}

/// Literal identifier relative to the goal
type LiteralGoalId = usize;

/// literal, tree level at which it was introduced if any, where it came from
#[derive(Clone, PartialEq, Debug)]
struct LiteralWithHistory<C: ModusConstant, V: ModusVariable> {
    literal: Literal<C, V>,
    introduction: TreeLevel,
    origin: LiteralOrigin,
}
type GoalWithHistory<C: ModusConstant, V: ModusVariable> = Vec<LiteralWithHistory<C, V>>;

/// An SLD tree consists of
/// - a goal with its dependencies (at which level and from which part of body each literal was introduced)
/// - a level, which is incremented as tree grows
/// - a mapping from (selected literal in goal, applied rule) to (mgu after rule renaming, rule renaming, resolvent subtree)
#[derive(Clone)]
pub struct Tree<C: ModusConstant, V: ModusVariable> {
    goal: GoalWithHistory<C, V>,
    level: TreeLevel,
    resolvents:
        HashMap<(LiteralGoalId, ClauseId), (Substitution<C, V>, Substitution<C, V>, Tree<C, V>)>,
}

/// A proof tree consist of
/// - a clause
/// - a valuation for this clause
/// - proofs for parts of the clause body
#[derive(Clone, Debug)]
pub struct Proof<C: ModusConstant, V: ModusVariable> {
    pub clause: ClauseId,
    pub valuation: Substitution<C, V>,
    pub children: Vec<Proof<C, V>>,
}

impl<C: ModusConstant, V: ModusVariable> Substitute<C, V> for GoalWithHistory<C, V> {
    type Output = GoalWithHistory<C, V>;
    fn substitute(&self, s: &Substitution<C, V>) -> Self::Output {
        self.iter()
            .map(
                |LiteralWithHistory {
                     literal,
                     introduction,
                     origin,
                 }| LiteralWithHistory {
                    literal: literal.substitute(s),
                    introduction: introduction.clone(),
                    origin: origin.clone(),
                },
            )
            .collect()
    }
}

pub fn sld<C: ModusConstant, V: ModusVariable>(
    rules: &Vec<Clause<C, V>>,
    goal: &Goal<C, V>,
    maxdepth: TreeLevel,
) -> Option<Tree<C, V>> {
    /// select leftmost literal with compatible groundness
    fn select<C: ModusConstant, V: ModusVariable>(
        goal: &GoalWithHistory<C, V>,
        grounded: &HashMap<Signature, Vec<bool>>,
    ) -> Option<(LiteralGoalId, LiteralWithHistory<C, V>)> {
        goal.iter()
            .enumerate()
            .find(|(id, lit)| {
                let literal = &lit.literal;
                if builtin::select_builtin(literal).is_some() {
                    return true;
                }

                // For any user-defined atom, we can get its groundness requirement
                // (computed outside), and if a particular argument can not be
                // ungrounded (grounded[arg_index] == false), variables will not be
                // allowed there.
                let lit_grounded = grounded
                    .get(&literal.signature())
                    .expect("should find literal with matching groundedness signature");
                debug_assert_eq!(lit_grounded.len(), literal.args.len());
                return literal
                    .args
                    .iter()
                    .zip(lit_grounded.iter())
                    .all(|pair| !matches!(pair, (Term::Variable(_), &false)));
            })
            .map(|(a, b)| (a, b.clone()))
    }

    fn resolve<C: ModusConstant, V: ModusVariable>(
        lid: LiteralGoalId,
        rid: ClauseId,
        goal: &GoalWithHistory<C, V>,
        mgu: &Substitution<C, V>,
        rule: &Clause<C, V>,
        level: TreeLevel,
    ) -> GoalWithHistory<C, V> {
        let mut g: GoalWithHistory<C, V> = goal.clone();
        g.remove(lid);
        g.extend(
            rule.body
                .iter()
                .enumerate()
                .map(|(id, l)| {
                    let origin = LiteralOrigin {
                        clause: rid,
                        body_index: id,
                    };
                    LiteralWithHistory {
                        literal: l.clone(),
                        introduction: level,
                        origin,
                    }
                })
                .collect::<GoalWithHistory<C, V>>(),
        );
        g.substitute(mgu)
    }

    fn inner<C: ModusConstant, V: ModusVariable>(
        rules: &Vec<Clause<C, V>>,
        goal: &GoalWithHistory<C, V>,
        maxdepth: TreeLevel,
        level: TreeLevel,
        grounded: &HashMap<Signature, Vec<bool>>,
    ) -> Option<Tree<C, V>> {
        #[cfg(debug_assertions)]
        {
            // FIXME: move this ad-hoc debug code elsewhere
            eprintln!(
                "{}inner(rules, goal=[ {} ], level={}/{})",
                "  ".to_string().repeat(level),
                goal.iter()
                    .map(|g| format!(
                        "{}({})",
                        &g.literal.atom.0,
                        g.literal
                            .args
                            .iter()
                            .map(|x| match x {
                                Term::Constant(x) => x.to_string(),
                                Term::Variable(v) =>
                                    format!("{:?}", v).trim_matches('\"').to_string(),
                                _ => format!("{:?}", x),
                            })
                            .collect::<Vec<String>>()
                            .join(", ")
                    ))
                    .collect::<Vec<String>>()
                    .join(", "),
                level,
                maxdepth
            );
        }
        if goal.is_empty() {
            Some(Tree {
                goal: goal.clone(),
                level,
                resolvents: HashMap::new(),
            })
        } else if level >= maxdepth {
            None
        } else {
            let selected = select(goal, grounded);
            if selected.is_none() {
                return None;
            }
            let (lid, l) = selected.unwrap();

            let builtin_resolves = builtin::select_builtin(&l.literal)
                .and_then(|pred| pred.apply(&l.literal))
                .and_then(|unify_cand| {
                    unify_cand.unify(&l.literal).map(|mgu| {
                        (
                            ClauseId::Builtin,
                            mgu.clone(),
                            Substitution::<C, V>::new(),
                            resolve(
                                lid,
                                ClauseId::Builtin,
                                &goal,
                                &mgu,
                                &Clause {
                                    head: unify_cand,
                                    body: Vec::new(), // TODO: allow builtin rules to return more conditions?
                                },
                                level + 1,
                            ),
                        )
                    })
                })
                .into_iter();
            let user_rules_resolves = rules
                .iter()
                .enumerate()
                .filter(|(_, c)| c.head.signature() == l.literal.signature())
                .map(|(rid, c)| (ClauseId::Rule(rid), c.rename()))
                .filter_map(|(rid, (c, renaming))| {
                    c.head.unify(&l.literal).map(|mgu| {
                        (
                            rid,
                            mgu.clone(),
                            renaming,
                            resolve(lid, rid, &goal, &mgu, &c, level + 1),
                        )
                    })
                });

            let resolvents: HashMap<
                (LiteralGoalId, ClauseId),
                (Substitution<C, V>, Substitution<C, V>, Tree<C, V>),
            > = builtin_resolves
                .chain(user_rules_resolves)
                .filter_map(|(rid, mgu, renaming, resolvent)| {
                    inner(rules, &resolvent, maxdepth, level + 1, grounded)
                        .and_then(|tree| Some(((lid, rid), (mgu, renaming, tree))))
                })
                .collect();
            if resolvents.is_empty() {
                None
            } else {
                Some(Tree {
                    goal: goal.clone(),
                    level,
                    resolvents,
                })
            }
        }
    }

    let grounded = wellformed::check_grounded_variables(rules).unwrap();

    let goal_with_history = goal
        .iter()
        .enumerate()
        .map(|(id, l)| {
            let origin = LiteralOrigin {
                clause: ClauseId::Query,
                body_index: id,
            };
            LiteralWithHistory {
                literal: l.clone(),
                introduction: 0,
                origin,
            }
        })
        .collect();
    inner(rules, &goal_with_history, maxdepth, 0, &grounded)
}

pub fn solutions<C: ModusConstant, V: ModusVariable>(tree: &Tree<C, V>) -> HashSet<Goal<C, V>> {
    fn inner<C: ModusConstant, V: ModusVariable>(tree: &Tree<C, V>) -> Vec<Substitution<C, V>> {
        if tree.goal.is_empty() {
            let s = Substitution::<C, V>::new();
            return vec![s];
        }
        tree.resolvents
            .iter()
            .map(|(_, (mgu, _, subtree))| (mgu, inner(subtree)))
            .map(|(mgu, sub)| {
                sub.iter()
                    .map(|s| compose_extend(mgu, s))
                    .collect::<Vec<Substitution<C, V>>>()
            })
            .flatten()
            .collect()
    }
    inner(tree)
        .iter()
        .map(|s| {
            tree.goal
                .iter()
                .map(
                    |LiteralWithHistory {
                         literal,
                         introduction: _,
                         origin: _,
                     }| literal.substitute(s),
                )
                .collect()
        })
        .collect()
}

#[derive(Clone)]
struct PathNode<C: ModusConstant, V: ModusVariable> {
    resolvent: GoalWithHistory<C, V>,
    applied: ClauseId,
    selected: LiteralGoalId,
    renaming: Substitution<C, V>,
}

// sequence of nodes and global mgu
type Path<C: ModusConstant, V: ModusVariable> = (Vec<PathNode<C, V>>, Substitution<C, V>);

pub fn proofs<C: ModusConstant, V: ModusVariable>(
    tree: &Tree<C, V>,
    rules: &Vec<Clause<C, V>>,
    goal: &Goal<C, V>,
) -> Vec<Proof<C, V>> {
    fn flatten_compose<C: ModusConstant, V: ModusVariable>(
        lid: &LiteralGoalId,
        cid: &ClauseId,
        mgu: &Substitution<C, V>,
        renaming: &Substitution<C, V>,
        tree: &Tree<C, V>,
    ) -> Vec<Path<C, V>> {
        if tree.goal.is_empty() {
            return vec![(
                vec![PathNode {
                    resolvent: tree.goal.clone(),
                    applied: cid.clone(),
                    selected: lid.clone(),
                    renaming: renaming.clone(),
                }],
                mgu.clone(),
            )];
        }
        tree.resolvents
            .iter()
            .map(|((sub_lid, sub_cid), (sub_mgu, sub_renaming, sub_tree))| {
                flatten_compose(sub_lid, sub_cid, sub_mgu, sub_renaming, sub_tree)
                    .iter()
                    .map(|(sub_path, sub_val)| {
                        let mut nodes = vec![PathNode {
                            resolvent: tree.goal.clone(),
                            applied: cid.clone(),
                            selected: lid.clone(),
                            renaming: renaming.clone(),
                        }];
                        let val = compose_extend(mgu, sub_val);
                        nodes.extend(sub_path.clone());
                        (nodes, val)
                    })
                    .collect::<Vec<Path<C, V>>>()
            })
            .flatten()
            .collect()
    }
    // reconstruct proof for a given tree level
    fn proof_for_level<C: ModusConstant, V: ModusVariable>(
        path: &Vec<PathNode<C, V>>,
        mgu: &Substitution<C, V>,
        rules: &Vec<Clause<C, V>>,
        level: TreeLevel,
    ) -> Proof<C, V> {
        let mut sublevels_map: HashMap<usize, TreeLevel> = HashMap::new();
        for l in 0..path.len() {
            if !path[l].resolvent.is_empty() {
                let resolved_child = path[l].resolvent[path[l + 1].selected].clone();
                if resolved_child.introduction == level {
                    sublevels_map.insert(resolved_child.origin.body_index, l + 1);
                }
            }
        }
        let children_length = sublevels_map.len();
        match path[level].applied {
            ClauseId::Query => assert_eq!(children_length, path[0].resolvent.len()),
            ClauseId::Rule(rid) => assert_eq!(children_length, rules[rid].body.len()),
            ClauseId::Builtin => assert_eq!(children_length, 0),
        };

        let mut sublevels = Vec::<TreeLevel>::with_capacity(sublevels_map.len());
        for k in sublevels_map.keys() {
            assert!(*k < children_length);
        }
        for i in 0..children_length {
            sublevels.push(*sublevels_map.get(&i).unwrap());
        }
        Proof {
            clause: path[level].applied.clone(),
            valuation: compose_no_extend(&path[level].renaming, mgu),
            children: sublevels
                .iter()
                .map(|l| proof_for_level(path, mgu, rules, *l))
                .collect(),
        }
    }
    // assume lid of root is 0, as if it came from a clause "true :- goal" for query "true", but this is not used anyway
    let goal_vars = goal
        .iter()
        .map(|l| l.variables())
        .reduce(|mut l, r| {
            l.extend(r);
            l
        })
        .unwrap_or_default();
    let goal_id_reaming: Substitution<C, V> = goal_vars
        .iter()
        .map(|v| (v.clone(), Term::Variable(v.clone())))
        .collect();
    let paths = flatten_compose(
        &0,
        &ClauseId::Query,
        &Substitution::new(),
        &goal_id_reaming,
        tree,
    );
    let all_proofs: Vec<Proof<C, V>> = paths
        .iter()
        .map(|(path, mgu)| proof_for_level(path, mgu, rules, 0))
        .collect();

    //TODO: instead, I should find optimal proofs
    let mut computed: HashSet<Goal<C, V>> = HashSet::new();
    let mut proofs: Vec<Proof<C, V>> = Vec::new();
    for p in all_proofs {
        let solution: Goal<C, V> = goal.substitute(&p.valuation);
        if !computed.contains(&solution) {
            computed.insert(solution.clone());
            proofs.push(p)
        }
    }
    proofs
}

#[cfg(test)]
mod tests {
    use std::{
        collections::HashMap,
        sync::atomic::{AtomicU32, Ordering},
    };

    use super::*;

    static AVAILABLE_INDEX: AtomicU32 = AtomicU32::new(0);

    /// Assume that underscore is not used in normal variables
    impl Rename<logic::toy::Variable> for logic::toy::Variable {
        fn rename(&self) -> logic::toy::Variable {
            let index = AVAILABLE_INDEX.fetch_add(1, Ordering::SeqCst);
            let prefix = self.split('_').next().unwrap();
            let renamed = format!("{}_{}", prefix, index);
            let mut s = HashMap::<
                logic::toy::Variable,
                logic::Term<logic::Predicate, logic::toy::Variable>,
            >::new();
            s.insert(self.clone(), logic::Term::Variable(renamed.clone()));
            renamed
        }
    }

    impl Variable<logic::Predicate, logic::toy::Variable> for logic::toy::Variable {
        fn aux() -> logic::toy::Variable {
            let index = AVAILABLE_INDEX.fetch_add(1, Ordering::SeqCst);
            format!("Aux{}", index)
        }
    }

    #[test]
    fn simple_solving() {
        let goal: Goal<logic::Predicate, logic::toy::Variable> = vec!["a(X)".parse().unwrap()];
        let clauses: Vec<logic::toy::Clause> = vec![
            "a(X) :- b(X).".parse().unwrap(),
            logic::toy::Clause {
                head: "b(c)".parse().unwrap(),
                body: vec![],
            },
            logic::toy::Clause {
                head: "b(d)".parse().unwrap(),
                body: vec![],
            },
        ];
        let result = sld(&clauses, &goal, 10);
        assert!(result.is_some());
        let solutions = solutions(&result.unwrap());
        assert_eq!(solutions.len(), 2);
        assert!(solutions.contains(&vec!["a(c)".parse::<logic::toy::Literal>().unwrap()]));
        assert!(solutions.contains(&vec!["a(d)".parse::<logic::toy::Literal>().unwrap()]));
    }

    #[test]
    fn simple_nongrounded() {
        let goal: Goal<logic::Predicate, logic::toy::Variable> = vec!["a(b)".parse().unwrap()];
        let clauses: Vec<logic::toy::Clause> = vec![logic::toy::Clause {
            head: "a(X)".parse().unwrap(),
            body: vec![],
        }];
        let result = sld(&clauses, &goal, 10);
        assert!(result.is_some());
        let solutions = solutions(&result.unwrap());
        assert_eq!(solutions.len(), 1);
        assert!(solutions.contains(&vec!["a(b)".parse::<logic::toy::Literal>().unwrap()]));
    }

    #[test]
    fn simple_nongrounded_invalid() {
        let goal: Goal<logic::Predicate, logic::toy::Variable> = vec!["a(X)".parse().unwrap()];
        let clauses: Vec<logic::toy::Clause> = vec![logic::toy::Clause {
            head: "a(X)".parse().unwrap(),
            body: vec![],
        }];
        let result = sld(&clauses, &goal, 10);
        assert!(result.is_none());
    }

    #[test]
    fn complex_goal() {
        let goal: Goal<logic::Predicate, logic::toy::Variable> =
            vec!["a(X)".parse().unwrap(), "b(X)".parse().unwrap()];
        let clauses: Vec<logic::toy::Clause> = vec![
            logic::toy::Clause {
                head: "a(t)".parse().unwrap(),
                body: vec![],
            },
            logic::toy::Clause {
                head: "a(f)".parse().unwrap(),
                body: vec![],
            },
            logic::toy::Clause {
                head: "b(g)".parse().unwrap(),
                body: vec![],
            },
            logic::toy::Clause {
                head: "b(t)".parse().unwrap(),
                body: vec![],
            },
        ];
        let result = sld(&clauses, &goal, 10);
        assert!(result.is_some());
        let solutions = solutions(&result.unwrap());
        assert_eq!(solutions.len(), 1);
        assert!(solutions.contains(&vec!["a(t)".parse().unwrap(), "b(t)".parse().unwrap()]));
    }

    #[test]
    fn solving_with_binary_relations() {
        let goal: Goal<logic::Predicate, logic::toy::Variable> = vec!["a(X)".parse().unwrap()];
        let clauses: Vec<logic::toy::Clause> = vec![
            "a(X) :- b(X, Y), c(Y).".parse().unwrap(),
            logic::toy::Clause {
                head: "b(t, f)".parse().unwrap(),
                body: vec![],
            },
            logic::toy::Clause {
                head: "b(f, t)".parse().unwrap(),
                body: vec![],
            },
            logic::toy::Clause {
                head: "b(g, t)".parse().unwrap(),
                body: vec![],
            },
            logic::toy::Clause {
                head: "c(t)".parse().unwrap(),
                body: vec![],
            },
        ];
        let result = sld(&clauses, &goal, 10);
        assert!(result.is_some());
        let solutions = solutions(&result.unwrap());
        assert_eq!(solutions.len(), 2);
        assert!(solutions.contains(&vec!["a(f)".parse().unwrap()]));
        assert!(solutions.contains(&vec!["a(g)".parse().unwrap()]));
    }

    #[test]
    fn simple_recursion() {
        let goal: Goal<logic::Predicate, logic::toy::Variable> =
            vec!["reach(a, X)".parse().unwrap()];
        let clauses: Vec<logic::toy::Clause> = vec![
            "reach(X, Y) :- reach(X, Z), arc(Z, Y).".parse().unwrap(),
            "reach(X, Y) :- arc(X, Y).".parse().unwrap(),
            logic::toy::Clause {
                head: "arc(a, b)".parse().unwrap(),
                body: vec![],
            },
            logic::toy::Clause {
                head: "arc(b, c)".parse().unwrap(),
                body: vec![],
            },
            logic::toy::Clause {
                head: "arc(c, d)".parse().unwrap(),
                body: vec![],
            },
            logic::toy::Clause {
                head: "arc(d, e)".parse().unwrap(),
                body: vec![],
            },
            logic::toy::Clause {
                head: "arc(f, e)".parse().unwrap(),
                body: vec![],
            },
            logic::toy::Clause {
                head: "arc(g, f)".parse().unwrap(),
                body: vec![],
            },
            logic::toy::Clause {
                head: "arc(g, a)".parse().unwrap(),
                body: vec![],
            },
        ];
        let result = sld(&clauses, &goal, 15);
        assert!(result.is_some());
        let solutions = solutions(&result.unwrap());
        assert_eq!(solutions.len(), 4);
        assert!(solutions.contains(&vec!["reach(a, b)".parse().unwrap()]));
        assert!(solutions.contains(&vec!["reach(a, c)".parse().unwrap()]));
        assert!(solutions.contains(&vec!["reach(a, d)".parse().unwrap()]));
        assert!(solutions.contains(&vec!["reach(a, e)".parse().unwrap()]));
    }

    #[test]
    fn string_concat() {
        let goal: Goal<logic::Predicate, logic::toy::Variable> =
            vec!["string_concat(hello, world, X)".parse().unwrap()];
        let clauses: Vec<logic::toy::Clause> = vec![];
        let result = sld(&clauses, &goal, 10);
        assert!(result.is_some());
        let solutions = solutions(&result.unwrap());
        assert_eq!(solutions.len(), 1);
        assert!(
            solutions.contains(&vec!["string_concat(hello, world, helloworld)"
                .parse()
                .unwrap()])
        );
    }

    #[test]
    fn string_concat_complex() {
        // TODO: figure out how to test empty string
        let good = ["aaabbb", "aabb"];
        let bad = ["aab", "a", "bb"];
        for (s, is_good) in good
            .iter()
            .map(|x| (*x, true))
            .chain(bad.iter().map(|x| (*x, false)))
        {
            let goal: Goal<logic::Predicate, logic::toy::Variable> =
                vec![format!("a({})", s).parse().unwrap()];
            let clauses: Vec<logic::toy::Clause> = vec![
                logic::toy::Clause {
                    head: "a(ab)".parse().unwrap(),
                    body: vec![],
                },
                "a(S) :- string_concat(a, X, S), string_concat(Y, b, X), a(Y)"
                    .parse()
                    .unwrap(),
            ];
            let result = sld(&clauses, &goal, 50);
            if is_good {
                assert!(result.is_some());
                let solutions = solutions(&result.unwrap());
                assert_eq!(solutions.len(), 1);
                assert!(solutions.contains(&vec![format!("a({})", s).parse().unwrap()]));
            } else {
                assert!(result.is_none());
            }
        }
    }
}
