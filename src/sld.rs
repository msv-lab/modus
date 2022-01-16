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
    fmt::Debug,
    hash::Hash,
};

use crate::{
    builtin,
    unification::{compose_extend, compose_no_extend, Rename, Substitution},
};
use crate::{builtin::SelectBuiltinResult, unification::RenameWithSubstitution};
use crate::{
    logic::{self, Signature},
    unification::Substitute,
    wellformed,
};
use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};
use logic::{Clause, IRTerm, Literal};

pub trait Auxiliary: Rename<Self> + Sized {
    fn aux() -> Self;
}

type RuleId = usize;
type GoalId = usize;
type TreeLevel = usize;
pub(crate) type Goal<T = IRTerm> = Vec<Literal<T>>;

/// A clause is either a rule, or a query
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum ClauseId {
    Rule(RuleId),
    Query,
    Builtin(Literal<IRTerm>),
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
struct LiteralWithHistory {
    literal: Literal<IRTerm>,
    introduction: TreeLevel,
    origin: LiteralOrigin,
}
type GoalWithHistory = Vec<LiteralWithHistory>;

/// An SLD tree consists of
/// - a goal with its dependencies (at which level and from which part of body each literal was introduced)
/// - a level, which is incremented as tree grows
/// - a mapping from (selected literal in goal, applied rule) to (mgu after rule renaming, rule renaming, resolvent subtree)
///
/// TODO: there is likely some convenient way to extend this type to hold errors as well, so
/// we can preserve the tree structure of errors instead of flattening into a list
/// like we currently do.
#[derive(Clone, PartialEq, Debug)]
pub struct Tree {
    goal: GoalWithHistory,
    level: TreeLevel,
    resolvents: HashMap<(LiteralGoalId, ClauseId), (Substitution, Substitution, Tree)>,
}

impl Tree {
    /// Converts this tree to a directed graph.
    pub fn to_graph(&self, rules: &[Clause]) -> Graph {
        fn convert(
            t: &Tree,
            rules: &[Clause],
            nodes: &mut Vec<String>,
            edges: &mut Vec<(usize, usize, String)>,
        ) {
            let curr_label = t
                .goal
                .iter()
                .map(|l| l.literal.to_string())
                .collect::<Vec<_>>()
                .join(",\n");
            nodes.push(curr_label);
            let curr_index = nodes.len() - 1;

            if !t.goal.is_empty() && t.resolvents.is_empty() {
                // REVIEW: is this needed? Or will the user understand that only if a leaf node has no goals
                // it is a success?
                nodes.push("FAIL".to_string());
                edges.push((curr_index, curr_index + 1, "".to_string()));
            } else {
                let mut resolvent_pairs = t.resolvents.iter().collect::<Vec<_>>();
                resolvent_pairs.sort_by_key(|(k, _)| k.0); // for some consistency

                for (k, v) in resolvent_pairs {
                    let new_index = nodes.len();
                    convert(&v.2, rules, nodes, edges);

                    let edge_label = match &k.1 {
                        ClauseId::Rule(rid) => rules[*rid].head.to_string(),
                        ClauseId::Query => "query".to_string(),
                        ClauseId::Builtin(lit) => lit.to_string(),
                    };
                    edges.push((curr_index, new_index, edge_label));
                }
            }
        }

        let name = "SLD_Tree";
        let mut nodes = Vec::new();
        let mut edges = Vec::new();
        convert(self, rules, &mut nodes, &mut edges);
        Graph { name, nodes, edges }
    }

    /// Returns a string explaining the SLD tree, using indentation, etc.
    pub fn explain(&self, rules: &[Clause]) -> String {
        fn dfs(t: &Tree, rules: &[Clause], lines: &mut Vec<String>) {
            let curr = if t.goal.is_empty() {
                format!("{}| Success", " ".repeat(t.level))
            } else {
                format!(
                    "{}| {} We require {}",
                    " ".repeat(t.level),
                    t.level,
                    t.goal
                        .iter()
                        .map(|l| l.literal.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            };
            lines.push(curr);

            let mut resolvent_pairs = t.resolvents.iter().collect::<Vec<_>>();
            resolvent_pairs.sort_by_key(|(k, _)| k.0);

            for (k, v) in &resolvent_pairs {
                let (goal_id, cid) = k;

                let substitution_map: HashMap<_, _> =
                    v.0.iter()
                        .map(|(t1, t2)| (t1.get_original().clone(), t2.clone()))
                        .collect();
                // let variable_choice = format!(
                //     "{}| Taking {}",
                //     " ".repeat(t.level),
                //     substitution_map.iter().map(|(prev, new_val)| prev.to_string() + " = " + &new_val.to_string()).collect::<Vec<_>>().join(", ")
                // );
                // lines.push(variable_choice);

                let chosen_rule_body = match cid {
                    ClauseId::Rule(rid) => rules[*rid]
                        .substitute(&substitution_map)
                        .body
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                    ClauseId::Query => unimplemented!(),
                    ClauseId::Builtin(lit) => lit.substitute(&v.0).to_string(),
                };
                let curr_attempt = format!(
                    "{}| {} requires {}",
                    " ".repeat(t.level),
                    t.goal[*goal_id].literal,
                    if chosen_rule_body.is_empty() {
                        "nothing, it's a fact.".to_owned()
                    } else {
                        chosen_rule_body
                    }
                );
                lines.push(curr_attempt);

                dfs(&v.2, rules, lines);
            }

            if !t.goal.is_empty() && resolvent_pairs.is_empty() {
                lines.push(format!("{}| Fail", " ".repeat(t.level)));
            }
        }

        let mut lines: Vec<String> = Vec::new();
        dfs(self, rules, &mut lines);
        lines.join("\n")
    }
}

type Nd<'a> = (usize, &'a str);
type Ed<'a> = (Nd<'a>, Nd<'a>, &'a str);

pub struct Graph {
    name: &'static str,
    nodes: Vec<String>,
    edges: Vec<(usize, usize, String)>, // labelled edges
}

impl<'a> dot::Labeller<'a, Nd<'a>, Ed<'a>> for Graph {
    fn graph_id(&'a self) -> dot::Id<'a> {
        dot::Id::new(self.name).unwrap()
    }

    fn node_id(&'a self, n: &Nd) -> dot::Id<'a> {
        dot::Id::new(format!("N{}", n.0)).unwrap()
    }

    fn node_label(&'a self, n: &Nd<'a>) -> dot::LabelText<'a> {
        dot::LabelText::LabelStr(n.1.into())
    }

    fn edge_label(&'a self, e: &Ed<'a>) -> dot::LabelText<'a> {
        dot::LabelText::LabelStr(e.2.into())
    }
}

impl<'a> dot::GraphWalk<'a, Nd<'a>, Ed<'a>> for Graph {
    fn nodes(&'a self) -> dot::Nodes<'a, Nd<'a>> {
        self.nodes.iter().map(|n| &n[..]).enumerate().collect()
    }

    fn edges(&'a self) -> dot::Edges<'a, Ed> {
        self.edges
            .iter()
            .map(|(i, j, label)| {
                (
                    (*i, &self.nodes[*i][..]),
                    (*j, &self.nodes[*j][..]),
                    &label[..],
                )
            })
            .collect()
    }

    fn source(&'a self, edge: &Ed<'a>) -> Nd {
        edge.0
    }

    fn target(&'a self, edge: &Ed<'a>) -> Nd {
        edge.1
    }
}

/// A proof tree consist of
/// - a clause
/// - a valuation for this clause
/// - proofs for parts of the clause body
#[derive(Clone, Debug, PartialEq)]
pub struct Proof {
    pub clause: ClauseId,
    pub valuation: Substitution,
    pub children: Vec<Proof>,
}

impl Proof {
    /// Returns the height of this proof tree, where a leaf node has height 0.
    fn height(&self) -> usize {
        self.children.iter().map(|child| child.height() + 1).max().unwrap_or(0)
    }
}

impl PartialOrd for Proof {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.height().partial_cmp(&other.height())
    }
}

impl Substitute<IRTerm> for GoalWithHistory {
    type Output = GoalWithHistory;
    fn substitute(&self, s: &Substitution<IRTerm>) -> Self::Output {
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

#[derive(Clone, PartialEq, Debug)]
pub enum ResolutionError {
    /// Contains the literal with the unknown predicate name.
    UnknownPredicate(Literal),
    /// Contains the relevant goals.
    InsufficientGroundness(Vec<Literal>),
    /// Contains the goals when the max depth was exceeded.
    MaximumDepthExceeded(Vec<Literal>, usize),
    /// Contains the relevant literal (builtin call), and the name of the selected builtin.
    BuiltinFailure(Literal, &'static str),
    /// Contains the literal that didn't match with any rule head.
    InsufficientRules(Literal),
    /// Contains the set of inconsistent signatures.
    InconsistentGroundnessSignature(HashSet<Signature>),
}

impl ResolutionError {
    pub fn get_diagnostic(self) -> Diagnostic<()> {
        fn get_position_labels(literals: &[Literal]) -> Vec<Label<()>> {
            literals
                .iter()
                .filter_map(|lit| lit.position.as_ref())
                .map(|pos| Label::primary((), pos.offset..(pos.offset + pos.length)))
                .collect()
        }

        // Displays literals that don't have a span in the notes of the diagnostic.
        fn get_notes(literals: &[Literal]) -> Vec<String> {
            literals
                .iter()
                .filter_map(|lit| {
                    if lit.position.is_none() {
                        Some(lit.to_string())
                    } else {
                        None
                    }
                })
                .collect()
        }

        let (message, labels, notes, severity) = match self {
            ResolutionError::UnknownPredicate(literal) => (
                if literal.predicate.is_operator() {
                    format!("unknown operator - {}", literal.predicate)
                } else {
                    format!("unknown predicate - {}", literal.predicate)
                },
                get_position_labels(&[literal.clone()]),
                get_notes(&[literal]),
                Severity::Error,
            ),
            ResolutionError::InsufficientGroundness(literals) => (
                format!("insufficient groundess for {} goal(s)", literals.len()),
                get_position_labels(&literals),
                get_notes(&literals),
                Severity::Warning,
            ),
            ResolutionError::MaximumDepthExceeded(literals, max_depth) => (
                format!("exceeded maximum depth of {}", max_depth),
                get_position_labels(&literals),
                get_notes(&literals),
                Severity::Warning,
            ),
            ResolutionError::BuiltinFailure(literal, builtin_name) => (
                format!("builtin {} failed to apply or unify", builtin_name),
                get_position_labels(&[literal.clone()]),
                get_notes(&[literal]),
                Severity::Warning,
            ),
            ResolutionError::InsufficientRules(literal) => (
                format!("could not find a rule to resolve with literal {}", literal),
                get_position_labels(&[literal.clone()]),
                get_notes(&[literal]),
                Severity::Warning,
            ),
            ResolutionError::InconsistentGroundnessSignature(signatures) => (
                // TODO: capture the inconsistent clauses
                format!(
                    "{} clause(s) have inconsistent signatures",
                    signatures.len()
                ),
                Vec::new(),
                Vec::new(),
                Severity::Error,
            ),
        };

        Diagnostic::new(severity)
            .with_message(message)
            .with_labels(labels)
            .with_notes(notes)
    }
}

/// Result of building the SLD tree.
///
/// Uses a custom result type in resolution since we often have some information about
/// either state, an 'Ok' and 'Err' state is too disjoint for our purposes.
pub struct SLDResult {
    /// The subset of the full SLD tree that leads to paths with empty goals (i.e. successful resolution).
    pub success_tree: Option<Tree>,
    pub full_tree: Tree,
    pub errors: Vec<ResolutionError>,
}

impl From<SLDResult> for Result<Tree, Vec<Diagnostic<()>>> {
    fn from(sld_result: SLDResult) -> Self {
        if let Some(t) = sld_result.success_tree {
            Ok(t)
        } else {
            Err(sld_result
                .errors
                .into_iter()
                .map(ResolutionError::get_diagnostic)
                .collect::<Vec<_>>())
        }
    }
}

/// Returns both the SLD tree of only valid paths and the full tree.
pub fn sld(rules: &Vec<Clause<IRTerm>>, goal: &Goal, maxdepth: TreeLevel) -> SLDResult {
    /// select leftmost literal with compatible groundness
    fn select(
        goal: &GoalWithHistory,
        grounded: &HashMap<Signature, Vec<bool>>,
    ) -> Result<(LiteralGoalId, LiteralWithHistory), ResolutionError> {
        for (id, lit) in goal.iter().enumerate() {
            let literal = &lit.literal;
            let select_builtin_res = builtin::select_builtin(literal);
            if select_builtin_res.0.is_match() {
                return Ok((id, lit.clone()));
            }

            // For any user-defined atom, we can get its groundness requirement
            // (computed outside), and if a particular argument can not be
            // ungrounded (grounded[arg_index] == false), variables will not be
            // allowed there.
            let lit_grounded = grounded.get(&literal.signature());
            if let Some(lit_grounded) = lit_grounded {
                debug_assert_eq!(lit_grounded.len(), literal.args.len());
                if literal
                    .args
                    .iter()
                    .zip(lit_grounded.iter())
                    .all(|pair| matches!(pair, (_, true) | (IRTerm::Constant(_), false)))
                {
                    return Ok((id, lit.clone()));
                } else {
                    continue;
                }
            } else if select_builtin_res.0 == SelectBuiltinResult::GroundnessMismatch {
                continue;
            }

            return Err(ResolutionError::UnknownPredicate(literal.clone()));
        }

        Err(ResolutionError::InsufficientGroundness(
            goal.iter().map(|lit| lit.literal.clone()).collect(),
        ))
    }

    fn resolve(
        lid: LiteralGoalId,
        rid: ClauseId,
        goal: &GoalWithHistory,
        mgu: &Substitution,
        rule: &Clause,
        level: TreeLevel,
    ) -> GoalWithHistory {
        let mut g: GoalWithHistory = goal.clone();
        g.remove(lid);
        g.extend(
            rule.body
                .iter()
                .enumerate()
                .map(|(id, l)| {
                    let origin = LiteralOrigin {
                        clause: rid.clone(),
                        body_index: id,
                    };
                    LiteralWithHistory {
                        literal: l.clone(),
                        introduction: level,
                        origin,
                    }
                })
                .collect::<GoalWithHistory>(),
        );
        g.substitute(mgu)
    }

    fn inner(
        rules: &Vec<Clause<IRTerm>>,
        goal: &GoalWithHistory,
        maxdepth: TreeLevel,
        level: TreeLevel,
        grounded: &HashMap<Signature, Vec<bool>>,
    ) -> SLDResult {
        if goal.is_empty() {
            let t = Tree {
                goal: goal.clone(),
                level,
                resolvents: HashMap::new(),
            };
            SLDResult {
                success_tree: Some(t.clone()),
                full_tree: t,
                errors: Vec::new(),
            }
        } else if level >= maxdepth {
            let t = Tree {
                goal: goal.clone(),
                level,
                resolvents: HashMap::default(),
            };
            let errors = vec![ResolutionError::MaximumDepthExceeded(
                goal.iter()
                    .map(|lit_hist| lit_hist.literal.clone())
                    .collect(),
                maxdepth,
            )];
            SLDResult {
                success_tree: None,
                full_tree: t,
                errors,
            }
        } else {
            let selection_res = select(goal, grounded);
            if let Err(e) = selection_res {
                let t = Tree {
                    goal: goal.clone(),
                    level,
                    resolvents: HashMap::default(),
                };
                return SLDResult {
                    success_tree: None,
                    full_tree: t,
                    errors: vec![e],
                };
            }
            let (lid, l) = selection_res.unwrap();

            let mut errs: Vec<ResolutionError> = Vec::new();

            let selected_builtin = builtin::select_builtin(&l.literal);
            let builtin_resolves = match selected_builtin {
                (SelectBuiltinResult::Match, lit) => lit,
                _ => None,
            }
            .and_then(|pred| pred.apply(&l.literal))
            .and_then(|unify_cand| {
                unify_cand.unify(&l.literal).map(|mgu| {
                    (
                        ClauseId::Builtin(unify_cand.clone()),
                        mgu.clone(),
                        Substitution::<IRTerm>::new(),
                        resolve(
                            lid,
                            ClauseId::Builtin(unify_cand.clone()),
                            goal,
                            &mgu,
                            &Clause {
                                head: unify_cand,
                                body: Vec::new(), // TODO: allow builtin rules to return more conditions?
                            },
                            level + 1,
                        ),
                    )
                })
            });
            if selected_builtin.0.is_match() && builtin_resolves.is_none() {
                errs.push(ResolutionError::BuiltinFailure(
                    l.literal.clone(),
                    selected_builtin
                        .1
                        .expect("match should provide builtin")
                        .name(),
                ))
            }

            let user_rules_resolves = rules
                .iter()
                .enumerate()
                .filter(|(_, c)| c.head.signature() == l.literal.signature())
                .map(|(rid, c)| (ClauseId::Rule(rid), c.rename_with_sub()))
                .filter_map(|(rid, (c, renaming))| {
                    c.head.unify(&l.literal).map(|mgu| {
                        (
                            rid.clone(),
                            mgu.clone(),
                            renaming,
                            resolve(lid, rid, goal, &mgu, &c, level + 1),
                        )
                    })
                })
                .collect::<Vec<_>>();
            if !selected_builtin.0.is_match() && user_rules_resolves.is_empty() {
                errs.push(ResolutionError::InsufficientRules(l.literal.clone()));
            }

            let mut success_resolvents: HashMap<
                (LiteralGoalId, ClauseId),
                (Substitution, Substitution, Tree),
            > = HashMap::new();
            let mut all_resolvents: HashMap<
                (LiteralGoalId, ClauseId),
                (Substitution, Substitution, Tree),
            > = HashMap::new();
            for (rid, mgu, renaming, resolvent) in
                builtin_resolves.into_iter().chain(user_rules_resolves)
            {
                let SLDResult {
                    success_tree,
                    full_tree,
                    mut errors,
                } = inner(rules, &resolvent, maxdepth, level + 1, grounded);
                all_resolvents.insert(
                    (lid, rid.clone()),
                    (mgu.clone(), renaming.clone(), full_tree),
                );
                errs.append(&mut errors);
                if let Some(success_tree) = success_tree {
                    success_resolvents.insert((lid, rid), (mgu, renaming, success_tree));
                }
            }

            // TODO: could also check the severity of errors and terminate early?
            // Although, semantic analysis would likely be better for those kinds of
            // issues anyway.
            let success_tree = if success_resolvents.is_empty() {
                None
            } else {
                Some(Tree {
                    goal: goal.clone(),
                    level,
                    resolvents: success_resolvents,
                })
            };
            SLDResult {
                success_tree,
                full_tree: Tree {
                    goal: goal.clone(),
                    level,
                    resolvents: all_resolvents,
                },
                errors: errs,
            }
        }
    }

    let grounded_result = wellformed::check_grounded_variables(rules);
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
    match grounded_result {
        Ok(grounded) => inner(rules, &goal_with_history, maxdepth, 0, &grounded),
        Err(e) => SLDResult {
            success_tree: None,
            full_tree: Tree {
                goal: goal_with_history,
                level: 0,
                resolvents: HashMap::default(),
            },
            errors: vec![ResolutionError::InconsistentGroundnessSignature(e)],
        },
    }
}

pub fn solutions(tree: &Tree) -> HashSet<Goal> {
    fn inner(tree: &Tree) -> Vec<Substitution> {
        if tree.goal.is_empty() {
            let s = Substitution::new();
            return vec![s];
        }
        tree.resolvents
            .iter()
            .map(|(_, (mgu, _, subtree))| (mgu, inner(subtree)))
            .map(|(mgu, sub)| {
                sub.iter()
                    .map(|s| compose_extend(mgu, s))
                    .collect::<Vec<Substitution<IRTerm>>>()
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
struct PathNode {
    resolvent: GoalWithHistory,
    applied: ClauseId,
    selected: LiteralGoalId,
    renaming: Substitution,
}

// sequence of nodes and global mgu
type Path = (Vec<PathNode>, Substitution);

pub fn proofs(tree: &Tree, rules: &Vec<Clause>, goal: &Goal) -> Vec<Proof> {
    fn flatten_compose(
        lid: &LiteralGoalId,
        cid: &ClauseId,
        mgu: &Substitution,
        renaming: &Substitution,
        tree: &Tree,
    ) -> Vec<Path> {
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
                    .collect::<Vec<Path>>()
            })
            .flatten()
            .collect()
    }
    // reconstruct proof for a given tree level
    fn proof_for_level(
        path: &Vec<PathNode>,
        mgu: &Substitution,
        rules: &Vec<Clause>,
        level: TreeLevel,
    ) -> Proof {
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
            ClauseId::Builtin(_) => assert_eq!(children_length, 0),
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
    let goal_id_renaming: Substitution<IRTerm> =
        goal_vars.iter().map(|v| (v.clone(), v.clone())).collect();
    let paths = flatten_compose(
        &0,
        &ClauseId::Query,
        &Substitution::new(),
        &goal_id_renaming,
        tree,
    );
    let all_proofs: Vec<Proof> = paths
        .iter()
        .map(|(path, mgu)| proof_for_level(path, mgu, rules, 0))
        .collect();

    let mut solution_to_proof_tree: HashMap<Goal, Proof> = HashMap::new();
    for p in all_proofs {
        let solution: Goal = goal.substitute(&p.valuation);
        // keeps the minimal proof tree
        if let Some(existing_proof) = solution_to_proof_tree.get(&solution) {
            if existing_proof <= &p {
                continue;
            }
        }
        solution_to_proof_tree.insert(solution, p);
    }
    solution_to_proof_tree.into_values().collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use serial_test::serial;

    // These tests all need to be marked as serial even though they don't
    // interfere with each other, because they can potentially modify
    // logic::AVAILABLE_VARIABLE_INDEX, but tests in the translate module depend
    // on the variable not being changed by something else during execution.

    fn contains_ignoring_position(it: &HashSet<Vec<Literal>>, lits: &Vec<Literal>) -> bool {
        it.iter().any(|curr_lits| {
            curr_lits.len() == lits.len()
                && curr_lits
                    .iter()
                    .zip(lits)
                    .all(|(l1, l2)| l1.eq_ignoring_position(&l2))
        })
    }

    #[test]
    #[serial]
    fn simple_solving() {
        let goal: Goal<logic::IRTerm> = vec!["a(X)".parse().unwrap()];
        let clauses: Vec<logic::Clause> = vec![
            "a(X) :- b(X).".parse().unwrap(),
            logic::Clause {
                head: "b(\"c\")".parse().unwrap(),
                body: vec![],
            },
            logic::Clause {
                head: "b(\"d\")".parse().unwrap(),
                body: vec![],
            },
        ];
        let tree = sld(&clauses, &goal, 10).success_tree.unwrap();
        let solutions = solutions(&tree);
        assert_eq!(solutions.len(), 2);

        assert!(contains_ignoring_position(
            &solutions,
            &vec!["a(\"c\")".parse::<logic::Literal>().unwrap()]
        ));
        assert!(contains_ignoring_position(
            &solutions,
            &vec!["a(\"d\")".parse::<logic::Literal>().unwrap()]
        ));
    }

    #[test]
    #[serial]
    fn simple_nongrounded() {
        let goal: Goal<logic::IRTerm> = vec!["a(\"b\")".parse().unwrap()];
        let clauses: Vec<logic::Clause> = vec![logic::Clause {
            head: "a(X)".parse().unwrap(),
            body: vec![],
        }];
        let tree = sld(&clauses, &goal, 10).success_tree.unwrap();
        let solutions = solutions(&tree);
        assert_eq!(solutions.len(), 1);
        assert!(contains_ignoring_position(
            &solutions,
            &vec!["a(\"b\")".parse::<logic::Literal>().unwrap()]
        ));
    }

    #[test]
    #[serial]
    fn simple_nongrounded_invalid() {
        let goal: Goal<logic::IRTerm> = vec!["a(X)".parse().unwrap()];
        let clauses: Vec<logic::Clause> = vec![logic::Clause {
            head: "a(X)".parse().unwrap(),
            body: vec![],
        }];
        let result = sld(&clauses, &goal, 10);
        assert_eq!(
            vec![ResolutionError::InsufficientGroundness(goal)],
            result.errors
        )
    }

    #[test]
    #[serial]
    fn complex_goal() {
        let goal: Goal<logic::IRTerm> = vec!["a(X)".parse().unwrap(), "b(X)".parse().unwrap()];
        let clauses: Vec<logic::Clause> = vec![
            logic::Clause {
                head: "a(\"t\")".parse().unwrap(),
                body: vec![],
            },
            logic::Clause {
                head: "a(\"f\")".parse().unwrap(),
                body: vec![],
            },
            logic::Clause {
                head: "b(\"g\")".parse().unwrap(),
                body: vec![],
            },
            logic::Clause {
                head: "b(\"t\")".parse().unwrap(),
                body: vec![],
            },
        ];
        let tree = sld(&clauses, &goal, 10).success_tree.unwrap();
        let solutions = solutions(&tree);
        assert_eq!(solutions.len(), 1);
        assert!(contains_ignoring_position(
            &solutions,
            &vec!["a(\"t\")".parse().unwrap(), "b(\"t\")".parse().unwrap()]
        ));
    }

    #[test]
    #[serial]
    fn solving_with_binary_relations() {
        let goal: Goal<logic::IRTerm> = vec!["a(X)".parse().unwrap()];
        let clauses: Vec<logic::Clause> = vec![
            "a(X) :- b(X, Y), c(Y).".parse().unwrap(),
            logic::Clause {
                head: "b(\"t\", \"f\")".parse().unwrap(),
                body: vec![],
            },
            logic::Clause {
                head: "b(\"f\", \"t\")".parse().unwrap(),
                body: vec![],
            },
            logic::Clause {
                head: "b(\"g\", \"t\")".parse().unwrap(),
                body: vec![],
            },
            logic::Clause {
                head: "c(\"t\")".parse().unwrap(),
                body: vec![],
            },
        ];
        let tree = sld(&clauses, &goal, 10).success_tree.unwrap();
        let solutions = solutions(&tree);
        assert_eq!(solutions.len(), 2);
        assert!(contains_ignoring_position(
            &solutions,
            &vec!["a(\"f\")".parse().unwrap()]
        ));
        assert!(contains_ignoring_position(
            &solutions,
            &vec!["a(\"g\")".parse().unwrap()]
        ));
    }

    #[test]
    #[serial]
    fn simple_recursion() {
        let goal: Goal<logic::IRTerm> = vec!["reach(\"a\", X)".parse().unwrap()];
        let clauses: Vec<logic::Clause> = vec![
            "reach(X, Y) :- reach(X, Z), arc(Z, Y).".parse().unwrap(),
            "reach(X, Y) :- arc(X, Y).".parse().unwrap(),
            logic::Clause {
                head: "arc(\"a\", \"b\")".parse().unwrap(),
                body: vec![],
            },
            logic::Clause {
                head: "arc(\"b\", \"c\")".parse().unwrap(),
                body: vec![],
            },
            logic::Clause {
                head: "arc(\"c\", \"d\")".parse().unwrap(),
                body: vec![],
            },
            logic::Clause {
                head: "arc(\"d\", \"e\")".parse().unwrap(),
                body: vec![],
            },
            logic::Clause {
                head: "arc(\"f\", \"e\")".parse().unwrap(),
                body: vec![],
            },
            logic::Clause {
                head: "arc(\"g\", \"f\")".parse().unwrap(),
                body: vec![],
            },
            logic::Clause {
                head: "arc(\"g\", \"a\")".parse().unwrap(),
                body: vec![],
            },
        ];
        let tree = sld(&clauses, &goal, 15).success_tree.unwrap();
        let solutions = solutions(&tree);
        assert_eq!(solutions.len(), 4);
        assert!(contains_ignoring_position(
            &solutions,
            &vec!["reach(\"a\", \"b\")".parse().unwrap()]
        ));
        assert!(contains_ignoring_position(
            &solutions,
            &vec!["reach(\"a\", \"c\")".parse().unwrap()]
        ));
        assert!(contains_ignoring_position(
            &solutions,
            &vec!["reach(\"a\", \"d\")".parse().unwrap()]
        ));
        assert!(contains_ignoring_position(
            &solutions,
            &vec!["reach(\"a\", \"e\")".parse().unwrap()]
        ));
    }

    #[test]
    #[serial]
    fn string_concat() {
        let goal: Goal<logic::IRTerm> =
            vec!["string_concat(\"hello\", \"world\", X)".parse().unwrap()];
        let clauses: Vec<logic::Clause> = vec![];
        let tree = sld(&clauses, &goal, 10).success_tree.unwrap();
        let solutions = solutions(&tree);
        assert_eq!(solutions.len(), 1);
        assert!(contains_ignoring_position(
            &solutions,
            &vec!["string_concat(\"hello\", \"world\", \"helloworld\")"
                .parse()
                .unwrap()]
        ));
    }

    #[test]
    #[serial]
    fn string_concat_complex() {
        // TODO: figure out how to test empty string
        let good = ["aaabbb", "aabb"];
        let bad = ["aab", "a", "bb"];
        for (s, is_good) in good
            .iter()
            .map(|x| (*x, true))
            .chain(bad.iter().map(|x| (*x, false)))
        {
            let goal: Goal<logic::IRTerm> = vec![format!("a(\"{}\")", s).parse().unwrap()];
            let clauses: Vec<logic::Clause> = vec![
                logic::Clause {
                    head: "a(\"ab\")".parse().unwrap(),
                    body: vec![],
                },
                "a(S) :- string_concat(\"a\", X, S), string_concat(Y, \"b\", X), a(Y)"
                    .parse()
                    .unwrap(),
            ];
            let tree_res = sld(&clauses, &goal, 50);
            if is_good {
                let solutions = solutions(&tree_res.success_tree.unwrap());
                assert_eq!(solutions.len(), 1);
                assert!(contains_ignoring_position(
                    &solutions,
                    &vec![format!("a(\"{}\")", s).parse().unwrap()]
                ));
            } else {
                assert!(tree_res.success_tree.is_none());
            }
        }
    }

    #[test]
    #[serial]
    fn leaf_height_is_zero() {
        let proof = Proof {
            clause: ClauseId::Rule(0),
            valuation: HashMap::default(),
            children: Vec::new(),
        };
        assert_eq!(proof.height(), 0);
    }

    #[test]
    #[serial]
    fn proof_minimality() {
        let goal: Goal<logic::IRTerm> = vec!["foo(X)".parse().unwrap()];
        let clauses: Vec<logic::Clause> = vec![
            "foo(X) :- bar(X).".parse().unwrap(),
            logic::Clause {
                head: "bar(\"test\")".parse().unwrap(),
                body: vec![],
            },
            logic::Clause {
                head: "foo(\"test\")".parse().unwrap(),
                body: vec![],
            },
        ];
        let tree = sld(&clauses, &goal, 15).success_tree.unwrap();
        let sld_proofs = proofs(&tree, &clauses, &goal);
        assert_eq!(sld_proofs.len(), 1);
        assert_eq!(sld_proofs[0].height(), 1, "{:?}", sld_proofs[0]);
    }
}
