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

use std::{
    collections::{HashMap, HashSet},
    fmt::{self, Debug},
    hash::Hash,
    io, iter,
};

use crate::{
    builtin,
    logic::Predicate,
    modusfile::{self, Modusfile},
    translate::translate_modusfile,
    unification::{compose_extend, compose_no_extend, Rename, Substitution},
};
use crate::{builtin::SelectBuiltinResult, unification::RenameWithSubstitution};
use crate::{
    logic::{self, Signature},
    unification::Substitute,
    wellformed,
};
use codespan_reporting::diagnostic::{Diagnostic, Label, Severity};
use colored::Colorize;
use logic::{Clause, IRTerm, Literal};
use ptree::{item::StringItem, print_tree, TreeBuilder, TreeItem};

pub trait Auxiliary: Rename<Self> + Sized {
    fn aux(anonymous: bool) -> Self;
}

type RuleId = usize;
type GoalId = usize;
type TreeLevel = usize;
pub(crate) type Goal<T = IRTerm> = Vec<Literal<T>>;

/// In this usage, a 'clause' represents some method of resolution.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum ClauseId {
    Rule(RuleId),
    Query,
    Builtin(Literal<IRTerm>),

    /// Stores the literal which we attempted to prove.
    /// So it should be a positive literal.
    NegationCheck(Literal<IRTerm>),
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
#[derive(Clone, PartialEq, Debug)]
pub struct Tree {
    goal: GoalWithHistory,
    level: TreeLevel,

    /// Branches that could lead to a successful path.
    success_resolvents: HashMap<(LiteralGoalId, ClauseId), (Substitution, Substitution, Tree)>,

    /// Branches that will lead to failing paths.
    fail_resolvents: HashMap<(LiteralGoalId, ClauseId), (Substitution, Substitution, Tree)>,

    /// Possible error associated with this node. It is probably a leaf if present.
    /// If this is a negation check, this might not be a leaf node.
    ///
    /// Note that even if error is None, the tree may still have failing paths,
    /// it's just that this particular node is not the point where it failed.
    error: Option<ResolutionError>,
}

impl Tree {
    fn is_success(&self) -> bool {
        self.goal.is_empty() || !self.success_resolvents.is_empty()
    }

    fn resolvents(
        &self,
    ) -> HashMap<&(usize, ClauseId), &(HashMap<IRTerm, IRTerm>, HashMap<IRTerm, IRTerm>, Tree)>
    {
        self.success_resolvents
            .iter()
            .chain(&self.fail_resolvents)
            .collect::<HashMap<_, _>>()
    }

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

            if !t.goal.is_empty() && t.resolvents().is_empty() {
                let err = t
                    .error
                    .as_ref()
                    .expect("Failed path should store SLD error.");
                let error_msg = err.to_string();
                nodes.push("FAIL".to_string());
                edges.push((curr_index, curr_index + 1, error_msg));
            } else {
                let mut resolvent_pairs = t.resolvents().into_iter().collect::<Vec<_>>();
                resolvent_pairs.sort_by_key(|(k, _)| k.0); // for some consistency

                for (k, v) in resolvent_pairs {
                    let new_index = nodes.len();
                    convert(&v.2, rules, nodes, edges);

                    let edge_label = match &k.1 {
                        ClauseId::Rule(rid) => rules[*rid].head.to_string(),
                        ClauseId::Query => "query".to_string(),
                        ClauseId::Builtin(lit) => lit.to_string(),
                        ClauseId::NegationCheck(lit) => format!("Check {lit}?"),
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
    pub fn explain(&self, rules: &[Clause]) -> StringItem {
        fn dfs(t: &Tree, rules: &[Clause], builder: &mut TreeBuilder) {
            let mut resolvent_pairs = t.resolvents().into_iter().collect::<Vec<_>>();
            resolvent_pairs.sort_by_key(|(k, _)| k.0);

            for (k, v) in &resolvent_pairs {
                let (goal_id, cid) = k;

                let substitution_map: HashMap<_, _> =
                    v.0.iter()
                        .map(|(t1, t2)| (t1.get_original().clone(), t2.clone()))
                        .collect();

                let requirement = match cid {
                    ClauseId::Rule(rid) => rules[*rid]
                        .substitute(&substitution_map)
                        .body
                        .iter()
                        .map(|x| x.to_string())
                        .collect::<Vec<_>>()
                        .join(", "),
                    ClauseId::Query => unimplemented!(),
                    ClauseId::Builtin(lit) => lit.substitute(&v.0).to_string(),
                    ClauseId::NegationCheck(lit) => {
                        format!("{} to have no proof", lit)
                    }
                };
                let curr_attempt = format!(
                    "{} requires {}",
                    t.goal[*goal_id].literal,
                    if requirement.is_empty() {
                        "nothing, it's a fact.".to_owned().italic()
                    } else {
                        requirement.italic()
                    }
                );

                if let Some(e) = &v.2.error {
                    builder.begin_child(format!("{}", e.to_string().bright_red()));
                } else {
                    builder.begin_child(curr_attempt);
                }
                dfs(&v.2, rules, builder);
                builder.end_child();
            }

            if t.goal.is_empty() {
                builder.add_empty_child(format!("{}", "Success".green()));
            }
        }

        let mut builder = TreeBuilder::new(format!(
            "We require {}",
            self.goal
                .iter()
                .map(|l| l.literal.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        ));
        dfs(self, rules, &mut builder);
        builder.build()
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
        self.children
            .iter()
            .map(|child| child.height() + 1)
            .max()
            .unwrap_or(0)
    }

    pub fn get_tree(&self, clauses: &Vec<Clause>) -> impl TreeItem {
        fn dfs(p: &Proof, clauses: &Vec<Clause>, builder: &mut TreeBuilder) {
            for child in &p.children {
                match &child.clause {
                    ClauseId::Rule(rid) => {
                        builder.begin_child(format!(
                            "{}",
                            clauses[*rid]
                                .head
                                .substitute(&p.valuation)
                                .to_string()
                                .dimmed()
                        ));
                        dfs(&child, clauses, builder);
                        builder.end_child();
                    }
                    ClauseId::Query => {
                        builder.add_empty_child("query".to_string());
                    }
                    ClauseId::Builtin(b) => match b.predicate.naive_predicate_kind() {
                        crate::analysis::Kind::Image => {
                            builder.add_empty_child(format!(
                                "{}",
                                b.substitute(&p.valuation).to_string().cyan()
                            ));
                        }
                        crate::analysis::Kind::Layer => {
                            builder.add_empty_child(format!(
                                "{}",
                                b.substitute(&p.valuation).to_string().bright_blue()
                            ));
                        }
                        crate::analysis::Kind::Logic => {
                            if b.predicate.is_operator() {
                                if b.predicate.0.ends_with("_begin") {
                                    builder.begin_child("(".to_string());
                                } else {
                                    builder.end_child();
                                    // HACK: Because we want to print the operators *after* their scope,
                                    //       we add a leaf node here. It will look as expected in the term
                                    //       but may not make sense as a DAG.
                                    builder.add_empty_child(format!(
                                        ")::{}",
                                        b.substitute(&p.valuation).unmangle().to_string().italic()
                                    ));
                                }
                            }
                        }
                    },
                    ClauseId::NegationCheck(_) => {} // negation checks are omitted from the proof tree
                }
            }
        }

        let mut builder = TreeBuilder::new("".to_string());
        dfs(self, clauses, &mut builder);

        builder.build()
    }

    pub fn pretty_print(&self, clauses: &Vec<Clause>) -> io::Result<()> {
        print_tree(&self.get_tree(clauses))
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
                    introduction: *introduction,
                    origin: origin.clone(),
                },
            )
            .collect()
    }
}

#[derive(Clone, Eq, PartialEq, Debug, Hash)]
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
    InconsistentGroundnessSignature(Vec<Signature>),
    /// Proof of a negated literal was found.
    NegationProof(Literal),
}

impl fmt::Display for ResolutionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ResolutionError::UnknownPredicate(literal) => {
                if literal.predicate.is_operator() {
                    write!(f, "unknown operator - {}", literal.predicate)
                } else {
                    write!(f, "unknown predicate - {}", literal.predicate)
                }
            }
            ResolutionError::InsufficientGroundness(literals) => {
                write!(f, "insufficient groundness for {} goal(s)", literals.len())
            }
            ResolutionError::MaximumDepthExceeded(_, max_depth) => {
                write!(f, "exceeded maximum depth of {}", max_depth)
            }
            ResolutionError::BuiltinFailure(_, builtin_name) => {
                write!(f, "builtin {} failed to apply or unify", builtin_name)
            }
            ResolutionError::InsufficientRules(literal) => write!(
                f,
                "could not find a rule to resolve with literal {}",
                literal
            ),
            ResolutionError::InconsistentGroundnessSignature(signatures) => write!(
                f,
                "{} clause(s) have inconsistent signatures",
                signatures.len()
            ),
            ResolutionError::NegationProof(lit) => {
                write!(f, "A proof was found for {}", lit.negated())
            }
        }
    }
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

        let message = self.to_string();
        let (labels, notes, severity) = match self {
            ResolutionError::UnknownPredicate(literal) => (
                get_position_labels(&[literal.clone()]),
                get_notes(&[literal]),
                Severity::Error,
            ),
            ResolutionError::InsufficientGroundness(literals) => (
                get_position_labels(&literals),
                get_notes(&literals),
                Severity::Warning,
            ),
            ResolutionError::MaximumDepthExceeded(literals, _) => (
                get_position_labels(&literals),
                get_notes(&literals),
                Severity::Warning,
            ),
            ResolutionError::BuiltinFailure(literal, _) => (
                get_position_labels(&[literal.clone()]),
                get_notes(&[literal]),
                Severity::Warning,
            ),
            ResolutionError::InsufficientRules(literal) => (
                get_position_labels(&[literal.clone()]),
                get_notes(&[literal]),
                Severity::Warning,
            ),
            ResolutionError::InconsistentGroundnessSignature(sigs) => (
                Vec::new(),
                sigs.iter().map(|sig| sig.to_string()).collect(),
                Severity::Error,
            ),
            ResolutionError::NegationProof(lit) => (
                get_position_labels(&[lit.clone()]),
                get_notes(&[lit]),
                Severity::Warning,
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
    pub tree: Tree,
    pub errors: HashSet<ResolutionError>,
}

impl From<SLDResult> for Result<Tree, Vec<Diagnostic<()>>> {
    fn from(sld_result: SLDResult) -> Self {
        if sld_result.tree.is_success() {
            Ok(sld_result.tree)
        } else {
            Err(sld_result
                .errors
                .into_iter()
                .map(ResolutionError::get_diagnostic)
                .collect::<Vec<_>>())
        }
    }
}

/// Returns a tree that contains both successful and failed paths, also, any resolution errors.
/// To save on memory usage, can avoid storing the failed paths by passing false to `store_full_tree`.
pub fn sld(
    rules: &[Clause<IRTerm>],
    goal: &Goal,
    maxdepth: TreeLevel,
    store_full_tree: bool,
) -> SLDResult {
    /// Select leftmost literal with compatible groundness.
    fn select(
        goal: &GoalWithHistory,
        grounded: &HashMap<Signature, Vec<bool>>,
    ) -> Result<(LiteralGoalId, LiteralWithHistory), ResolutionError> {
        for (id, lit) in goal.iter().enumerate() {
            let literal = &lit.literal;

            // A negated literal must have only constants or anonymous variables (which represent
            // variables that will not equal any other).
            // Otherwise, something like !string_eq("constant", X) would be pointless, the
            // user very likely means X to be bound through some other literal.
            // An alternative approach would be to check other variables in the goal.
            let positive_or_grounded_negation = literal.positive
                || literal
                    .args
                    .iter()
                    .all(|arg| arg.is_constant() || arg.is_underlying_anonymous_variable());

            let select_builtin_res = builtin::select_builtin(literal);
            if select_builtin_res.0.is_match() && positive_or_grounded_negation {
                return Ok((id, lit.clone()));
            }

            // For any user-defined atom, we can get its groundness requirement
            // (computed outside), and if a particular argument can not be
            // ungrounded (grounded[arg_index] == false), variables will not be
            // allowed there.
            let lit_grounded = grounded.get(&literal.signature());
            if let Some(lit_grounded) = lit_grounded {
                debug_assert_eq!(lit_grounded.len(), literal.args.len());
                if positive_or_grounded_negation
                    && literal
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

    /// NOTE: the new goals are added *first*, so the behaviour of SLD changes (but not the
    /// semantics, I think), making it more 'eager' to resolve.
    /// This makes it possible to get significant performance boosts by placing ground facts first
    /// in the body of some expression, in your Modusfile(s).
    /// For example, `fact(c), expensive_goal(c)`, may waste a lot of time and memory if `fact(c)` is
    /// not true. With the 'eager' approach, SLD will quickly terminate if `fact(c)` is false.
    fn resolve(
        lid: LiteralGoalId,
        rid: ClauseId,
        goal: &GoalWithHistory,
        mgu: &Substitution,
        rule: &Clause,
        level: TreeLevel,
    ) -> GoalWithHistory {
        let new_goals = rule.body.iter().enumerate().map(|(id, l)| {
            let origin = LiteralOrigin {
                clause: rid.clone(),
                body_index: id,
            };
            LiteralWithHistory {
                literal: l.clone(),
                introduction: level,
                origin,
            }
        });
        let g = new_goals
            .chain(goal.into_iter().enumerate().filter_map(|(i, v)| {
                if i != lid {
                    Some(v.clone())
                } else {
                    None
                }
            }))
            .collect::<Vec<_>>();
        g.substitute(mgu)
    }

    fn handle_negated_literal(
        lid: LiteralGoalId,
        l: LiteralWithHistory,
        goal: &GoalWithHistory,
        rules: &[Clause<IRTerm>],
        maxdepth: TreeLevel,
        level: TreeLevel,
        grounded: &HashMap<Signature, Vec<bool>>,
        store_full_tree: bool,
    ) -> SLDResult {
        let mut errs: HashSet<ResolutionError> = HashSet::new();

        let singleton_goal = vec![LiteralWithHistory {
            literal: l.literal.negated(),
            ..l
        }];

        // Perform SLD resolution with this goal and check if it succeeds or not.
        let sld_res = inner(
            rules,
            &singleton_goal,
            // The stratifiability check should make it safe to use the same maxdepth.
            maxdepth,
            0,
            grounded,
            store_full_tree,
        );

        let rid = ClauseId::NegationCheck(l.literal.negated());
        let mgu = HashMap::new();
        let renaming = HashMap::new();

        let mut success_resolvents = HashMap::new();
        let mut fail_resolvents = HashMap::new();
        if sld_res.tree.is_success() {
            if store_full_tree {
                fail_resolvents.insert((lid, rid), (mgu, renaming, sld_res.tree));
            }

            let err = ResolutionError::NegationProof(l.literal);
            errs.insert(err.clone());
            let tree = Tree {
                goal: goal.to_owned(),
                level,
                success_resolvents,
                fail_resolvents,
                error: Some(err),
            };

            return SLDResult { tree, errors: errs };
        } else {
            let resolvent = resolve(
                lid,
                rid.clone(),
                goal,
                &mgu,
                &Clause {
                    head: l.literal,
                    body: Vec::new(),
                },
                level + 1,
            );
            let SLDResult { tree, errors } = inner(
                rules,
                &resolvent,
                maxdepth,
                level + 1,
                grounded,
                store_full_tree,
            );

            if tree.is_success() {
                success_resolvents.insert((lid, rid), (mgu, renaming, tree));
            } else if store_full_tree {
                fail_resolvents.insert((lid, rid), (mgu, renaming, tree));
            }
            errs.extend(errors);

            let tree = Tree {
                goal: goal.to_owned(),
                level,
                success_resolvents,
                fail_resolvents,
                error: None,
            };

            return SLDResult { tree, errors: errs };
        }
    }

    fn inner(
        rules: &[Clause<IRTerm>],
        goal: &GoalWithHistory,
        maxdepth: TreeLevel,
        level: TreeLevel,
        grounded: &HashMap<Signature, Vec<bool>>,
        store_full_tree: bool,
    ) -> SLDResult {
        if goal.is_empty() {
            let t = Tree {
                goal: goal.to_owned(),
                level,
                success_resolvents: HashMap::new(),
                fail_resolvents: HashMap::new(),
                error: None,
            };
            SLDResult {
                tree: t,
                errors: HashSet::new(),
            }
        } else if level >= maxdepth {
            let error = ResolutionError::MaximumDepthExceeded(
                goal.iter()
                    .map(|lit_hist| lit_hist.literal.clone())
                    .collect(),
                maxdepth,
            );
            let t = Tree {
                goal: goal.to_owned(),
                level,
                success_resolvents: HashMap::default(),
                fail_resolvents: HashMap::default(),
                error: Some(error.clone()),
            };
            let errors = vec![error].into_iter().collect();
            SLDResult { tree: t, errors }
        } else {
            let selection_res = select(goal, grounded);
            if let Err(e) = selection_res {
                let t = Tree {
                    goal: goal.to_owned(),
                    level,
                    success_resolvents: HashMap::default(),
                    fail_resolvents: HashMap::default(),
                    error: Some(e.clone()),
                };
                return SLDResult {
                    tree: t,
                    errors: vec![e].into_iter().collect(),
                };
            }
            let (lid, l) = selection_res.unwrap();

            if !l.literal.positive {
                return handle_negated_literal(
                    lid,
                    l,
                    goal,
                    rules,
                    maxdepth,
                    level,
                    grounded,
                    store_full_tree,
                );
            }

            let mut errs: HashSet<ResolutionError> = HashSet::new();

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

            let mut leaf_error = None;
            if selected_builtin.0.is_match() && builtin_resolves.is_none() {
                let err = ResolutionError::BuiltinFailure(
                    l.literal.clone(),
                    selected_builtin
                        .1
                        .expect("match should provide builtin")
                        .name(),
                );
                errs.insert(err.clone());
                leaf_error = Some(err);
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
                let err = ResolutionError::InsufficientRules(l.literal.clone());
                errs.insert(err.clone());
                leaf_error = leaf_error.or(Some(err));
            }

            let mut success_resolvents: HashMap<
                (LiteralGoalId, ClauseId),
                (Substitution, Substitution, Tree),
            > = HashMap::new();
            let mut fail_resolvents: HashMap<
                (LiteralGoalId, ClauseId),
                (Substitution, Substitution, Tree),
            > = HashMap::new();
            for (rid, mgu, renaming, resolvent) in
                builtin_resolves.into_iter().chain(user_rules_resolves)
            {
                let SLDResult { tree, errors } = inner(
                    rules,
                    &resolvent,
                    maxdepth,
                    level + 1,
                    grounded,
                    store_full_tree,
                );
                if tree.is_success() {
                    success_resolvents.insert((lid, rid), (mgu, renaming, tree));
                } else if store_full_tree {
                    fail_resolvents.insert((lid, rid), (mgu, renaming, tree));
                }
                errs.extend(errors);
            }

            let tree = Tree {
                goal: goal.to_owned(),
                level,
                success_resolvents,
                fail_resolvents,
                error: leaf_error,
            };

            SLDResult { tree, errors: errs }
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
        Ok(grounded) => inner(
            rules,
            &goal_with_history,
            maxdepth,
            0,
            &grounded,
            store_full_tree,
        ),
        Err(e) => SLDResult {
            tree: Tree {
                goal: goal_with_history,
                level: 0,
                success_resolvents: HashMap::default(),
                fail_resolvents: HashMap::default(),
                error: Some(ResolutionError::InconsistentGroundnessSignature(
                    e.iter().cloned().collect(),
                )),
            },
            errors: vec![ResolutionError::InconsistentGroundnessSignature(
                e.into_iter().collect(),
            )]
            .into_iter()
            .collect(),
        },
    }
}

pub fn solutions(tree: &Tree) -> HashSet<Goal> {
    fn inner(tree: &Tree) -> Vec<Substitution> {
        if tree.goal.is_empty() {
            let s = Substitution::new();
            return vec![s];
        }
        tree.success_resolvents
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

pub fn proofs(tree: &Tree, rules: &[Clause], goal: &Goal) -> HashMap<Goal, Proof> {
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
                    selected: *lid,
                    renaming: renaming.clone(),
                }],
                mgu.clone(),
            )];
        }
        tree.success_resolvents
            .iter()
            .map(|((sub_lid, sub_cid), (sub_mgu, sub_renaming, sub_tree))| {
                flatten_compose(sub_lid, sub_cid, sub_mgu, sub_renaming, sub_tree)
                    .iter()
                    .map(|(sub_path, sub_val)| {
                        let mut nodes = vec![PathNode {
                            resolvent: tree.goal.clone(),
                            applied: cid.clone(),
                            selected: *lid,
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
        path: &[PathNode],
        mgu: &Substitution,
        rules: &[Clause],
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
            // There shouldn't be a subtree here since the tree is currently only stored
            // if the negation check failed (i.e. we found a proof).
            ClauseId::NegationCheck(_) => assert_eq!(children_length, 0),
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
        .map(|l| l.variables(true))
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
    solution_to_proof_tree
}

pub fn tree_from_modusfile(
    mf: Modusfile,
    query: modusfile::Expression,
    max_depth: usize,
    full_tree: bool,
) -> (Goal, Vec<Clause>, SLDResult) {
    // 1. Create a new clause with a nullary goal '_query', with a body of the user's query.
    // 2. Translate this and other clauses.
    // 3. Use the body of the IR clause with the '_query' head predicate as the goal.

    let goal_pred = Predicate("_query".to_owned());
    let user_clause = modusfile::ModusClause {
        head: Literal {
            positive: true,
            position: None,
            predicate: goal_pred.clone(),
            args: Vec::new(),
        },
        body: Some(query),
    };
    let clauses: Vec<Clause> = translate_modusfile(&Modusfile(
        mf.0.into_iter().chain(iter::once(user_clause)).collect(),
    ));

    let q_clause = clauses
        .iter()
        .find(|c| c.head.predicate == goal_pred)
        .expect("should find same predicate name after translation");
    let goal = &q_clause.body;

    (
        goal.clone(),
        clauses.clone(),
        sld(&clauses, &goal, max_depth, full_tree),
    )
}

#[cfg(test)]
mod tests {
    use crate::modusfile::{Expression, ModusTerm};

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
        let tree = sld(&clauses, &goal, 10, true).tree;
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
    fn simple_negation_solving() {
        let goal: Goal<logic::IRTerm> = vec!["a(\"c\")".parse().unwrap()];
        let clauses: Vec<logic::Clause> = vec![
            "a(X) :- !b(X).".parse().unwrap(),
            "b(\"d\").".parse().unwrap(),
        ];
        let sld_res = sld(&clauses, &goal, 10, true);
        let tree = sld_res.tree;
        let solutions = solutions(&tree);
        assert_eq!(solutions.len(), 1);

        assert!(contains_ignoring_position(
            &solutions,
            &vec!["a(\"c\")".parse::<logic::Literal>().unwrap()]
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
        let tree = sld(&clauses, &goal, 10, true).tree;
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
        let result = sld(&clauses, &goal, 10, true);
        assert_eq!(
            vec![ResolutionError::InsufficientGroundness(goal)],
            result.errors.into_iter().collect::<Vec<_>>()
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
        let tree = sld(&clauses, &goal, 10, true).tree;
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
        let tree = sld(&clauses, &goal, 10, true).tree;
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
        let tree = sld(&clauses, &goal, 15, true).tree;
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
        let tree = sld(&clauses, &goal, 10, true).tree;
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
            let tree_res = sld(&clauses, &goal, 50, true);
            if is_good {
                let solutions = solutions(&tree_res.tree);
                assert_eq!(solutions.len(), 1);
                assert!(contains_ignoring_position(
                    &solutions,
                    &vec![format!("a(\"{}\")", s).parse().unwrap()]
                ));
            } else {
                assert!(!tree_res.tree.is_success());
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
            "bar(\"test\").".parse().unwrap(),
            "foo(\"test\").".parse().unwrap(),
        ];
        let tree = sld(&clauses, &goal, 15, true).tree;
        let sld_proofs = proofs(&tree, &clauses, &goal);
        assert_eq!(sld_proofs.len(), 1);
        assert_eq!(
            sld_proofs.values().next().unwrap().height(),
            1,
            "{:?}",
            sld_proofs[&goal]
        );
    }

    #[test]
    #[serial]
    fn tree_from_expression_query() {
        let mf: Modusfile = "base_image(\"alpine3.14\"). base_image(\"alpine3.15\")."
            .parse()
            .unwrap();
        let query = Expression::Literal(Literal {
            positive: true,
            position: None,
            predicate: Predicate("base_image".into()),
            args: vec![ModusTerm::FormatString {
                position: logic::SpannedPosition {
                    offset: 11,
                    length: 13,
                },
                format_string_literal: "alpine${X}".to_owned(),
            }],
        });

        let (_, _, sld_res) = tree_from_modusfile(mf, query, 20, true);
        assert!(sld_res.tree.is_success());
    }

    #[test]
    #[serial]
    fn negation_and_builtins() {
        let goal: Goal<logic::IRTerm> = vec!["!is_alpine(\"notalpine3.15\")".parse().unwrap()];
        let clauses: Vec<logic::Clause> = vec![
            "is_alpine(variant) :- string_concat(\"alpine\", version, variant)."
                .parse()
                .unwrap(),
        ];
        let sld_res = sld(&clauses, &goal, 10, true);
        let tree = sld_res.tree;
        let solutions = solutions(&tree);
        assert_eq!(solutions.len(), 1);

        assert!(contains_ignoring_position(
            &solutions,
            &vec!["!is_alpine(\"notalpine3.15\")"
                .parse::<logic::Literal>()
                .unwrap()]
        ));
    }

    #[test]
    #[serial]
    fn negation_and_anonymous_variable() {
        let goal: Goal<logic::IRTerm> = vec!["!is_alpine(\"notalpine3.15\", _)".parse().unwrap()];
        let clauses: Vec<logic::Clause> = vec![
            "is_alpine(variant, version) :- string_concat(\"alpine\", version, variant)."
                .parse()
                .unwrap(),
        ];
        let sld_res = sld(&clauses, &goal, 10, true);
        let tree = sld_res.tree;
        let solutions = solutions(&tree);
        assert_eq!(solutions.len(), 1);

        assert!(contains_ignoring_position(&solutions, &goal));
    }

    #[test]
    #[serial]
    fn negation_errors_when_unknown() {
        let goal: Goal<logic::IRTerm> = vec!["!is_alpine(\"notalpine3.15\", _)".parse().unwrap()];
        let clauses: Vec<logic::Clause> = vec![];
        let sld_res = sld(&clauses, &goal, 10, true);

        assert_eq!(sld_res.errors.len(), 1);
        let is_match = matches!(
            sld_res.errors.iter().next(),
            Some(ResolutionError::UnknownPredicate(_))
        );
        assert!(is_match);
    }
}
