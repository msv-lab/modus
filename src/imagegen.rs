use std::collections::HashMap;
use std::path::{Path, PathBuf};

use crate::logic::{Clause, IRTerm, Literal, Predicate};
use crate::sld::{ClauseId, Proof};
use crate::unification::Substitute;

#[derive(Debug, Clone)]
pub struct BuildPlan {
    pub node: Vec<BuildNode>,
    pub dependencies: Vec<Vec<NodeId>>,
    pub outputs: Vec<Output>,
}

impl BuildPlan {
    pub fn new() -> BuildPlan {
        BuildPlan {
            node: Vec::new(),
            dependencies: Vec::new(),
            outputs: Vec::new(),
        }
    }

    pub fn new_node(&mut self, node: BuildNode, deps: Vec<NodeId>) -> NodeId {
        let id = self.node.len();
        self.node.push(node);
        self.dependencies.push(deps);
        debug_assert_eq!(self.node.len(), self.dependencies.len());
        id
    }
}

#[derive(Debug, Clone)]
struct State {
    cwd: String,
}

pub type NodeId = usize;

/// Represent one operation, such as `RUN` or `FROM`.
///
/// Think of it as one line of a Dockerfile, or one node in the buildkit graph.
///
/// TODO: add caching control
#[derive(Debug, Clone)]
pub enum BuildNode {
    From {
        image_ref: String,
    },
    Run {
        parent: NodeId,
        command: String,
        cwd: String,
    },
    CopyFromImage {
        parent: NodeId,
        src_image: NodeId,
        src_path: String,
        dst_path: String,
    },
    CopyFromLocal {
        parent: NodeId,
        src_path: String,
        dst_path: String,
    },
}

#[derive(Debug, Clone)]
pub struct Output {
    pub node: NodeId,
    pub query: Literal,
}

/// Given a list of pairs of ground (solved) queries and their proof tree, output
/// a build graph which builds all the queried images.
pub fn build_dag_from_proofs(
    query_and_proofs: &[(Literal, Proof)],
    rules: &Vec<Clause<IRTerm>>,
) -> BuildPlan {
    let mut res = BuildPlan::new();
    let mut image_literals: HashMap<Literal, NodeId> = HashMap::new();

    /// Takes in a part of the build tree, assuming that it is building an image
    /// (for example, the tree of an image literal, or a slice of a bigger tree,
    /// where the slice contains literals that occurs between _operator_copy_begin
    /// and _operator_copy_end).
    ///
    /// This function should return None if the subtree passed in does not build
    /// an image, for example because there is no call to any intrinsic. It
    /// should, however, report an error if there is no `from` but there is a
    /// `run`.
    fn process_image(
        subtree: &[&Proof],
        rules: &Vec<Clause<IRTerm>>,
        res: &mut BuildPlan,
        image_literals: &mut HashMap<Literal, NodeId>,
    ) -> Option<NodeId> {
        let mut last_node = None;
        let mut curr_state = State {
            cwd: "/".to_string(),
        };

        /* We go through the proof tree in depth-first order, since this is
         * effectively what the build instructions are supposed to be ordered in.
         *
         * Consider this as an example:
         * a :- b, c, run(5)
         * b :- run(1)
         * c :- run(2), d, run(4)
         * d :- run(3)
         *
         * Remember that repeated call to a literal will result in the expansion
         * of that literal to be repeated in the tree, so we don't need to worry
         * about that. A special optimization is made for when we see a literal
         * before any `from` (last_node == None), in which case we can check if
         * we already built that literal as another image, and reuse when
         * possible. In the case where we haven't, we can also store the node we
         * built from going into that literal into the image_literals store, to
         * be re-used later.
         */
        fn process_tree(
            proof: &Proof,
            last_node: &mut Option<NodeId>,
            rules: &Vec<Clause<IRTerm>>,
            res: &mut BuildPlan,
            image_literals: &mut HashMap<Literal, NodeId>,
            curr_state: &mut State,
        ) {
            match proof.clause {
                ClauseId::Query => {}
                ClauseId::Builtin(ref intrinsic) => {
                    process_intrinsic(intrinsic, last_node, rules, res, image_literals, curr_state);
                    debug_assert!(proof.children.is_empty()); // Intrinsics should not have children.
                    return;
                }
                ClauseId::Rule(rid) => {
                    let substituted_lit = rules[rid].head.substitute(&proof.valuation);
                    debug_assert!(substituted_lit
                        .args
                        .iter()
                        .all(|x| x.as_constant().is_some()));

                    if last_node.is_none() {
                        // Do the optimization mentioned above.
                        if let Some(&node_id) = image_literals.get(&substituted_lit) {
                            last_node.replace(node_id);
                            return; // no need to recurse to children anymore.
                        } else {
                            if let Some(node_id) = process_image(
                                &proof.children.iter().collect::<Vec<_>>()[..],
                                rules,
                                res,
                                image_literals,
                            ) {
                                last_node.replace(node_id);
                                image_literals.insert(substituted_lit, node_id);
                                return; // no need to recurse to children anymore, since I just built the content of this literal.
                            } else {
                                return; // the literal doesn't do any docker thing, so we can safely skip it.
                            }
                        }
                    } else {
                        // Can't re-use anymore since we already started an image.
                        // In this case the subtree of this literal shouldn't be
                        // an image anyway, so just dfs as normal.
                    }
                }
            }

            process_children(
                &proof.children.iter().collect::<Vec<_>>(),
                last_node,
                rules,
                res,
                image_literals,
                curr_state,
            );
        }

        fn process_intrinsic(
            intrinsic: &Literal,
            last_node: &mut Option<NodeId>,
            rules: &Vec<Clause<IRTerm>>,
            res: &mut BuildPlan,
            image_literals: &mut HashMap<Literal, NodeId>,
            curr_state: &mut State,
        ) {
            let name = &intrinsic.predicate.0[..];
            assert!(!name.starts_with("_operator_")); // operators handled separately below.
            match name {
                "from" => {
                    if last_node.is_some() {
                        panic!("from must be the first build instruction.");
                    }
                    // Special sharing for the "from" intrinsic.
                    if let Some(&existing_node) = image_literals.get(&intrinsic) {
                        last_node.replace(existing_node);
                    } else {
                        let new_node = res.new_node(
                            BuildNode::From {
                                image_ref: intrinsic.args[0].as_constant().unwrap().to_owned(),
                            },
                            vec![],
                        );
                        last_node.replace(new_node);
                        image_literals.insert(intrinsic.clone(), new_node);
                    }
                }
                "run" => {
                    if last_node.is_none() {
                        panic!("No base layer yet.");
                    }
                    let parent = last_node.unwrap();
                    let command = intrinsic.args[0].as_constant().unwrap().to_owned();
                    last_node.replace(res.new_node(
                        BuildNode::Run {
                            parent: parent,
                            command: command,
                            cwd: curr_state.cwd.clone(),
                        },
                        vec![parent],
                    ));
                }
                "copy" => {
                    if last_node.is_none() {
                        panic!("No base layer yet.");
                    }
                    let parent = last_node.unwrap();
                    let src_path = intrinsic.args[0].as_constant().unwrap().to_owned();
                    let dst_path = intrinsic.args[1].as_constant().unwrap();
                    let dst_path = Path::new(&curr_state.cwd)
                        .join(dst_path)
                        .to_string_lossy()
                        .into_owned();
                    last_node.replace(res.new_node(
                        BuildNode::CopyFromLocal {
                            parent,
                            src_path,
                            dst_path,
                        },
                        vec![parent],
                    ));
                }
                _ => {}
            }
        }

        fn process_children(
            children: &[&Proof],
            last_node: &mut Option<NodeId>,
            rules: &Vec<Clause<IRTerm>>,
            res: &mut BuildPlan,
            image_literals: &mut HashMap<Literal, NodeId>,
            curr_state: &mut State,
        ) {
            let mut i = 0usize;
            while i < children.len() {
                let child = children[i];
                if let ClauseId::Builtin(ref lit) = child.clause {
                    let name = &lit.predicate.0;
                    if let Some(op_name) = name
                        .strip_prefix("_operator_")
                        .and_then(|s| s.strip_suffix("_begin"))
                    {
                        // due to the way things work, the end predicate for this is
                        // guarenteed to be in the same level.
                        let end_name = format!("_operator_{}_end", op_name);
                        let pair_id = lit.args[0].as_constant().unwrap();
                        let mut j = i + 1;
                        while !{
                            if let ClauseId::Builtin(ref lit) = children[j].clause {
                                lit.predicate.0 == end_name
                                    && lit.args[0].as_constant() == Some(pair_id)
                            } else {
                                false
                            }
                        } {
                            j += 1;
                        }
                        // at this point j points to the end predicate.

                        let subtree_in_op = &children[i + 1..j];
                        // process this operator
                        match op_name {
                            "copy" => {
                                let parent = last_node.expect("No base layer yet.");
                                let img = process_image(subtree_in_op, rules, res, image_literals)
                                    .expect("Stuff inside this copy does not build an image.");
                                let node = res.new_node(
                                    // TODO: resolve path relative to state.cwd.
                                    BuildNode::CopyFromImage {
                                        parent,
                                        src_image: img,
                                        src_path: lit.args[1].as_constant().unwrap().to_owned(),
                                        dst_path: Path::new(&curr_state.cwd)
                                            .join(lit.args[2].as_constant().unwrap())
                                            .to_string_lossy()
                                            .into_owned(),
                                    },
                                    vec![parent, img],
                                );
                                last_node.replace(node);
                            }
                            "workdir" => {
                                let mut new_state = curr_state.clone();
                                new_state.cwd = Path::new(&curr_state.cwd).join(lit.args[1].as_constant().unwrap()).to_string_lossy().into_owned();
                                process_children(
                                    subtree_in_op,
                                    last_node,
                                    rules,
                                    res,
                                    image_literals,
                                    &mut new_state,
                                );
                            }
                            _ => {}
                        }
                        i = j + 1;
                        continue;
                    }
                }
                process_tree(child, last_node, rules, res, image_literals, curr_state);
                i += 1;
            }
        }

        process_children(
            subtree,
            &mut last_node,
            rules,
            res,
            image_literals,
            &mut curr_state,
        );

        last_node
    }

    for (query, proof) in query_and_proofs {
        debug_assert!(query.args.iter().all(|x| x.as_constant().is_some()));
        if let Some(&existing_node_id) = image_literals.get(&query) {
            // TODO: unreachable?
            res.outputs.push(Output {
                node: existing_node_id,
                query: query.clone(),
            });
            continue;
        }
        if let Some(node_id) = process_image(&[proof], rules, &mut res, &mut image_literals) {
            image_literals.insert(query.clone(), node_id);
            res.outputs.push(Output {
                node: node_id,
                query: query.clone(),
            });
        } else {
            panic!("{} does not resolve to any docker instructions.", query);
        }
    }

    res
}
