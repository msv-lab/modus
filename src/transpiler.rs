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
    str::FromStr,
    sync::atomic::{AtomicU32, Ordering},
};

use crate::{
    dockerfile::{Dockerfile, Image, Instruction, ResolvedDockerfile, ResolvedParent, Run},
    imagegen::{BuildPlan, NodeId},
    logic::{self, Clause, IRTerm, Literal, Predicate},
    modusfile::Modusfile,
    sld::{self, ClauseId},
};

use crate::imagegen::BuildNode;

// TODO: remove/rewrite
pub fn prove_goal(
    mf: &Modusfile,
    goal: &Vec<logic::Literal>,
) -> Result<Vec<sld::Proof>, &'static str> {
    let max_depth = 20;
    let clauses: Vec<Clause> =
        mf.0.iter()
            .flat_map(|mc| {
                let clauses: Vec<Clause> = mc.into();
                clauses
            })
            .collect();

    let res = sld::sld(&clauses, goal, max_depth);
    match res {
        Some(t) => Ok(sld::proofs(&t, &clauses, &goal)),
        None => Err("Failed in SLD tree construction."),
    }
}

pub fn transpile(mf: Modusfile, query: logic::Literal) -> Dockerfile<ResolvedParent> {
    let goal = vec![query];
    let max_depth = 50;
    let clauses: Vec<Clause> =
        mf.0.iter()
            .flat_map(|mc| {
                let clauses: Vec<Clause> = mc.into();
                clauses
            })
            .collect();

    let res_tree = sld::sld(&clauses, &goal, max_depth).expect("Failed in SLD tree construction.");
    // TODO: sld::proofs should return the ground query corresponding to each proof.
    let proofs = sld::proofs(&res_tree, &clauses, &goal);
    let query_and_proofs = proofs
        .into_iter()
        .enumerate()
        .map(|(i, p)| {
            (
                Literal {
                    position: None,
                    predicate: Predicate("_output".to_owned()),
                    args: vec![IRTerm::Constant(i.to_string())],
                },
                p,
            )
        })
        .collect::<Vec<_>>();
    let build_plan = crate::imagegen::build_dag_from_proofs(&query_and_proofs[..], &clauses);
    plan_to_docker(&build_plan)
}

fn plan_to_docker(plan: &BuildPlan) -> ResolvedDockerfile {
    let mut topological_order = Vec::with_capacity(plan.node.len());
    let mut seen = HashSet::new();
    fn dfs(
        plan: &BuildPlan,
        node: NodeId,
        topological_order: &mut Vec<NodeId>,
        seen: &mut HashSet<NodeId>,
    ) {
        if seen.contains(&node) {
            return;
        }
        for &deps in plan.dependencies[node].iter() {
            dfs(plan, deps, topological_order, seen);
        }
        topological_order.push(node);
        seen.insert(node);
    }
    for output in plan.outputs.iter() {
        dfs(&plan, output.node, &mut topological_order, &mut seen);
    }

    let instructions = topological_order
        .into_iter()
        .map(|node_id| {
            use crate::dockerfile::*;
            let node = &plan.node[node_id];
            let str_id = format!("n_{}", node_id);
            match node {
                BuildNode::From { image_ref } => vec![Instruction::From(From {
                    parent: ResolvedParent::Image(Image::from_str(image_ref).unwrap()),
                    alias: Some(str_id),
                })],
                BuildNode::Run {
                    parent,
                    command,
                    cwd,
                } => vec![
                    Instruction::From(From {
                        parent: ResolvedParent::Stage(format!("n_{}", parent)),
                        alias: Some(str_id),
                    }),
                    Instruction::Workdir(Workdir(cwd.to_owned())),
                    Instruction::Run(Run(command.to_owned())),
                ],
                BuildNode::CopyFromImage {
                    parent,
                    src_image,
                    src_path,
                    dst_path,
                } => vec![
                    Instruction::From(From {
                        parent: ResolvedParent::Stage(format!("n_{}", parent)),
                        alias: Some(str_id),
                    }),
                    Instruction::Copy(Copy(format!(
                        "--from=n_{} {:?} {:?}",
                        src_image, src_path, dst_path
                    ))),
                ],
                BuildNode::CopyFromLocal {
                    parent,
                    src_path,
                    dst_path,
                } => vec![
                    Instruction::From(From {
                        parent: ResolvedParent::Stage(format!("n_{}", parent)),
                        alias: Some(str_id),
                    }),
                    Instruction::Copy(Copy(format!("{:?} {:?}", src_path, dst_path))),
                ],
            }
        })
        .flatten()
        .collect::<Vec<_>>();
    Dockerfile(instructions)
}
