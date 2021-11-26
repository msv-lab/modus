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
    collections::HashMap,
    str::FromStr,
    sync::atomic::{AtomicU32, Ordering},
};

use crate::{
    dockerfile::{Dockerfile, Image, Instruction, ResolvedDockerfile, ResolvedParent, Run},
    logic::{self, Clause, Literal},
    modusfile::Modusfile,
    sld::{self, ClauseId},
};

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
    let proofs = prove_goal(&mf, &goal).unwrap();
    assert_eq!(proofs.len(), 1);
    let proof = proofs.into_iter().next().unwrap();
    proof_to_docker(proof)
}

static AVAILABLE_STAGE_INDEX: AtomicU32 = AtomicU32::new(0);
pub fn new_stage() -> ResolvedParent {
    ResolvedParent::Stage(format!(
        "_t{}",
        AVAILABLE_STAGE_INDEX.fetch_add(1, Ordering::SeqCst)
    ))
}

pub fn proof_to_docker(proof: sld::Proof) -> ResolvedDockerfile {
    let mut flattened: Vec<Literal> = Vec::new();
    fn dfs(proof: &sld::Proof, flattened: &mut Vec<Literal>) {
        if let ClauseId::Builtin(ref lit) = proof.clause {
            flattened.push(lit.clone());
        }
        for c in proof.children.iter() {
            dfs(c, flattened);
        }
    }
    dfs(&proof, &mut flattened);

    let mut instructions = Vec::new();

    fn process_image<'a>(
        literals: &'a [Literal],
        instructions: &mut Vec<Instruction<ResolvedParent>>,
        current_id: &str,
    ) -> Vec<(&'a [Literal], String)> {
        let mut i = 0usize;
        let mut dependencies = Vec::new();
        while i < literals.len() {
            let current = &literals[i];
            match &current.predicate.0[..] {
                "run" => {
                    let arg = current.args[0].as_constant().unwrap();
                    instructions.push(Instruction::Run(Run(arg.to_owned())));
                }
                "from" => {
                    let arg = current.args[0].as_constant().unwrap();
                    instructions.push(Instruction::From(crate::dockerfile::From {
                        parent: ResolvedParent::Image(Image::from_str(arg).unwrap()),
                        alias: if current_id.is_empty() {
                            None
                        } else {
                            Some(current_id.to_owned())
                        },
                    }));
                }
                "_operator_copy_begin" => {
                    let pair_id = current.args[0].as_constant().unwrap();
                    let mut j = i + 1;
                    while literals[j].predicate.0 != "_operator_copy_end" {
                        j += 1;
                    }
                    dependencies.push((&literals[i + 1..j], format!("copy_{}", pair_id)));
                    let from = current.args[1].as_constant().unwrap();
                    let to = current.args[2].as_constant().unwrap();
                    instructions.push(Instruction::Copy(crate::dockerfile::Copy(format!(
                        "--from=copy_{} {:?} {:?}",
                        pair_id, from, to
                    ))));
                    i = j + 1;
                    continue;
                }
                "_operator_copy_end" => unreachable!(),
                "copy" => {
                    let from = current.args[0].as_constant().unwrap();
                    let to = current.args[1].as_constant().unwrap();
                    instructions.push(Instruction::Copy(crate::dockerfile::Copy(format!(
                        "{:?} {:?}",
                        from, to
                    ))));
                }
                "_operator_workdir_begin" => {
                    let workdir = current.args[1].as_constant().unwrap();
                    instructions.push(Instruction::Workdir(crate::dockerfile::Workdir(
                        workdir.to_owned(),
                    )));
                }
                "_operator_workdir_end" => {
                    // TODO: restore
                }
                _ => {}
            }
            i += 1;
        }
        dependencies
    }

    let mut current_dependencies = vec![(&flattened[..], "".to_owned())];
    let mut main_instructions = Vec::new();
    while !current_dependencies.is_empty() {
        let (literals, id) = current_dependencies.remove(0);
        let new_dependencies = process_image(
            literals,
            if id == "" {
                &mut main_instructions
            } else {
                &mut instructions
            },
            &id,
        );
        current_dependencies.extend_from_slice(&new_dependencies);
    }
    instructions.extend(main_instructions.into_iter());
    Dockerfile::<ResolvedParent>(instructions)
}
