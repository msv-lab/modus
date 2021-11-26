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
    logic::{self, Clause},
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
    let mut instructions = Vec::new();
    fn dfs(proof: &sld::Proof, instructions: &mut Vec<Instruction<ResolvedParent>>) {
        match &proof.clause {
            ClauseId::Builtin(lit) => match &lit.predicate.0[..] {
                "run" => {
                    let arg = lit.args[0].as_constant().unwrap();
                    instructions.push(Instruction::Run(Run(arg.to_owned())));
                }
                "from" => {
                    let arg = lit.args[0].as_constant().unwrap();
                    instructions.push(Instruction::From(crate::dockerfile::From {
                        parent: ResolvedParent::Image(Image::from_str(arg).unwrap()),
                        alias: None,
                    }));
                },
                "_operator_copy_begin" => unimplemented!(), // TODO
                _ => {}
            },
            _ => {}
        }
        for c in proof.children.iter() {
            dfs(c, instructions);
        }
    }
    dfs(&proof, &mut instructions);
    Dockerfile::<ResolvedParent>(instructions)
}
