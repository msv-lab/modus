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

use std::{collections::HashMap, sync::atomic::AtomicU32};

use crate::{
    dockerfile::{Dockerfile, ResolvedParent},
    logic::{self, Clause, Constant, Variable},
    modusfile::Modusfile,
    sld,
};

pub fn prove_goal(
    mf: &Modusfile,
    goal: &Vec<logic::Literal>,
) -> Result<Vec<sld::Proof<logic::Constant, logic::Variable>>, &'static str> {
    let max_depth = 20;
    let clauses: Vec<Clause<logic::Constant, logic::Variable>> =
        mf.0.iter().map(|mc| mc.into()).collect();

    let res = sld::sld(&clauses, goal, max_depth);
    match res {
        Some(t) => Ok(sld::proofs(&t, &clauses, &goal)),
        None => Err("Failed in SLD tree construction."),
    }
}

pub fn transpile(mf: Modusfile, query: logic::Literal) -> Dockerfile<ResolvedParent> {
    let goal = vec![query];
    let proofs = prove_goal(&mf, &goal);
    todo!()
}

static AVAILABLE_STAGE_INDEX: AtomicU32 = AtomicU32::new(0);

fn proof_to_docker(
    rules: &Vec<Clause>,
    proof: &sld::Proof<Constant, Variable>,
    cache: &mut HashMap<sld::Goal<Constant, Variable>, ResolvedParent>,
    goal: &sld::Goal<Constant, Variable>,
) -> ResolvedParent {
    todo!()
}

fn proofs_to_docker(
    rules: &Vec<Clause>,
    proofs: &Vec<sld::Proof<Constant, Variable>>,
    goal: &sld::Goal<Constant, Variable>,
) -> Dockerfile<ResolvedParent> {
    todo!()
}
