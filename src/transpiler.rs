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

use core::fmt;
use std::{
    collections::HashMap,
    str::FromStr,
    sync::atomic::{AtomicU32, Ordering},
    thread::panicking,
};

use nom::map;

use crate::{
    dockerfile,
    dockerfile::{Dockerfile, Image, Instruction, ResolvedDockerfile, ResolvedParent, Run},
    logic::{Atom, Clause, Literal, Term},
    modusfile,
    modusfile::{Constant, Modusfile},
    sld::{self, ClauseId},
    unification::{Rename, Substitute, Substitution},
};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Variable {
    User(String),
    Auxiliary(u32),
    Renamed(u32, Box<Variable>),
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variable::User(s) => write!(f, "{}", s),
            _ => unimplemented!(),
        }
    }
}

static AVAILABLE_VARIABLE_INDEX: AtomicU32 = AtomicU32::new(0);

impl Rename<Constant, Variable> for Variable {
    type Output = Variable;
    fn rename(&self) -> (Variable, Substitution<Constant, Variable>) {
        let index = AVAILABLE_VARIABLE_INDEX.fetch_add(1, Ordering::SeqCst);
        let renamed = Variable::Renamed(index, Box::new((*self).clone()));
        let mut s = HashMap::<Variable, Term<Constant, Variable>>::new();
        s.insert(self.clone(), Term::Variable(renamed.clone()));
        (renamed, s)
    }
}

impl sld::Variable<Constant, Variable> for Variable {
    fn aux() -> Variable {
        let index = AVAILABLE_VARIABLE_INDEX.fetch_add(1, Ordering::SeqCst);
        Variable::Auxiliary(index)
    }
}

pub fn prove_goal(
    mf: &Modusfile,
    goal: &Vec<modusfile::Literal>,
) -> Result<Vec<sld::Proof<modusfile::Constant, modusfile::Variable>>, &'static str> {
    let max_depth = 20;
    let clauses: Vec<Clause<modusfile::Constant, modusfile::Variable>> =
        mf.0.iter().map(|mc| mc.into()).collect();

    let res = sld::sld(&clauses, goal, max_depth);
    match res {
        Some(t) => Ok(sld::proofs(&t, &clauses, &goal)),
        None => Err("Failed in SLD tree construction."),
    }
}

pub fn transpile(mf: Modusfile, query: modusfile::Literal) -> ResolvedDockerfile {
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

pub fn proof_to_docker(proof: sld::Proof<Constant, Variable>) -> ResolvedDockerfile {
    let mut instructions = Vec::new();
    fn dfs(
        proof: &sld::Proof<Constant, Variable>,
        instructions: &mut Vec<Instruction<ResolvedParent>>,
    ) {
        match &proof.clause {
            ClauseId::Builtin(lit) => match &lit.atom.0[..] {
                "run" => {
                    let arg = match lit.args[0].as_constant().unwrap() {
                        Constant::String(s) => s,
                        _ => panic!("Expected string"),
                    };
                    instructions.push(Instruction::Run(Run(arg.clone())));
                }
                "from" => {
                    let arg = match lit.args[0].as_constant().unwrap() {
                        Constant::String(s) => s,
                        _ => panic!("Expected string"),
                    };
                    instructions.push(Instruction::From(crate::dockerfile::From {
                        parent: ResolvedParent::Image(Image::from_str(arg).unwrap()),
                        alias: None,
                    }));
                }
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
