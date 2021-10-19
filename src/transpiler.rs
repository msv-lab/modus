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
    sync::atomic::{AtomicU32, Ordering},
    thread::panicking,
};

use nom::map;

use crate::{
    dockerfile,
    dockerfile::{Dockerfile, ResolvedParent},
    logic::{Atom, Clause, Literal, Term},
    modusfile,
    modusfile::{Constant, Modusfile},
    sld,
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

pub fn transpile(mf: Modusfile, query: modusfile::Literal) -> Dockerfile<ResolvedParent> {
    todo!()
}

static AVAILABLE_STAGE_INDEX: AtomicU32 = AtomicU32::new(0);

fn proof_to_docker(
    rules: &Vec<Clause<Constant, Variable>>,
    proof: &sld::Proof<Constant, Variable>,
    docker_instrs: &mut Vec<dockerfile::Instruction<ResolvedParent>>,
    cache: &mut HashMap<sld::Goal<Constant, Variable>, ResolvedParent>,
    goal: &sld::Goal<Constant, Variable>,
) -> ResolvedParent {
    todo!()
}

fn proofs_to_docker(
    rules: &Vec<Clause<Constant, Variable>>,
    proofs: &Vec<sld::Proof<Constant, Variable>>,
    goal: &sld::Goal<Constant, Variable>,
) -> Dockerfile<ResolvedParent> {
    todo!()
}
