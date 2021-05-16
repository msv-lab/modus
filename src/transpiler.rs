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

use std::{collections::HashMap, sync::atomic::{AtomicU32, Ordering}};

use crate::{
    dockerfile,
    dockerfile::{
        Dockerfile, ResolvedParent,
    },
    logic::{
        Clause, Literal, Term
    },
    modusfile,
    modusfile::{Constant, Modusfile},
    sld,
    unification::{Rename, Substitution}
};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
enum Variable {
    User(String),
    Auxiliary(u32),
    Renamed(u32, Box<Variable>),
}

static AVAILABLE_INDEX: AtomicU32 = AtomicU32::new(0);

impl Rename<Constant, Variable> for Variable {
    type Output = Variable;
    fn rename(&self) -> (Variable, Substitution<Constant, Variable>) {
        let index = AVAILABLE_INDEX.fetch_add(1, Ordering::SeqCst);
        let renamed = Variable::Renamed(index, Box::new((*self).clone()));
        let mut s = HashMap::<Variable, Term<Constant, Variable>>::new();
        s.insert(self.clone(), Term::Variable(renamed.clone()));
        (renamed, s)
    }
}

impl sld::Variable<Constant, Variable> for Variable {
    fn aux() -> Variable {
        let index = AVAILABLE_INDEX.fetch_add(1, Ordering::SeqCst);
        Variable::Auxiliary(index)
    }
}

pub fn transpile(mf: Modusfile, query: Option<modusfile::Literal>) -> Dockerfile<ResolvedParent> {
    let mut docker_instrs = vec![];
    for instr in mf.0 {
        match instr {
            modusfile::Instruction::Copy(c) =>
                docker_instrs.push(dockerfile::Instruction::Copy(c)),
            modusfile::Instruction::Run(r) =>
                docker_instrs.push(dockerfile::Instruction::Run(r)),
            modusfile::Instruction::Env(e) =>
                docker_instrs.push(dockerfile::Instruction::Env(e)),
            modusfile::Instruction::Workdir(e) =>
                docker_instrs.push(dockerfile::Instruction::Workdir(e)),
            modusfile::Instruction::Rule(Clause{head, body}) => {
                if !head.args.is_empty() {
                    panic!("head literals with parameters are not supported")
                }
                let stage_name = head.atom.0;
                if body.len() > 1 {
                    panic!("bodies with multiple literals are not supported")
                }
                let body_literal = &body[0];
                let from = {
                    let Literal { atom, args } = body_literal;
                    match (atom.0.as_str(), args.as_slice()) {
                        ("image", [Term::Constant(Constant::String(s))]) =>
                            dockerfile::From{ parent: ResolvedParent::Image(s.parse().unwrap()), alias: Some(stage_name) },
                        (s, []) =>
                            dockerfile::From{ parent: ResolvedParent::Stage(s.into()), alias: Some(stage_name) },
                        _ => panic!("unsuppored body literal {}", body_literal)
                    }
                };
                docker_instrs.push(dockerfile::Instruction::From(from))
            },
        }
    }
    Dockerfile(docker_instrs)
}