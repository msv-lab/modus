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

use std::str;

use crate::modusfile::{
    Modusfile, ModusInstruction, ModusConstant, ModusRule, ModusLiteral, ModusTerm
};
use crate::dockerfile::{
    Dockerfile, DockerInstruction, From, ResolvedParent,
};
use crate::logic;
use logic::{
    Rule, Literal, Term
};


pub fn transpile(mf: Modusfile, query: Option<ModusLiteral>) -> Dockerfile<ResolvedParent> {
    let mut docker_instrs = vec![];
    for instr in mf.0 {
        match instr {
            ModusInstruction::Copy(c) => docker_instrs.push(DockerInstruction::Copy(c)),
            ModusInstruction::Run(r) => docker_instrs.push(DockerInstruction::Run(r)),
            ModusInstruction::Env(e) => docker_instrs.push(DockerInstruction::Env(e)),
            ModusInstruction::Workdir(e) => docker_instrs.push(DockerInstruction::Workdir(e)),
            ModusInstruction::Rule(ModusRule{head, body}) => {
                if !head.args.is_empty() {
                    panic!("head literals with parameters are not supported")
                }
                let stage_name = head.atom.0;
                if body.len() > 1 {
                    panic!("bodies with multiple literals are not supported")
                }
                let body_literal = &body[0];
                let from = {
                    let ModusLiteral { atom, args } = body_literal;
                    match (atom.0.as_str(), args.as_slice()) {
                        ("image", [Term::Constant(ModusConstant::Image(i))]) =>
                            From{ parent: ResolvedParent::Image(i.clone()), alias: Some(stage_name) },
                        (s, []) =>
                            From{ parent: ResolvedParent::Stage(s.into()), alias: Some(stage_name) },
                        _ => panic!("unsuppored body literal {}", body_literal)
                    }
                };
                docker_instrs.push(DockerInstruction::From(from))
            },
        }
    }
    Dockerfile(docker_instrs)
}