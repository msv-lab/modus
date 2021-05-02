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

use crate::modusfile::{
    Modusfile, ModusInstruction
};
use crate::dockerfile::{
    Dockerfile, DockerInstruction, From, Parent
};
use crate::datalog::{
    DatalogRule, DatalogLiteral, DatalogTerm
};


pub fn transpile(mf: Modusfile, query: Option<DatalogLiteral>) -> Dockerfile {
    let mut docker_instrs = vec![];
    for instr in mf.0 {
        match instr {
            ModusInstruction::Copy(c) => docker_instrs.push(DockerInstruction::Copy(c)),
            ModusInstruction::Run(r) => docker_instrs.push(DockerInstruction::Run(r)),
            ModusInstruction::Env(e) => docker_instrs.push(DockerInstruction::Env(e)),
            ModusInstruction::Rule(DatalogRule{head, body}) => {
                if head.args.is_empty() {
                    panic!("head literals with parameters are not supported")
                }
                let stage_name = head.name;
                if body.len() > 1 {
                    panic!("bodies with multiple literals are not supported")
                }
                let body_literal = &body[0];
                let from = {
                    let DatalogLiteral { name, args } = body_literal;
                    match (name.as_str(), args.as_slice()) {
                        ("image", [DatalogTerm::Constant(s)]) =>
                            From{ parent: Parent::Image(s.parse().unwrap()), alias: Some(stage_name) },
                        (s, []) =>
                            From{ parent: Parent::Stage(s.into()), alias: Some(stage_name) },
                        _ => panic!("unsuppored body literal {}", body_literal)
                    }
                };
                docker_instrs.push(DockerInstruction::From(from))
            },
        }
    }
    Dockerfile(docker_instrs)
}