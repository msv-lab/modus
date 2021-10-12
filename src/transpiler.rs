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

use std::{collections::HashMap, sync::atomic::{AtomicU32, Ordering}, thread::panicking};

use nom::map;

use crate::{dockerfile, dockerfile::{
        Dockerfile, ResolvedParent,
    }, logic::{
        Clause, Literal, Term, Atom
    }, modusfile, modusfile::{Constant, Modusfile}, sld, unification::{Rename, Substitute, Substitution}};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Variable {
    User(String),
    Auxiliary(u32),
    Renamed(u32, Box<Variable>),
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
    let mut rules: Vec<Clause<Constant, Variable>> = Vec::new();
    let mut instructions: Vec<Vec<modusfile::Instruction>> = Vec::new();

    //TODO: image/1 should be builtin
    rules.push(Clause { head: Literal { atom: Atom("image".into()), 
                                        args: vec![ Term::Variable(Variable::User("X".into()))]}, 
                        body: vec![] });
    instructions.push(vec![]);

    let mut last_rule: Option<Clause<Constant, Variable>> = None;
    let mut last_stage: Vec<modusfile::Instruction> = Vec::new();
    for instr in &mf.0 {
        match instr {
            modusfile::Instruction::Rule(r) => {
                if last_rule.is_some() {
                    rules.push(last_rule.unwrap());
                    instructions.push(last_stage);
                }
                last_rule = Some(r.clone());
                last_stage = Vec::new();
            },
            _ => {
                last_stage.push(instr.clone());
            }
        }
    }
    if last_rule.is_some() {
        rules.push(last_rule.unwrap());
        instructions.push(last_stage);
    }

    let arbitrary_constant = 20;
    let goal = &vec![query];
    let result = sld::sld(&rules, &goal, arbitrary_constant);
    if result.is_none() {
        panic!("failed to resolve parameters")
    }
    let tree = result.unwrap();
    let proofs = sld::proofs(&tree, &rules, &goal);
    proofs_to_docker(&rules, &instructions, &proofs, &goal)
}

fn modus_to_docker(instr: &modusfile::Instruction) -> dockerfile::Instruction<ResolvedParent> {
    match instr {
        modusfile::Instruction::Copy(c) =>
            dockerfile::Instruction::Copy(c.clone()),
        modusfile::Instruction::Run(r) =>
            dockerfile::Instruction::Run(r.clone()),
        modusfile::Instruction::Env(e) =>
            dockerfile::Instruction::Env(e.clone()),
        modusfile::Instruction::Workdir(e) =>
            dockerfile::Instruction::Workdir(e.clone()),
        _ => panic!("unsupported instruction")
    }
}

static AVAILABLE_STAGE_INDEX: AtomicU32 = AtomicU32::new(0);

fn proof_to_docker(rules: &Vec<Clause<Constant, Variable>>,
                   instructions: &Vec<Vec<modusfile::Instruction>>,
                   proof: &sld::Proof<Constant, Variable>,
                   docker_instrs: &mut Vec<dockerfile::Instruction<ResolvedParent>>,
                   cache: &mut HashMap<sld::Goal<Constant, Variable>, ResolvedParent>,
                   goal: &sld::Goal<Constant, Variable>) -> ResolvedParent {
    let solution: sld::Goal<Constant, Variable> = goal.substitute(&proof.valuation);
    if cache.contains_key(&solution) {
        return cache.get(&solution).unwrap().clone();
    }
    let index = AVAILABLE_STAGE_INDEX.fetch_add(1, Ordering::SeqCst);
    let name = format!("stage_{}", index);
    let dependencies: Vec<ResolvedParent> = proof.children.iter().map(|subproof| match subproof.clause {
        sld::ClauseId::Rule(0) => {
            let value = subproof.valuation.get(&Variable::User("X".into())).unwrap();
            match value {
                Term::Constant(Constant::String(s)) => ResolvedParent::Image(s.parse().unwrap()),
                _ => unreachable!()
            }
        },
        sld::ClauseId::Rule(_) => proof_to_docker(rules, instructions, subproof, docker_instrs, cache, goal),
        _ => unreachable!() 
    }).collect();
    let from = dockerfile::From{ parent: dependencies[0].clone(), alias: Some(name.clone()) };
    docker_instrs.push(dockerfile::Instruction::From(from));
    for (k, v) in &proof.valuation {
        match (k, v) {
            (Variable::User(n), Term::Constant(Constant::String(s))) => {
                let a = dockerfile::Arg(format!("{}=\"{}\"", n, s));
                docker_instrs.push(dockerfile::Instruction::Arg(a))
            },
            _ => unreachable!()
        }
    }
    match proof.clause {
        sld::ClauseId::Rule(rid) if rid > 0 => {
            docker_instrs.append(&mut instructions[rid].iter().map(modus_to_docker).collect());
        },
        sld::ClauseId::Query => (),
        _ => unreachable!()
    }
    
    ResolvedParent::Stage(name)
}

fn proofs_to_docker(rules: &Vec<Clause<Constant, Variable>>,
                    instructions: &Vec<Vec<modusfile::Instruction>>,
                    proofs: &Vec<sld::Proof<Constant, Variable>>,
                    goal: &sld::Goal<Constant, Variable>) -> Dockerfile<ResolvedParent> {
    let mut docker_instrs: Vec<dockerfile::Instruction<ResolvedParent>> = vec![];

    let mut cache: HashMap<sld::Goal<Constant, Variable>, ResolvedParent> = HashMap::new();

    for p in proofs {
        let _ = proof_to_docker(rules, instructions, &p, &mut docker_instrs, &mut cache, &goal);
    }

    Dockerfile(docker_instrs)                  
}
