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

use nom::{
    bytes::complete::{tag_no_case},
    character::complete::{line_ending},
    branch::alt,
    sequence::{pair, terminated, preceded},
    multi::{many0},
    combinator::{map, eof},
    IResult
};

use crate::dockerfile::{
    Copy, Run, Env,
    copy_instr, run_instr, env_instr, mandatory_space, ignored_line
};
use crate::datalog::{
    DatalogRule,
    datalog_rule
};

#[derive(Clone, PartialEq, Debug)]
pub enum ModusInstruction {
    Rule(DatalogRule),
    Run(Run),
    Env(Env),
    Copy(Copy),
}

#[derive(Clone, PartialEq, Debug)]
pub struct Modusfile(pub Vec<ModusInstruction>);

impl str::FromStr for Modusfile {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match modusfile(s) {
            Result::Ok((_, o)) => Ok(o),
            Result::Err(e) => Result::Err(format!("{}", e)),
        }
    }
}

pub fn rule_instr(i: &str) -> IResult<&str, DatalogRule> {
    preceded(pair(tag_no_case("RULE"), mandatory_space), datalog_rule)(i)
}

fn modus_instruction(i: &str) -> IResult<&str, ModusInstruction> {
    alt((
        map(terminated(rule_instr, alt((line_ending, eof))), ModusInstruction::Rule),
        map(terminated(copy_instr, alt((line_ending, eof))), ModusInstruction::Copy),
        map(terminated(run_instr, alt((line_ending, eof))), ModusInstruction::Run),
        map(terminated(env_instr, alt((line_ending, eof))), ModusInstruction::Env)
    ))(i)
}

fn modusfile(i: &str) -> IResult<&str, Modusfile> {
    map(terminated(many0(preceded(many0(ignored_line), modus_instruction)), many0(ignored_line)), Modusfile)(i)
}


