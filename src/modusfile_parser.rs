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

use crate::dockerfile_parser;
use crate::datalog_parser;

use nom::{
    bytes::complete::{is_not, tag, tag_no_case},
    character::complete::{
        char,
        alpha1,
        alphanumeric1,
        space0,
        space1,
        line_ending,
        not_line_ending,
        one_of
    },
    branch::alt,
    sequence::{pair, delimited, terminated, preceded, tuple},
    multi::{many0, many1, separated_list1},
    combinator::{value, recognize, map, opt, eof},
    error::ParseError,
    IResult
};

use dockerfile_parser::{Copy, Run, Env};
use dockerfile_parser::{copy_instr, run_instr, env_instr, mandatory_space, ignored_line};
use datalog_parser::{DatalogLiteral, DatalogRule, DatalogTerm};
use datalog_parser::{rule};


#[derive(Clone, PartialEq, Debug)]
pub enum ModusInstruction {
    Rule(DatalogRule),
    Run(Run),
    Env(Env),
    Copy(Copy),
}

#[derive(Clone, PartialEq, Debug)]
pub struct Modusfile(Vec<ModusInstruction>);

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
    preceded(pair(tag_no_case("RULE"), mandatory_space), rule)(i)
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


