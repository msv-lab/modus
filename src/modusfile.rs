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
use std::fmt;

use nom::{
    bytes::complete::{tag_no_case, tag},
    character::complete::{line_ending, none_of, alpha1, alphanumeric1},
    branch::alt,
    sequence::{pair, terminated, preceded, delimited},
    multi::{many0},
    combinator::{map, eof, recognize},
    IResult
};

use crate::dockerfile::{
    Copy, Run, Env, Workdir,
    copy_instr, run_instr, env_instr, workdir_instr,
    mandatory_space, ignored_line
};
use crate::values::{
    Text, Image,
    image_literal
};
use crate::datalog;
use datalog::{
    Rule, Literal, Term
};


#[derive(Clone, PartialEq, Debug)]
pub struct ModusVariable(String);

#[derive(Clone, PartialEq, Debug)]
pub enum ModusConstant {
    Text(Text),
    Image(Image)
}

pub type ModusRule = Rule<ModusConstant, ModusVariable>;
pub type ModusLiteral = Literal<ModusConstant, ModusVariable>;
pub type ModusTerm = Term<ModusConstant, ModusVariable>;

#[derive(Clone, PartialEq, Debug)]
pub enum ModusInstruction {
    Rule(ModusRule),
    Run(Run),
    Env(Env),
    Copy(Copy),
    Workdir(Workdir)
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

impl str::FromStr for ModusRule {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match datalog::rule(modus_const, modus_var)(s) {
            Result::Ok((_, o)) => Ok(o),
            Result::Err(e) => Result::Err(format!("{}", e)),
        }
    }
}

impl str::FromStr for ModusLiteral {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match datalog::literal(modus_const, modus_var)(s) {
            Result::Ok((_, o)) => Ok(o),
            Result::Err(e) => Result::Err(format!("{}", e)),
        }
    }
}

impl fmt::Display for ModusConstant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ModusConstant::Image(i) => write!(f, "i\"{}\"", i),
            ModusConstant::Text(s) => write!(f, "\"{}\"", s)
        }
    }
}

impl fmt::Display for ModusVariable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/", self.0)
    }
}

//TODO: support proper string literals
fn string_content(i: &str) -> IResult<&str, &str> {
    recognize(many0(none_of("\\\"")))(i)
}

fn modus_const(i: &str) -> IResult<&str, ModusConstant> {
    alt((
        map(delimited(tag("\""), string_content, tag("\"")),
            |s| ModusConstant::Text(Text(s.into()))),
        map(image_literal,
            |s| ModusConstant::Image(s))
    ))(i)
}

//TODO: I need to think more carefully how to connect this to ARGs
pub fn variable_identifier(i: &str) -> IResult<&str, &str> {
    recognize(
      pair(
        alpha1,
        many0(alt((alphanumeric1, tag("_"))))
      )
    )(i)
}

fn modus_var(i: &str) -> IResult<&str, ModusVariable> {
    map(variable_identifier, |s| ModusVariable(s.into()))(i)
}

pub fn rule_instr(i: &str) -> IResult<&str, ModusRule> {
    preceded(pair(tag_no_case("RULE"), mandatory_space), datalog::rule(modus_const, modus_var))(i)
}

fn modus_instruction(i: &str) -> IResult<&str, ModusInstruction> {
    alt((
        map(terminated(rule_instr, alt((line_ending, eof))), ModusInstruction::Rule),
        map(terminated(copy_instr, alt((line_ending, eof))), ModusInstruction::Copy),
        map(terminated(run_instr, alt((line_ending, eof))), ModusInstruction::Run),
        map(terminated(env_instr, alt((line_ending, eof))), ModusInstruction::Env),
        map(terminated(workdir_instr, alt((line_ending, eof))), ModusInstruction::Workdir)
    ))(i)
}

fn modusfile(i: &str) -> IResult<&str, Modusfile> {
    map(terminated(many0(preceded(many0(ignored_line), modus_instruction)), many0(ignored_line)), Modusfile)(i)
}


