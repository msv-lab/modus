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

use crate::dockerfile;
use dockerfile::{
    Copy, Run, Env, Workdir
};
use crate::logic;
use crate::typing;

#[derive(Clone, PartialEq, Debug)]
pub enum Constant {
    String(String),
    Integer(u32)  //TODO: arbitrary-precision arithmetic?
}

pub type Rule = logic::Rule<Constant, String>;
pub type Literal = logic::Literal<Constant, String>;
pub type Term = logic::Term<Constant, String>;

#[derive(Clone, PartialEq, Debug)]
pub enum Instruction {
    Rule(Rule),
    Run(Run),
    Env(Env),
    Copy(Copy),
    Workdir(Workdir)
}

#[derive(Clone, PartialEq, Debug)]
pub struct Modusfile(pub Vec<Instruction>);

#[derive(Clone, PartialEq, Debug)]
pub struct Version {
    major: u32,
    minor: u32,
    patch: u32,
    pre_release: String,
    build: String,
}


impl str::FromStr for Modusfile {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parser::modusfile(s) {
            Result::Ok((_, o)) => Ok(o),
            Result::Err(e) => Result::Err(format!("{}", e)),
        }
    }
}

impl str::FromStr for Rule {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match logic::parser::rule(parser::modus_const, parser::modus_var)(s) {
            Result::Ok((_, o)) => Ok(o),
            Result::Err(e) => Result::Err(format!("{}", e)),
        }
    }
}

impl str::FromStr for Literal {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match logic::parser::literal(parser::modus_const, parser::modus_var)(s) {
            Result::Ok((_, o)) => Ok(o),
            Result::Err(e) => Result::Err(format!("{}", e)),
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::String(s) => write!(f, "i\"{}\"", s),
            Constant::Integer(i) => write!(f, "{}", i)
        }
    }
}

mod parser {
    use super::*;
    use dockerfile::Image;

    use nom::{
        IResult,
        branch::alt,
        bytes::complete::{tag_no_case, tag},
        character::complete::{line_ending, none_of, alpha1, alphanumeric1},
        combinator::{eof, map, peek, recognize},
        multi::{many0},
        sequence::{pair, terminated, preceded, delimited}
    };
   

    //TODO: support proper string literals
    fn string_content(i: &str) -> IResult<&str, &str> {
        recognize(many0(none_of("\\\"")))(i)
    }

    pub fn image_literal(i: &str) -> IResult<&str, Image> {
        delimited(tag("i\""), dockerfile::parser::image, tag("\""))(i)
    }

    pub fn modus_const(i: &str) -> IResult<&str, Constant> {
        alt((
            map(delimited(tag("\""), string_content, tag("\"")),
                |s| Constant::String(s.into())),
            map(image_literal,
                |s| Constant::String(s.to_string())) //TODO: Need to construct a compound object
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

    pub fn modus_var(i: &str) -> IResult<&str, String> {
        map(variable_identifier, String::from)(i)
    }

    pub fn rule_instr(i: &str) -> IResult<&str, Rule> {
        preceded(pair(tag_no_case("RULE"), dockerfile::parser::mandatory_space),
                logic::parser::rule(modus_const, modus_var))(i)
    }

    //TODO: a parsing rule for an instruction should be extracted into a combinator
    fn modus_instruction(i: &str) -> IResult<&str, Instruction> {
        alt((
            map(terminated(rule_instr, alt((line_ending, peek(eof)))), Instruction::Rule),
            map(terminated(dockerfile::parser::copy_instr, alt((line_ending, peek(eof)))), Instruction::Copy),
            map(terminated(dockerfile::parser::run_instr, alt((line_ending, peek(eof)))), Instruction::Run),
            map(terminated(dockerfile::parser::env_instr, alt((line_ending, peek(eof)))), Instruction::Env),
            map(terminated(dockerfile::parser::workdir_instr, alt((line_ending, peek(eof)))), Instruction::Workdir)
        ))(i)
    }

    pub fn modusfile(i: &str) -> IResult<&str, Modusfile> {
        map(terminated(many0(preceded(many0(dockerfile::parser::ignored_line), modus_instruction)),
                    terminated(many0(dockerfile::parser::ignored_line), eof)), Modusfile)(i)
    }

}


