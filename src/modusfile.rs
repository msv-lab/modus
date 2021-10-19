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

use fp_core::compose::compose_two;
use std::fmt;
use std::str;

use crate::logic;
use crate::modusfile::parser::modus_const;
use crate::{dockerfile, transpiler};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Constant {
    String(String),
    Integer(u32), //TODO: arbitrary-precision arithmetic?
}

pub type Clause = logic::Clause<Constant, transpiler::Variable>;
pub type Fact = Clause;
pub type Rule = Clause;
pub type Literal = logic::Literal<Constant, transpiler::Variable>;
pub type Term = logic::Term<Constant, transpiler::Variable>;

#[derive(Clone, PartialEq, Debug)]
pub struct Modusfile(pub Vec<Clause>);

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

impl str::FromStr for Clause {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parser::modus_clause(s) {
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
            Constant::String(s) => write!(f, "\"{}\"", s),
            Constant::Integer(i) => write!(f, "{}", i),
        }
    }
}

mod parser {
    use crate::logic::parser::{literal, literal_identifier};
    use crate::transpiler;

    use super::*;

    use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::{none_of, space0},
        combinator::{eof, map, recognize},
        multi::{many0, separated_list0},
        sequence::{delimited, preceded, separated_pair, terminated},
        IResult,
    };

    fn head(i: &str) -> IResult<&str, Literal> {
        literal(modus_const, modus_var)(i)
    }

    fn body(i: &str) -> IResult<&str, Vec<Literal>> {
        // TODO: Implement expression bodies (using an expression tree?)
        separated_list0(
            delimited(space0, tag(","), space0),
            literal(modus_const, modus_var),
        )(i)
    }

    fn fact(i: &str) -> IResult<&str, Clause> {
        // Custom definition of fact since datalog facts are normally "head :- ", but Moduslog
        // defines it as "head."
        map(terminated(head, tag(".")), |h| Clause {
            head: h,
            body: Vec::new(),
        })(i)
    }

    fn rule(i: &str) -> IResult<&str, Clause> {
        map(
            separated_pair(
                head,
                delimited(space0, tag(":-"), space0),
                terminated(body, tag(".")),
            ),
            |(head, body)| Clause { head, body },
        )(i)
    }

    //TODO: support proper string literals
    fn string_content(i: &str) -> IResult<&str, &str> {
        recognize(many0(none_of("\\\"")))(i)
    }

    pub fn modus_const(i: &str) -> IResult<&str, Constant> {
        map(delimited(tag("\""), string_content, tag("\"")), |s| {
            Constant::String(s.into())
        })(i)
    }

    pub fn variable_identifier(i: &str) -> IResult<&str, &str> {
        literal_identifier(i)
    }

    pub fn modus_var(i: &str) -> IResult<&str, transpiler::Variable> {
        map(
            variable_identifier,
            compose!(String::from, transpiler::Variable::User),
        )(i)
    }

    pub fn modus_clause(i: &str) -> IResult<&str, Clause> {
        alt((fact, rule))(i)
    }

    pub fn modusfile(i: &str) -> IResult<&str, Modusfile> {
        map(
            terminated(
                many0(preceded(
                    many0(dockerfile::parser::ignored_line),
                    modus_clause,
                )),
                terminated(many0(dockerfile::parser::ignored_line), eof),
            ),
            Modusfile,
        )(i)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fact() {
        let l1 = Literal {
            atom: logic::Atom("l1".into()),
            args: Vec::new(),
        };
        let c = Clause {
            head: l1,
            body: Vec::new(),
        };
        assert_eq!("l1.", c.to_string());
        assert_eq!(Ok(c), "l1.".parse());
    }

    #[test]
    fn rule() {
        let l1 = Literal {
            atom: logic::Atom("l1".into()),
            args: Vec::new(),
        };
        let l2 = Literal {
            atom: logic::Atom("l2".into()),
            args: Vec::new(),
        };
        let c = Rule {
            head: l1,
            body: vec![l2],
        };
        assert_eq!("l1 :- l2.", c.to_string());
        assert_eq!(Ok(c), "l1 :- l2.".parse());
    }
}
