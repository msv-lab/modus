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

use nom::character::complete::line_ending;
use nom::character::complete::not_line_ending;
use std::fmt;
use std::str;

use crate::dockerfile;
use crate::logic;

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    Literal(Literal),
    OperatorApplication(Vec<Expression>, Operator),
}

#[derive(Clone, PartialEq, Debug)]
pub struct ModusClause {
    pub head: Literal,
    pub body: Vec<Expression>,
}

impl From<&crate::modusfile::ModusClause> for logic::Clause {
    fn from(modus_clause: &crate::modusfile::ModusClause) -> Self {
        fn get_literals(expr: &Expression) -> Vec<logic::Literal> {
            match expr {
                Expression::Literal(l) => vec![l.clone()],
                // for now, ignore operators
                Expression::OperatorApplication(exprs, _) => {
                    exprs.iter().flat_map(|e| get_literals(e)).collect()
                }
            }
        }
        let literals = modus_clause
            .body
            .iter()
            .flat_map(|e| get_literals(e))
            .collect();
        Self {
            head: modus_clause.head.clone(),
            body: literals,
        }
    }
}

type ModusTerm = logic::IRTerm;
type Literal = logic::Literal<ModusTerm>;
type Fact = ModusClause;
type Rule = ModusClause;
pub type Operator = Literal;

#[derive(Clone, PartialEq, Debug)]
pub struct Modusfile(pub Vec<ModusClause>);

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

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::OperatorApplication(exprs, op) => write!(
                f,
                "({})::{}",
                exprs
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join(", "),
                op
            ),
            Expression::Literal(l) => write!(f, "{}", l.to_string()),
        }
    }
}

// could write a macro that generates these
impl From<Literal> for Expression {
    fn from(l: Literal) -> Self {
        Expression::Literal(l)
    }
}

impl str::FromStr for ModusClause {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parser::modus_clause(s) {
            Result::Ok((_, o)) => Ok(o),
            Result::Err(e) => Result::Err(format!("{}", e)),
        }
    }
}

impl fmt::Display for ModusClause {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.body.len() {
            0 => write!(f, "{}.", self.head),
            _ => write!(
                f,
                "{} :- {}.",
                self.head,
                self.body
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
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

pub mod parser {
    use crate::logic::parser::{literal, literal_identifier};

    use super::*;

    use nom::character::complete::multispace0;
    use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::{none_of, space0},
        combinator::{eof, map, recognize},
        multi::{many0, separated_list0},
        sequence::{delimited, preceded, separated_pair, terminated},
        IResult,
    };

    fn comment(s: &str) -> IResult<&str, &str> {
        delimited(tag("#"), not_line_ending, line_ending)(s)
    }

    fn head(i: &str) -> IResult<&str, Literal> {
        literal(modus_const, modus_var)(i)
    }

    fn expression(i: &str) -> IResult<&str, Expression> {
        alt((
            map(literal(modus_const, modus_var), |lit| {
                Expression::Literal(lit)
            }),
            map(
                separated_pair(
                    // implicit recursion here
                    delimited(tag("("), body, tag(")")),
                    tag("::"),
                    literal(modus_const, modus_var),
                ),
                |(exprs, operator)| Expression::OperatorApplication(exprs, operator),
            ),
        ))(i)
    }

    /// Comma-separated list of expressions, interspersed with comments.
    fn body(i: &str) -> IResult<&str, Vec<Expression>> {
        preceded(
            delimited(
                multispace0,
                separated_list0(multispace0, comment),
                multispace0,
            ),
            separated_list0(
                delimited(
                    multispace0,
                    tag(","),
                    delimited(
                        multispace0,
                        separated_list0(multispace0, comment),
                        multispace0,
                    ),
                ),
                expression,
            ),
        )(i)
    }

    fn fact(i: &str) -> IResult<&str, ModusClause> {
        // Custom definition of fact since datalog facts are normally "head :- ", but Moduslog
        // defines it as "head."
        map(terminated(head, tag(".")), |h| ModusClause {
            head: h,
            body: Vec::new(),
        })(i)
    }

    fn rule(i: &str) -> IResult<&str, ModusClause> {
        map(
            separated_pair(
                head,
                delimited(space0, tag(":-"), multispace0),
                terminated(body, tag(".")),
            ),
            |(head, body)| ModusClause { head, body },
        )(i)
    }

    // TODO: support proper string literals + format strings
    fn string_content(i: &str) -> IResult<&str, &str> {
        recognize(many0(none_of("\\\"")))(i)
    }

    pub fn modus_const(i: &str) -> IResult<&str, &str> {
        // TODO: don't treat f-strings as const
        delimited(alt((tag("\""), tag("f\""))), string_content, tag("\""))(i)
    }

    pub fn variable_identifier(i: &str) -> IResult<&str, &str> {
        literal_identifier(i)
    }

    pub fn modus_var(i: &str) -> IResult<&str, &str> {
        variable_identifier(i)
    }

    pub fn modus_clause(i: &str) -> IResult<&str, ModusClause> {
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
            atom: logic::Predicate("l1".into()),
            args: Vec::new(),
        };
        let c = ModusClause {
            head: l1,
            body: Vec::new(),
        };
        assert_eq!("l1.", c.to_string());
        assert_eq!(Ok(c), "l1.".parse());
    }

    #[test]
    fn rule() {
        let l1 = Literal {
            atom: logic::Predicate("l1".into()),
            args: Vec::new(),
        };
        let l2 = Literal {
            atom: logic::Predicate("l2".into()),
            args: Vec::new(),
        };
        let l3 = Literal {
            atom: logic::Predicate("l3".into()),
            args: Vec::new(),
        };
        let c = Rule {
            head: l1,
            body: vec![l2.into(), l3.into()],
        };
        assert_eq!("l1 :- l2, l3.", c.to_string());
        assert_eq!(Ok(c.clone()), "l1 :- l2, l3.".parse());
        assert_eq!(Ok(c.clone()), "l1 :- l2,\n\tl3.".parse());
    }

    #[test]
    fn rule_with_operator() {
        let foo = Literal {
            atom: logic::Predicate("foo".into()),
            args: Vec::new(),
        };
        let a = Literal {
            atom: logic::Predicate("a".into()),
            args: Vec::new(),
        };
        let b = Literal {
            atom: logic::Predicate("b".into()),
            args: Vec::new(),
        };
        let merge = Operator {
            atom: logic::Predicate("merge".into()),
            args: Vec::new(),
        };
        let r = Rule {
            head: foo,
            body: vec![Expression::OperatorApplication(
                vec![a.into(), b.into()],
                merge,
            )],
        };
        assert_eq!("foo :- (a, b)::merge.", r.to_string());
        assert_eq!(Ok(r.clone()), "foo :- (a, b)::merge.".parse());
    }

    #[test]
    fn modusclause_to_clause() {
        let foo = Literal {
            atom: logic::Predicate("foo".into()),
            args: Vec::new(),
        };
        let a = Literal {
            atom: logic::Predicate("a".into()),
            args: Vec::new(),
        };
        let b = Literal {
            atom: logic::Predicate("b".into()),
            args: Vec::new(),
        };
        let merge = Operator {
            atom: logic::Predicate("merge".into()),
            args: Vec::new(),
        };
        let r = Rule {
            head: foo,
            body: vec![Expression::OperatorApplication(
                vec![a.into(), b.into()],
                merge,
            )],
        };
        assert_eq!("foo :- (a, b)::merge.", r.to_string());

        // Convert to the simpler syntax
        let c: logic::Clause = (&r).into();
        assert_eq!("foo :- a, b", c.to_string());
    }
}
