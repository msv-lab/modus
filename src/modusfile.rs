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
use nom::error::convert_error;
use std::fmt;
use std::str;

use crate::dockerfile;
use crate::logic;

#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    Literal(Literal),

    // An operator applied to an expression
    OperatorApplication(Box<Expression>, Operator),

    // A conjunction of expressions.
    And(Box<Expression>, Box<Expression>),

    // A disjunction of expressions.
    Or(Box<Expression>, Box<Expression>),
}

#[derive(Clone, PartialEq, Debug)]
pub struct ModusClause {
    pub head: Literal,
    // If None, this clause is a fact.
    pub body: Option<Expression>,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum ModusTerm {
    Constant(String),
    /// A format string with '\$' left unhandled. This should be dealt with when
    /// converting to the IR.
    FormatString(String),
    UserVariable(String),
}

impl From<ModusTerm> for logic::IRTerm {
    fn from(modus_term: ModusTerm) -> Self {
        match modus_term {
            ModusTerm::Constant(c) => logic::IRTerm::Constant(c),
            // TODO: return a Result type and propogate the error up properly if we want to warn about
            // this. Example: the user might try to use a format string in a head literal.
            ModusTerm::FormatString(_) => panic!("Cannot convert a format string to an IRTerm."),
            ModusTerm::UserVariable(v) => logic::IRTerm::UserVariable(v),
        }
    }
}

impl fmt::Display for logic::Literal<ModusTerm> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

type Literal = logic::Literal<ModusTerm>;

impl From<Literal> for logic::Literal {
    fn from(modus_literal: Literal) -> Self {
        Self {
            predicate: modus_literal.predicate,
            args: modus_literal
                .args
                .into_iter()
                .map(|arg| arg.into())
                .collect(),
        }
    }
}

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
            Result::Err(nom::Err::Error(e) | nom::Err::Failure(e)) => {
                Result::Err(convert_error(s, e))
            }
            _ => unimplemented!(),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::OperatorApplication(expr, op) => {
                write!(f, "({})::{}", expr.to_string(), op)
            }
            Expression::Literal(l) => write!(f, "{}", l.to_string()),
            Expression::And(expr1, expr2) => {
                // Explicit parenthesization when printing to output, looks a bit
                // verbose but shouldn't affect user code.
                write!(f, "({}, {})", expr1, expr2)
            }
            Expression::Or(expr1, expr2) => {
                write!(f, "({}; {})", expr1, expr2)
            }
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
        if let Some(e) = &self.body {
            write!(f, "{} :- {}.", self.head, e.to_string(),)
        } else {
            write!(f, "{}.", self.head)
        }
    }
}

impl str::FromStr for Literal {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match logic::parser::literal(parser::modus_term)(s) {
            Result::Ok((_, o)) => Ok(o),
            Result::Err(e) => Result::Err(format!("{}", e)),
        }
    }
}

pub mod parser {
    use crate::logic::parser::{literal, literal_identifier, IResult};

    use super::*;

    use nom::bytes::complete::is_not;
    use nom::character::complete::multispace0;
    use nom::combinator::cut;
    use nom::error::context;
    use nom::multi::{fold_many0, separated_list1};
    use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::space0,
        combinator::{eof, map, recognize},
        multi::{many0, separated_list0},
        sequence::{delimited, preceded, separated_pair, terminated},
    };

    fn comment(s: &str) -> IResult<&str, &str> {
        delimited(tag("#"), not_line_ending, line_ending)(s)
    }

    fn comments(s: &str) -> IResult<&str, Vec<&str>> {
        delimited(
            multispace0,
            separated_list0(multispace0, comment),
            multispace0,
        )(s)
    }

    fn head(i: &str) -> IResult<&str, Literal> {
        literal(modus_term)(i)
    }

    fn expression_inner(i: &str) -> IResult<&str, Expression> {
        let lit_parser = map(literal(modus_term), |lit| Expression::Literal(lit));
        // These inner expression parsers can fully recurse.
        let op_application_parser = map(
            separated_pair(
                delimited(tag("("), body, cut(tag(")"))),
                tag("::"),
                cut(literal(modus_term)),
            ),
            |(expr, operator)| Expression::OperatorApplication(Box::new(expr), operator),
        );
        let parenthesized_expr = delimited(tag("("), body, cut(tag(")")));
        alt((lit_parser, op_application_parser, parenthesized_expr))(i)
    }

    fn body(i: &str) -> IResult<&str, Expression> {
        let comma_separated_exprs = map(
            separated_list1(delimited(comments, tag(","), comments), expression_inner),
            |es| {
                es.into_iter()
                    .reduce(|e1, e2| Expression::And(Box::new(e1), Box::new(e2)))
                    .expect("Converting list to expression pairs.")
            },
        );
        let semi_separated_exprs = map(
            separated_list1(
                delimited(comments, tag(";"), comments),
                comma_separated_exprs,
            ),
            |es| {
                es.into_iter()
                    .reduce(|e1, e2| Expression::Or(Box::new(e1), Box::new(e2)))
                    .expect("Converting list to expression pairs.")
            },
        );
        // Parses the body as a semicolon separated list of comma separated inner expressions.
        // This resolves ambiguity by making commas/and higher precedence.
        preceded(comments, semi_separated_exprs)(i)
    }

    fn fact(i: &str) -> IResult<&str, ModusClause> {
        // Custom definition of fact since datalog facts are normally "head :- ", but Moduslog
        // defines it as "head."
        map(terminated(head, tag(".")), |h| ModusClause {
            head: h,
            body: None,
        })(i)
    }

    fn rule(i: &str) -> IResult<&str, ModusClause> {
        map(
            separated_pair(
                head,
                delimited(space0, tag(":-"), multispace0),
                cut(terminated(body, tag("."))),
            ),
            |(head, body)| ModusClause {
                head,
                body: Some(body),
            },
        )(i)
    }

    /// Processes the given string, converting escape substrings into the proper characters.
    ///
    /// This also supports string continuation, This allows users to write strings like: "Hello, \
    ///                                                                                 World!"
    /// which is actually just "Hello, World!".
    fn process_raw_string(s: &str) -> String {
        let mut processed = String::new();

        let mut chars = s.chars().peekable();
        while let Some(c) = chars.next() {
            if c == '\\' {
                match chars.next() {
                    Some('"') => processed.push('"'),
                    Some('\\') => processed.push('\\'),
                    Some('n') => processed.push('\n'),
                    Some('r') => processed.push('\r'),
                    Some('t') => processed.push('\t'),
                    Some('0') => processed.push('\0'),
                    Some('\n') => {
                        // string continuation so we'll ignore whitespace till we get to a non-whitespace.
                        while let Some(c) = chars.peek() {
                            if !c.is_whitespace() {
                                break;
                            }
                            chars.next();
                        }
                    }
                    Some(c) => {
                        // leave it unchanged if we don't recognize the escape char
                        processed.push('\\');
                        processed.push(c);
                    }
                    None => panic!("given string ends with an escape character"),
                }
            } else {
                processed.push(c);
            }
        }
        processed
    }

    fn string_content(i: &str) -> IResult<&str, String> {
        let (a, b) = recognize(fold_many0(
            // Either an escaped double quote or anything that's not a double quote.
            // It should try the escaped double quote first.
            alt((tag("\\\""), is_not("\""))),
            || "".to_string(),
            |a, b| a + b,
        ))(i)?;
        let s = process_raw_string(b);
        Ok((a, s))
    }

    pub fn modus_const(i: &str) -> IResult<&str, String> {
        delimited(tag("\""), string_content, cut(tag("\"")))(i)
    }

    /// Parses a substring outside of the expansion part of a format string's content.
    pub fn outside_format_expansion(i: &str) -> IResult<&str, String> {
        fold_many0(
            alt((tag("\\$"), is_not("$"))),
            || "".to_owned(),
            |a, b| a + b,
        )(i)
    }

    fn modus_format_string(i: &str) -> IResult<&str, String> {
        // TODO: check that the token(s) inside "${...}" conform to the variable syntax.
        // Note that if it's escaped, "\${...}", it does not need to conform to the variable syntax.
        delimited(tag("f\""), string_content, cut(tag("\"")))(i)
    }

    pub fn variable_identifier(i: &str) -> IResult<&str, &str> {
        literal_identifier(i)
    }

    pub fn modus_var(i: &str) -> IResult<&str, &str> {
        variable_identifier(i)
    }

    pub fn modus_term(i: &str) -> IResult<&str, ModusTerm> {
        alt((
            map(modus_const, ModusTerm::Constant),
            map(modus_format_string, ModusTerm::FormatString),
            map(modus_var, |s| ModusTerm::UserVariable(s.to_owned())),
        ))(i)
    }

    pub fn modus_clause(i: &str) -> IResult<&str, ModusClause> {
        context("modus_clause", alt((fact, rule)))(i)
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
            predicate: logic::Predicate("l1".into()),
            args: Vec::new(),
        };
        let c = ModusClause {
            head: l1,
            body: None,
        };
        assert_eq!("l1.", c.to_string());
        assert_eq!(Ok(c), "l1.".parse());
    }

    #[test]
    fn rule() {
        let l1 = Literal {
            predicate: logic::Predicate("l1".into()),
            args: Vec::new(),
        };
        let l2 = Literal {
            predicate: logic::Predicate("l2".into()),
            args: Vec::new(),
        };
        let l3 = Literal {
            predicate: logic::Predicate("l3".into()),
            args: Vec::new(),
        };
        let c = Rule {
            head: l1,
            body: Expression::And(Box::new(l2.into()), Box::new(l3.into())).into(),
        };
        assert_eq!("l1 :- (l2, l3).", c.to_string());
        assert_eq!(Ok(c.clone()), "l1 :- l2, l3.".parse());
        assert_eq!(Ok(c.clone()), "l1 :- l2,\n\tl3.".parse());
    }

    #[test]
    fn rule_with_or() {
        let l1: Literal = "l1".parse().unwrap();
        let l2: Literal = "l2".parse().unwrap();
        let c = Rule {
            head: "foo".parse().unwrap(),
            body: Expression::Or(Box::new(l1.into()), Box::new(l2.into())).into(),
        };

        assert_eq!("foo :- (l1; l2).", c.to_string());
        assert_eq!(Ok(c.clone()), "foo :- l1; l2.".parse());
    }

    #[test]
    fn rule_with_operator() {
        let foo = Literal {
            predicate: logic::Predicate("foo".into()),
            args: Vec::new(),
        };
        let a = Literal {
            predicate: logic::Predicate("a".into()),
            args: Vec::new(),
        };
        let b = Literal {
            predicate: logic::Predicate("b".into()),
            args: Vec::new(),
        };
        let merge = Operator {
            predicate: logic::Predicate("merge".into()),
            args: Vec::new(),
        };
        let r = Rule {
            head: foo,
            body: Expression::OperatorApplication(
                Expression::And(Box::new(a.into()), Box::new(b.into())).into(),
                merge,
            )
            .into(),
        };
        assert_eq!("foo :- ((a, b))::merge.", r.to_string());
        assert_eq!(Ok(r.clone()), "foo :- ((a, b))::merge.".parse());
    }

    #[test]
    fn modusclause_to_clause() {
        let foo = Literal {
            predicate: logic::Predicate("foo".into()),
            args: Vec::new(),
        };
        let a = Literal {
            predicate: logic::Predicate("a".into()),
            args: Vec::new(),
        };
        let b = Literal {
            predicate: logic::Predicate("b".into()),
            args: Vec::new(),
        };
        let merge = Operator {
            predicate: logic::Predicate("merge".into()),
            args: Vec::new(),
        };
        let r = Rule {
            head: foo,
            body: Expression::OperatorApplication(
                Expression::And(Box::new(a.into()), Box::new(b.into())).into(),
                merge,
            )
            .into(),
        };
        assert_eq!("foo :- ((a, b))::merge.", r.to_string());

        // Convert to the simpler syntax
        let c: Vec<logic::Clause> = (&r).into();
        assert_eq!(1, c.len());
        assert_eq!("foo :- a, b", c[0].to_string());
    }

    #[test]
    fn modusclause_to_clause_with_or() {
        let foo: Literal = "foo".parse().unwrap();
        let a: Literal = "a".parse().unwrap();
        let b: Literal = "b".parse().unwrap();
        let merge = Operator {
            predicate: logic::Predicate("merge".into()),
            args: Vec::new(),
        };
        let r1 = Rule {
            head: foo.clone(),
            body: Expression::OperatorApplication(
                Expression::Or(Box::new(a.clone().into()), Box::new(b.clone().into())).into(),
                merge,
            )
            .into(),
        };
        let r2 = Rule {
            head: foo.clone(),
            body: Expression::And(
                Box::new(a.clone().into()),
                Box::new(Expression::And(
                    Box::new(b.clone().into()),
                    Box::new(Expression::Or(
                        Box::new(a.clone().into()),
                        Box::new(b.clone().into()),
                    )),
                )),
            )
            .into(),
        };
        assert_eq!("foo :- ((a; b))::merge.", r1.to_string());
        assert_eq!("foo :- (a, (b, (a; b))).", r2.to_string());

        let c1: Vec<logic::Clause> = (&r1).into();
        assert_eq!(2, c1.len());
        assert_eq!("foo :- a", c1[0].to_string());
        assert_eq!("foo :- b", c1[1].to_string());

        let c2: Vec<logic::Clause> = (&r2).into();
        assert_eq!(2, c2.len());
        assert_eq!("foo :- a, b, a", c2[0].to_string());
        assert_eq!("foo :- a, b, b", c2[1].to_string());
    }

    #[test]
    fn modus_constant() {
        // Could use https://crates.io/crates/test_case if this pattern occurs often
        let inp1 = r#""Hello\nWorld""#;
        let inp2 = r#""Tabs\tare\tbetter\tthan\tspaces""#;
        let inp3 = r#""Testing \
                       multiline.""#;
        let (_, s1) = parser::modus_const(inp1).unwrap();
        let (_, s2) = parser::modus_const(inp2).unwrap();
        let (_, s3) = parser::modus_const(inp3).unwrap();

        assert_eq!(s1, "Hello\nWorld");
        assert_eq!(s2, "Tabs\tare\tbetter\tthan\tspaces");
        assert_eq!(s3, "Testing multiline.");
    }

    #[test]
    fn modus_expression() {
        let a: Literal = "a".parse().unwrap();
        let b: Literal = "b".parse().unwrap();
        let c: Literal = "c".parse().unwrap();
        let d: Literal = "d".parse().unwrap();

        let e1 = Expression::And(Expression::Literal(a).into(), Expression::Literal(b).into());
        let e2 = Expression::And(Expression::Literal(c).into(), Expression::Literal(d).into());

        let expr = Expression::Or(e1.into(), e2.into());

        let expr_str = "((a, b); (c, d))";
        assert_eq!(expr_str, expr.to_string());
        let rule = format!("foo :- {}.", expr_str);
        assert_eq!(Ok(Some(expr)), rule.parse().map(|r: ModusClause| r.body));
    }
}
