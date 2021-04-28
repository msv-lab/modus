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
    bytes::complete::{is_not, tag, tag_no_case},
    character::complete::{
        char,
        alpha1,
        alphanumeric1,
        space0,
        space1,
        line_ending,
        not_line_ending,
        one_of,
        none_of
    },
    branch::alt,
    sequence::{pair, delimited, terminated, preceded, tuple, separated_pair},
    multi::{many0, many1, separated_list1},
    combinator::{value, recognize, map, opt, eof},
    error::ParseError,
    IResult
};


#[derive(Clone, PartialEq, Debug)]
pub enum Term {
    Constant(String),
    Variable(String)
}

#[derive(Clone, PartialEq, Debug)]
pub struct Literal {
    name: String,
    args: Vec<Term>
}

#[derive(Clone, PartialEq, Debug)]
pub struct Rule {
    head: Literal,
    body: Vec<Literal>
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Term::Constant(s) => write!(f, "\"{}\"", s), //TODO: I need to escape the string
            Term::Variable(s) => write!(f, "{}", s)
        }
    }
}

fn display_sep_by_comma<T: fmt::Display>(seq: &[T]) -> String {
    let mut result = String::new();
    match seq.split_last() {
        Some((last, elements)) => {
            for el in elements {
                result += &el.to_string();
                result.push_str(", ");
            }
            result += &last.to_string();
        },
        None => ()
    }
    result
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({})", self.name, display_sep_by_comma(&self.args))
    }
}

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} :- {}", self.head, display_sep_by_comma(&self.body))
    }
}


impl str::FromStr for Rule {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match rule(s) {
            Result::Ok((_, o)) => Ok(o),
            Result::Err(e) => Result::Err(format!("{}", e)),
        }
    }
}

fn space(i: &str) -> IResult<&str, ()> {
    value(
        (), // Output is thrown away.
        one_of(" \t")
    )(i)
}

fn optional_space(i: &str) -> IResult<&str, ()> {
    value(
        (), // Output is thrown away.
        many0(space)
    )(i)
}

fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O>
  where
  F: FnMut(&'a str) -> IResult<&'a str, O>,
{
  delimited(
    optional_space,
    inner,
    optional_space
  )
}

//TODO: I need to thing more carefully how to connect this to stage name
pub fn literal_identifier(i: &str) -> IResult<&str, &str> {
    recognize(
      pair(
        alpha1,
        many0(alt((alphanumeric1, tag("_"), tag("-"))))
      )
    )(i)
}

//TODO: I need to thing more carefully how to connect this to ARGs
pub fn variable_identifier(i: &str) -> IResult<&str, &str> {
    recognize(
      pair(
        alpha1,
        many0(alt((alphanumeric1, tag("_"))))
      )
    )(i)
}

//TODO: support proper string literals
pub fn string_content(i: &str) -> IResult<&str, &str> {
    recognize(many0(none_of("\\\"")))(i)
}

fn term(i: &str) -> IResult<&str, Term> {
    alt((
        map(delimited(tag("\""), string_content, tag("\"")), |s| Term::Constant(s.into())),
        map(variable_identifier, |s| Term::Variable(s.into()))
    ))(i)
}

fn literal(i: &str) -> IResult<&str, Literal> {
    map(pair(literal_identifier,
             delimited(terminated(tag("("), optional_space),
                       separated_list1(ws(tag(",")), term),
                       preceded(optional_space, tag(")")))),
        |(name, args)| Literal { name: name.into(), args })(i)
}

fn rule(i: &str) -> IResult<&str, Rule> {
    map(separated_pair(literal, ws(tag(":-")), separated_list1(ws(tag(",")), literal)),
        |(head, body)| Rule { head, body })(i)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn print_simple_rule() {
        let c = Term::Constant("C".into());
        let va = Term::Variable("A".into());
        let vb = Term::Variable("B".into());
        let l1 = Literal{ name: "l1".into(), args: vec![va.clone(), vb.clone()] };
        let l2 = Literal{ name: "l2".into(), args: vec![va.clone(), c.clone()] };
        let l3 = Literal{ name: "l3".into(), args: vec![vb.clone(), c.clone()] };
        let r = Rule{ head: l1, body: vec![l2, l3] };
        assert_eq!("l1(A, B) :- l2(A, \"C\"), l3(B, \"C\")", r.to_string());
    }

    #[test]
    fn parse_simple_rule() {
        let c = Term::Constant("C".into());
        let va = Term::Variable("A".into());
        let vb = Term::Variable("B".into());
        let l1 = Literal{ name: "l1".into(), args: vec![va.clone(), vb.clone()] };
        let l2 = Literal{ name: "l2".into(), args: vec![va.clone(), c.clone()] };
        let l3 = Literal{ name: "l3".into(), args: vec![vb.clone(), c.clone()] };
        let r = Rule{ head: l1, body: vec![l2, l3] };
        assert_eq!(Ok(r), "l1(A, B) :- l2(A, \"C\"), l3(B, \"C\")".parse());
    }

}