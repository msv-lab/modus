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
    bytes::complete::{tag},
    character::complete::{
        alpha1,
        alphanumeric1,
        one_of,
        none_of
    },
    branch::alt,
    sequence::{pair, delimited, terminated, preceded, separated_pair},
    multi::{many0, separated_list0, separated_list1},
    combinator::{value, recognize, map},
    IResult
};


#[derive(Clone, PartialEq, Debug)]
pub enum DatalogTerm {
    Constant(String),
    Variable(String)
}

#[derive(Clone, PartialEq, Debug)]
pub struct DatalogLiteral {
    pub name: String,
    pub args: Vec<DatalogTerm>
}

#[derive(Clone, PartialEq, Debug)]
pub struct DatalogRule {
    pub head: DatalogLiteral,
    pub body: Vec<DatalogLiteral>
}

impl fmt::Display for DatalogTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            DatalogTerm::Constant(s) => write!(f, "\"{}\"", s), //TODO: I need to escape the string
            DatalogTerm::Variable(s) => write!(f, "{}", s)
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

impl fmt::Display for DatalogLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({})", self.name, display_sep_by_comma(&self.args))
    }
}

impl fmt::Display for DatalogRule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} :- {}", self.head, display_sep_by_comma(&self.body))
    }
}


impl str::FromStr for DatalogRule {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match datalog_rule(s) {
            Result::Ok((_, o)) => Ok(o),
            Result::Err(e) => Result::Err(format!("{}", e)),
        }
    }
}

impl str::FromStr for DatalogLiteral {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match datalog_literal(s) {
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

fn datalog_term(i: &str) -> IResult<&str, DatalogTerm> {
    alt((
        map(delimited(tag("\""), string_content, tag("\"")), |s| DatalogTerm::Constant(s.into())),
        map(variable_identifier, |s| DatalogTerm::Variable(s.into()))
    ))(i)
}

fn datalog_literal(i: &str) -> IResult<&str, DatalogLiteral> {
    map(pair(literal_identifier,
             delimited(terminated(tag("("), optional_space),
                       separated_list0(ws(tag(",")), datalog_term),
                       preceded(optional_space, tag(")")))),
        |(name, args)| DatalogLiteral { name: name.into(), args })(i)
}

pub fn datalog_rule(i: &str) -> IResult<&str, DatalogRule> {
    map(separated_pair(datalog_literal, ws(tag(":-")), separated_list1(ws(tag(",")), datalog_literal)),
        |(head, body)| DatalogRule { head, body })(i)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn print_simple_rule() {
        let c = DatalogTerm::Constant("C".into());
        let va = DatalogTerm::Variable("A".into());
        let vb = DatalogTerm::Variable("B".into());
        let l1 = DatalogLiteral{ name: "l1".into(), args: vec![va.clone(), vb.clone()] };
        let l2 = DatalogLiteral{ name: "l2".into(), args: vec![va.clone(), c.clone()] };
        let l3 = DatalogLiteral{ name: "l3".into(), args: vec![vb.clone(), c.clone()] };
        let r = DatalogRule{ head: l1, body: vec![l2, l3] };
        assert_eq!("l1(A, B) :- l2(A, \"C\"), l3(B, \"C\")", r.to_string());
    }

    #[test]
    fn parse_simple_rule() {
        let c = DatalogTerm::Constant("C".into());
        let va = DatalogTerm::Variable("A".into());
        let vb = DatalogTerm::Variable("B".into());
        let l1 = DatalogLiteral{ name: "l1".into(), args: vec![va.clone(), vb.clone()] };
        let l2 = DatalogLiteral{ name: "l2".into(), args: vec![va.clone(), c.clone()] };
        let l3 = DatalogLiteral{ name: "l3".into(), args: vec![vb.clone(), c.clone()] };
        let r = DatalogRule{ head: l1, body: vec![l2, l3] };
        assert_eq!(Ok(r), "l1(A, B) :- l2(A, \"C\"), l3(B, \"C\")".parse());
    }

}