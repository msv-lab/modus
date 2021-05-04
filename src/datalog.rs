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
pub enum Term<C, V> {
    Constant(C),
    Variable(V)
}

#[derive(Clone, PartialEq, Debug)]
pub struct Literal<C, V> {
    pub name: String,
    pub args: Vec<Term<C,V>>
}

#[derive(Clone, PartialEq, Debug)]
pub struct Rule<C,V> {
    pub head: Literal<C,V>,
    pub body: Vec<Literal<C,V>>
}

impl<C, V> fmt::Display for Term<C, V> 
where
    C: fmt::Display,
    V: fmt::Display
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Term::Constant(s) => write!(f, "\"{}\"", s), //TODO: I need to escape the string
            Term::Variable(s) => write!(f, "{}", s)
        }
    }
}

fn display_sep<T: fmt::Display>(seq: &[T], sep: &str) -> String {
    let mut result = String::new();
    if let Some((last, elements)) = seq.split_last() {
        for el in elements {
            result += &el.to_string();
            result.push_str(sep);
            result.push_str(" ");
        }
        result += &last.to_string();
    }
    result
}

impl<C, V> fmt::Display for Literal<C, V> 
where
    C: fmt::Display,
    V: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}({})", self.name, display_sep(&self.args, ","))
    }
}

impl<C, V> fmt::Display for Rule<C, V>
where
    C: fmt::Display,
    V: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.head, display_sep(&self.body, " &"))
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

//TODO: I need to think more carefully how to connect this to stage name
pub fn literal_identifier(i: &str) -> IResult<&str, &str> {
    recognize(
      pair(
        alpha1,
        many0(alt((alphanumeric1, tag("_"), tag("-"))))
      )
    )(i)
}

pub fn term<'a, FC: 'a, FV: 'a, C, V>(constant: FC, variable: FV) -> impl FnMut(&'a str) -> IResult<&'a str, Term<C, V>>
  where
    FC: FnMut(&'a str) -> IResult<&'a str, C>,
    FV: FnMut(&'a str) -> IResult<&'a str, V>,
{
    alt((map(constant, Term::Constant), map(variable, Term::Variable)))
}

pub fn literal<'a, FC: 'a, FV: 'a, C, V>(constant: FC, variable: FV) -> impl FnMut(&'a str) -> IResult<&'a str, Literal<C, V>>
  where
    FC: FnMut(&'a str) -> IResult<&'a str, C>,
    FV: FnMut(&'a str) -> IResult<&'a str, V>,
{
    map(pair(literal_identifier,
        delimited(terminated(tag("("), optional_space),
                  separated_list0(ws(tag(",")), term(constant, variable)),
                  preceded(optional_space, tag(")")))),
   |(name, args)| Literal { name: name.into(), args })
}

pub fn rule<'a, FC: 'a, FV: 'a, C, V>(constant: FC, variable: FV) -> impl FnMut(&'a str) -> IResult<&'a str, Rule<C, V>>
  where
    FC: FnMut(&'a str) -> IResult<&'a str, C> + Clone,
    FV: FnMut(&'a str) -> IResult<&'a str, V> + Clone,
{
    map(separated_pair(literal(constant.clone(), variable.clone()),
                       ws(tag(":")),
                       separated_list1(ws(tag("&")), literal(constant, variable))),
        |(head, body)| Rule { head, body })
}

#[cfg(test)]
mod tests {
    use super::*;

    // define a toy language StrDatalog for testing

    type StrTerm = Term<String, String>;
    type StrLiteral = Literal<String, String>;
    type StrRule = Rule<String, String>;

    fn str_const(i: &str) -> IResult<&str, String> {
        map(delimited(tag("\""), recognize(many0(none_of("\\\""))), tag("\"")), String::from)(i)
    }

    fn str_var(i: &str) -> IResult<&str, String> {
        map(
            recognize(
                pair(
                    alpha1,
                    many0(alt((alphanumeric1, tag("_")))))), 
            String::from)(i)
    }


    impl str::FromStr for StrRule {
        type Err = String;
    
        fn from_str(s: &str) -> Result<Self, Self::Err> {
            match rule(str_const, str_var)(s) {
                Result::Ok((_, o)) => Ok(o),
                Result::Err(e) => Result::Err(format!("{}", e)),
            }
        }
    }
    
    #[test]
    fn print_simple_rule() {
        let c = StrTerm::Constant("C".into());
        let va = StrTerm::Variable("A".into());
        let vb = StrTerm::Variable("B".into());
        let l1 = StrLiteral{ name: "l1".into(), args: vec![va.clone(), vb.clone()] };
        let l2 = StrLiteral{ name: "l2".into(), args: vec![va.clone(), c.clone()] };
        let l3 = StrLiteral{ name: "l3".into(), args: vec![vb.clone(), c.clone()] };
        let r = StrRule{ head: l1, body: vec![l2, l3] };
        assert_eq!("l1(A, B): l2(A, \"C\") & l3(B, \"C\")", r.to_string());
    }

    #[test]
    fn parse_simple_rule() {
        let c = StrTerm::Constant("C".into());
        let va = StrTerm::Variable("A".into());
        let vb = StrTerm::Variable("B".into());
        let l1 = StrLiteral{ name: "l1".into(), args: vec![va.clone(), vb.clone()] };
        let l2 = StrLiteral{ name: "l2".into(), args: vec![va.clone(), c.clone()] };
        let l3 = StrLiteral{ name: "l3".into(), args: vec![vb.clone(), c.clone()] };
        let r = StrRule{ head: l1, body: vec![l2, l3] };
        assert_eq!(Ok(r), "l1(A, B): l2(A, \"C\") & l3(B, \"C\")".parse());
    }

}