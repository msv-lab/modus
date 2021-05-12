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
    combinator::{value, recognize, map, opt},
    IResult
};
use fp_core::compose::*;

#[derive(Clone, PartialEq, Debug)]
pub struct Atom(pub String);

#[derive(Clone, PartialEq, Debug)]
pub enum Term<C, V> {
    Constant(C),
    Atom(Atom),
    Variable(V),
    Compound(Atom, Vec<Term<C, V>>),
}

#[derive(Clone, PartialEq, Debug)]
pub struct Literal<C, V> {
    pub atom: Atom,
    pub args: Vec<Term<C,V>>
}

#[derive(Clone, PartialEq, Debug)]
pub struct Rule<C,V> {
    pub head: Literal<C,V>,
    pub body: Vec<Literal<C,V>>
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<C, V> fmt::Display for Term<C, V> 
where
    C: fmt::Display,
    V: fmt::Display
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Term::Constant(s)    => write!(f, "{}", s),
            Term::Variable(s)    => write!(f, "{}", s),
            Term::Atom(s)        => write!(f, "{}", s),
            Term::Compound(a, l) => write!(f, "{}({})", a, display_sep(l, ", ")),
        }
    }
}

fn display_sep<T: fmt::Display>(seq: &[T], sep: &str) -> String {
    let mut result = String::new();
    if let Some((last, elements)) = seq.split_last() {
        for el in elements {
            result += &el.to_string();
            result.push_str(sep);
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
        match &*self.args {
            [] => write!(f, "{}", self.atom),
            _ => write!(f, "{}({})", self.atom, display_sep(&self.args, ", "))
        }
    }
}

impl<C, V> fmt::Display for Rule<C, V>
where
    C: fmt::Display,
    V: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} :- {}", self.head, display_sep(&self.body, ", "))
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
             opt(delimited(terminated(tag("("), optional_space),
                       separated_list1(ws(tag(",")), term(constant, variable)),
                       preceded(optional_space, tag(")"))))),
   |(name, args)| match args { Some(args) => Literal { atom: Atom(name.into()), args },
                               None => Literal { atom: Atom(name.into()), args: Vec::new() } })
}

pub fn rule<'a, FC: 'a, FV: 'a, C, V>(constant: FC, variable: FV) -> impl FnMut(&'a str) -> IResult<&'a str, Rule<C, V>>
  where
    FC: FnMut(&'a str) -> IResult<&'a str, C> + Clone,
    FV: FnMut(&'a str) -> IResult<&'a str, V> + Clone,
{
    map(separated_pair(literal(constant.clone(), variable.clone()),
                       ws(tag(":-")),
                       separated_list1(ws(tag(",")), literal(constant, variable))),
        |(head, body)| Rule { head, body })
}

#[cfg(test)]
mod tests {
    use super::*;

    // define a toy language for testing

    #[derive(Clone, PartialEq, Debug)]
    struct StringConst(String);

    impl fmt::Display for StringConst {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "\"{}\"", self.0)
        }
    }
    
    type TestTerm = Term<StringConst, String>;
    type TestRule = Rule<StringConst, String>;

    fn str_const(i: &str) -> IResult<&str, StringConst> {
        map(delimited(tag("\""), recognize(many0(none_of("\\\""))), tag("\"")),
            compose!(String::from, StringConst))(i)
    }

    fn str_var(i: &str) -> IResult<&str, String> {
        map(
            recognize(
                pair(
                    alpha1,
                    many0(alt((alphanumeric1, tag("_")))))), 
            String::from)(i)
    }

    impl str::FromStr for TestRule {
        type Err = String;
    
        fn from_str(s: &str) -> Result<Self, Self::Err> {
            match rule(str_const, str_var)(s) {
                Result::Ok((_, o)) => Ok(o),
                Result::Err(e) => Result::Err(format!("{}", e)),
            }
        }
    }
    
    #[test]
    fn simple_rule() {
        let c = TestTerm::Constant(StringConst("C".into()));
        let va = Term::Variable("A".into());
        let vb = Term::Variable("B".into());
        let l1 = Literal{ atom: Atom("l1".into()), args: vec![va.clone(), vb.clone()] };
        let l2 = Literal{ atom: Atom("l2".into()), args: vec![va.clone(), c.clone()] };
        let l3 = Literal{ atom: Atom("l3".into()), args: vec![vb.clone(), c.clone()] };
        let r = Rule{ head: l1, body: vec![l2, l3] };
        assert_eq!("l1(A, B) :- l2(A, \"C\"), l3(B, \"C\")", r.to_string());
        assert_eq!(Ok(r), "l1(A, B) :- l2(A, \"C\"), l3(B, \"C\")".parse());
    }

    #[test]
    fn nullary_predicate() {
        let va = TestTerm::Variable("A".into());
        let l1 = Literal{ atom: Atom("l1".into()), args: Vec::new() };
        let l2 = Literal{ atom: Atom("l2".into()), args: vec![va.clone()] };
        let r = Rule{ head: l1, body: vec![l2] };
        assert_eq!("l1 :- l2(A)", r.to_string());
        assert_eq!(Ok(r), "l1 :- l2(A)".parse());
    }

}