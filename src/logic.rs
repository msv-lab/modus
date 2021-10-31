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

//! This module contains logical structures that define the intermediate language used by Modus.
//!
//! Currently, these structures are generic, parameterized over the types they may use for constants
//! or variables.

use crate::sld;
use crate::unification::{Rename, Substitution};
use std::collections::HashMap;
use std::convert::TryInto;
use std::fmt;
use std::fmt::{Debug, Display};
use std::str;
use std::sync::atomic::{AtomicU32, Ordering};
use std::{collections::HashSet, hash::Hash};

pub trait ModusConstant: Clone + PartialEq + Eq + Hash + Debug + Display + From<String> {}
pub trait ModusVariable: Clone + PartialEq + Eq + Hash + Debug + Display + Rename<Self> {}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Constant {
    String(String),
    Integer(u32), //TODO: arbitrary-precision arithmetic?
}

impl From<String> for Constant {
    fn from(s: String) -> Self {
        Constant::String(s)
    }
}

impl ModusConstant for Constant {}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Variable {
    User(String),
    Auxiliary(u32),
    Renamed(u32, Box<Variable>),
}

impl fmt::Display for Variable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Variable::User(s) => write!(f, "{}", s),
            _ => unimplemented!(),
        }
    }
}

static AVAILABLE_VARIABLE_INDEX: AtomicU32 = AtomicU32::new(0);

impl Rename<Variable> for Variable {
    fn rename(&self) -> Variable {
        let index = AVAILABLE_VARIABLE_INDEX.fetch_add(1, Ordering::SeqCst);
        Variable::Renamed(index, Box::new((*self).clone()))
    }
}

impl sld::Variable<Constant, Variable> for Variable {
    fn aux() -> Variable {
        let index = AVAILABLE_VARIABLE_INDEX.fetch_add(1, Ordering::SeqCst);
        Variable::Auxiliary(index)
    }
}

impl ModusVariable for Variable {}

/// A predicate symbol
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Predicate(pub String);

impl From<String> for Predicate {
    fn from(s: String) -> Self {
        Predicate(s)
    }
}

/// C is constant, V is variable
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Term<C: ModusConstant = Constant, V: ModusVariable = Variable> {
    Constant(C),
    Variable(V),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Literal<C: ModusConstant = Constant, V: ModusVariable = Variable> {
    pub atom: Predicate,
    pub args: Vec<Term<C, V>>,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Signature(pub Predicate, pub u32);

#[derive(Clone, PartialEq, Debug)]
pub struct Clause<C: ModusConstant = Constant, V: ModusVariable = Variable> {
    pub head: Literal<C, V>,
    pub body: Vec<Literal<C, V>>,
}

pub trait Ground {
    fn is_ground(&self) -> bool;
}

impl<C: ModusConstant, V: ModusVariable> Term<C, V> {
    pub fn variables(&self) -> HashSet<V> {
        let mut s = HashSet::<V>::new();
        match self {
            Term::Variable(v) => {
                s.insert(v.clone());
            }
            _ => (),
        };
        s
    }
}

impl<C: ModusConstant, V: ModusVariable> Literal<C, V> {
    pub fn signature(&self) -> Signature {
        Signature(self.atom.clone(), self.args.len().try_into().unwrap())
    }
    pub fn variables(&self) -> HashSet<V> {
        self.args
            .iter()
            .map(|r| r.variables())
            .reduce(|mut l, r| {
                l.extend(r);
                l
            })
            .unwrap_or_default()
    }
}

impl<C: ModusConstant, V: ModusVariable> Clause<C, V> {
    pub fn variables(&self) -> HashSet<V> {
        let mut body = self
            .body
            .iter()
            .map(|r| r.variables())
            .reduce(|mut l, r| {
                l.extend(r);
                l
            })
            .unwrap_or_default();
        body.extend(self.head.variables());
        body
    }
}

impl<C: ModusConstant, V: ModusVariable> Ground for Term<C, V> {
    fn is_ground(&self) -> bool {
        self.variables().is_empty()
    }
}

impl<C: ModusConstant, V: ModusVariable> Ground for Literal<C, V> {
    fn is_ground(&self) -> bool {
        self.variables().is_empty()
    }
}

impl<C: ModusConstant, V: ModusVariable> Ground for Clause<C, V> {
    fn is_ground(&self) -> bool {
        self.variables().is_empty()
    }
}

impl fmt::Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.0, self.1)
    }
}

impl fmt::Display for Predicate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<C: ModusConstant, V: ModusVariable> fmt::Display for Term<C, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Term::Constant(s) => write!(f, "{}", s),
            Term::Variable(s) => write!(f, "{}", s),
        }
    }
}

fn display_sep<T: fmt::Display>(seq: &[T], sep: &str) -> String {
    return seq
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<String>>()
        .join(sep);
}

impl<C, V> fmt::Display for Literal<C, V>
where
    C: ModusConstant,
    V: ModusVariable,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.args {
            [] => write!(f, "{}", self.atom),
            _ => write!(f, "{}({})", self.atom, display_sep(&self.args, ", ")),
        }
    }
}

impl<C, V> fmt::Display for Clause<C, V>
where
    C: ModusConstant,
    V: ModusVariable,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} :- {}", self.head, display_sep(&self.body, ", "))
    }
}

pub mod parser {

    use super::*;

    use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::{alpha1, alphanumeric1, one_of},
        combinator::{map, opt, recognize, value},
        multi::{many0, separated_list0, separated_list1},
        sequence::{delimited, pair, preceded, separated_pair, terminated},
        IResult,
    };

    fn space(i: &str) -> IResult<&str, ()> {
        value(
            (), // Output is thrown away.
            one_of(" \t"),
        )(i)
    }

    fn optional_space(i: &str) -> IResult<&str, ()> {
        value(
            (), // Output is thrown away.
            many0(space),
        )(i)
    }

    fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O>
    where
        F: FnMut(&'a str) -> IResult<&'a str, O>,
    {
        delimited(optional_space, inner, optional_space)
    }

    //TODO: I need to think more carefully how to connect this to stage name
    pub fn literal_identifier(i: &str) -> IResult<&str, &str> {
        recognize(pair(
            alpha1,
            many0(alt((alphanumeric1, tag("_"), tag("-")))),
        ))(i)
    }

    pub fn term<'a, FC: 'a, FV: 'a, C: ModusConstant, V: ModusVariable>(
        constant: FC,
        variable: FV,
    ) -> impl FnMut(&'a str) -> IResult<&'a str, Term<C, V>>
    where
        FC: FnMut(&'a str) -> IResult<&'a str, C>,
        FV: FnMut(&'a str) -> IResult<&'a str, V>,
    {
        alt((map(constant, Term::Constant), map(variable, Term::Variable)))
    }

    pub fn literal<'a, FC: 'a, FV: 'a, C: ModusConstant, V: ModusVariable>(
        constant: FC,
        variable: FV,
    ) -> impl FnMut(&'a str) -> IResult<&'a str, Literal<C, V>>
    where
        FC: FnMut(&'a str) -> IResult<&'a str, C>,
        FV: FnMut(&'a str) -> IResult<&'a str, V>,
    {
        map(
            pair(
                literal_identifier,
                opt(delimited(
                    terminated(tag("("), optional_space),
                    separated_list1(ws(tag(",")), term(constant, variable)),
                    preceded(optional_space, tag(")")),
                )),
            ),
            |(name, args)| match args {
                Some(args) => Literal {
                    atom: Predicate(name.into()),
                    args,
                },
                None => Literal {
                    atom: Predicate(name.into()),
                    args: Vec::new(),
                },
            },
        )
    }

    pub fn clause<'a, FC: 'a, FV: 'a, C: ModusConstant, V: ModusVariable>(
        constant: FC,
        variable: FV,
    ) -> impl FnMut(&'a str) -> IResult<&'a str, Clause<C, V>>
    where
        FC: FnMut(&'a str) -> IResult<&'a str, C> + Clone,
        FV: FnMut(&'a str) -> IResult<&'a str, V> + Clone,
    {
        map(
            separated_pair(
                literal(constant.clone(), variable.clone()),
                ws(tag(":-")),
                separated_list0(ws(tag(",")), literal(constant, variable)),
            ),
            |(head, body)| Clause { head, body },
        )
    }
}

pub mod toy {
    use super::Predicate;
    use fp_core::compose::compose_two;
    use std::str;

    use crate::logic::{ModusConstant, ModusVariable};
    use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::{alphanumeric1, one_of},
        combinator::{map, recognize},
        multi::many0,
        sequence::pair,
        IResult,
    };

    // define a toy language with only atoms for testing

    pub type Variable = String;
    pub type Term = super::Term<Predicate, Variable>;
    pub type Literal = super::Literal<Predicate, Variable>;
    pub type Clause = super::Clause<Predicate, Variable>;

    impl ModusConstant for Predicate {}
    impl ModusVariable for Variable {}

    fn toy_var(i: &str) -> IResult<&str, Variable> {
        map(
            recognize(pair(
                one_of("_ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
                many0(alt((alphanumeric1, tag("_")))),
            )),
            String::from,
        )(i)
    }

    fn toy_const(i: &str) -> IResult<&str, Predicate> {
        map(
            recognize(pair(
                one_of("abcdefghijklmnopqrstuvwxyz"),
                many0(alt((alphanumeric1, tag("_")))),
            )),
            compose!(String::from, Predicate),
        )(i)
    }

    impl str::FromStr for Clause {
        type Err = String;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            match super::parser::clause(toy_const, toy_var)(s) {
                Result::Ok((_, o)) => Ok(o),
                Result::Err(e) => Result::Err(format!("{}", e)),
            }
        }
    }

    impl str::FromStr for Literal {
        type Err = String;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            match super::parser::literal(toy_const, toy_var)(s) {
                Result::Ok((_, o)) => Ok(o),
                Result::Err(e) => Result::Err(format!("{}", e)),
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::toy;
    use super::*;

    #[test]
    fn simple_rule() {
        let c = toy::Term::Constant(Predicate("c".into()));
        let va = toy::Term::Variable("A".into());
        let vb = toy::Term::Variable("B".into());
        let l1 = toy::Literal {
            atom: Predicate("l1".into()),
            args: vec![va.clone(), vb.clone()],
        };
        let l2 = toy::Literal {
            atom: Predicate("l2".into()),
            args: vec![va.clone(), c.clone()],
        };
        let l3 = toy::Literal {
            atom: Predicate("l3".into()),
            args: vec![vb.clone(), c.clone()],
        };
        let r = Clause {
            head: l1,
            body: vec![l2, l3],
        };
        assert_eq!("l1(A, B) :- l2(A, c), l3(B, c)", r.to_string());
        assert_eq!(Ok(r), "l1(A, B) :- l2(A, c), l3(B, c)".parse());
    }

    #[test]
    fn nullary_predicate() {
        let va = toy::Term::Variable("A".into());
        let l1 = toy::Literal {
            atom: Predicate("l1".into()),
            args: Vec::new(),
        };
        let l2 = toy::Literal {
            atom: Predicate("l2".into()),
            args: vec![va.clone()],
        };
        let r = Clause {
            head: l1,
            body: vec![l2],
        };
        assert_eq!("l1 :- l2(A)", r.to_string());
        assert_eq!(Ok(r), "l1 :- l2(A)".parse());
    }
}
