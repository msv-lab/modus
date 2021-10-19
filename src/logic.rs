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
use std::convert::TryInto;
use std::fmt;
use std::str;
use std::{collections::HashSet, hash::Hash};

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Atom(pub String);

/// C is constant, V is variable
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum Term<C, V> {
    Constant(C),
    Atom(Atom),
    Variable(V),
    Compound(Atom, Vec<Term<C, V>>),
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Literal<C, V> {
    pub atom: Atom,
    pub args: Vec<Term<C, V>>,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Signature(pub Atom, pub u32);

#[derive(Clone, PartialEq, Debug)]
pub struct Clause<C, V> {
    pub head: Literal<C, V>,
    pub body: Vec<Literal<C, V>>,
}

pub trait Ground {
    fn is_ground(&self) -> bool;
}

impl<C, V: Clone + Eq + Hash> Term<C, V> {
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

impl<C, V: Clone + Eq + Hash> Literal<C, V> {
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

impl<C, V: Clone + Eq + Hash> Clause<C, V> {
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

impl<C, V> Ground for Term<C, V>
where
    V: Clone + Eq + Hash,
{
    fn is_ground(&self) -> bool {
        self.variables().is_empty()
    }
}

impl<C, V> Ground for Literal<C, V>
where
    V: Clone + Eq + Hash,
{
    fn is_ground(&self) -> bool {
        self.variables().is_empty()
    }
}

impl<C, V> Ground for Clause<C, V>
where
    V: Clone + Eq + Hash,
{
    fn is_ground(&self) -> bool {
        self.variables().is_empty()
    }
}

impl fmt::Display for Signature {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/{}", self.0, self.1)
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<C, V> fmt::Display for Term<C, V>
where
    C: fmt::Display,
    V: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Term::Constant(s) => write!(f, "{}", s),
            Term::Variable(s) => write!(f, "{}", s),
            Term::Atom(s) => write!(f, "{}", s),
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
            _ => write!(f, "{}({})", self.atom, display_sep(&self.args, ", ")),
        }
    }
}

impl<C, V> fmt::Display for Clause<C, V>
where
    C: fmt::Display,
    V: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.body.len() {
            0 => write!(f, "{}.", self.head),
            _ => write!(f, "{} :- {}.", self.head, display_sep(&self.body, ", ")),
        }
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

    pub fn term<'a, FC: 'a, FV: 'a, C, V>(
        constant: FC,
        variable: FV,
    ) -> impl FnMut(&'a str) -> IResult<&'a str, Term<C, V>>
    where
        FC: FnMut(&'a str) -> IResult<&'a str, C>,
        FV: FnMut(&'a str) -> IResult<&'a str, V>,
    {
        alt((map(constant, Term::Constant), map(variable, Term::Variable)))
    }

    pub fn literal<'a, FC: 'a, FV: 'a, C, V>(
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
                    atom: Atom(name.into()),
                    args,
                },
                None => Literal {
                    atom: Atom(name.into()),
                    args: Vec::new(),
                },
            },
        )
    }

    pub fn clause<'a, FC: 'a, FV: 'a, C, V>(
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
    use super::Atom;
    use fp_core::compose::compose_two;
    use std::str;

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
    pub type Term = super::Term<Atom, Variable>;
    pub type Literal = super::Literal<Atom, Variable>;
    pub type Clause = super::Clause<Atom, Variable>;

    fn toy_var(i: &str) -> IResult<&str, Variable> {
        map(
            recognize(pair(
                one_of("_ABCDEFGHIJKLMNOPQRSTUVWXYZ"),
                many0(alt((alphanumeric1, tag("_")))),
            )),
            String::from,
        )(i)
    }

    fn toy_const(i: &str) -> IResult<&str, Atom> {
        map(
            recognize(pair(
                one_of("abcdefghijklmnopqrstuvwxyz"),
                many0(alt((alphanumeric1, tag("_")))),
            )),
            compose!(String::from, Atom),
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
        let c = toy::Term::Constant(Atom("c".into()));
        let va = toy::Term::Variable("A".into());
        let vb = toy::Term::Variable("B".into());
        let l1 = toy::Literal {
            atom: Atom("l1".into()),
            args: vec![va.clone(), vb.clone()],
        };
        let l2 = toy::Literal {
            atom: Atom("l2".into()),
            args: vec![va.clone(), c.clone()],
        };
        let l3 = toy::Literal {
            atom: Atom("l3".into()),
            args: vec![vb.clone(), c.clone()],
        };
        let r = Clause {
            head: l1,
            body: vec![l2, l3],
        };
        assert_eq!("l1(A, B) :- l2(A, c), l3(B, c).", r.to_string());
        assert_eq!(Ok(r), "l1(A, B) :- l2(A, c), l3(B, c).".parse());
    }

    #[test]
    fn nullary_predicate() {
        let va = toy::Term::Variable("A".into());
        let l1 = toy::Literal {
            atom: Atom("l1".into()),
            args: Vec::new(),
        };
        let l2 = toy::Literal {
            atom: Atom("l2".into()),
            args: vec![va.clone()],
        };
        let r = Clause {
            head: l1,
            body: vec![l2],
        };
        assert_eq!("l1 :- l2(A).", r.to_string());
        assert_eq!(Ok(r), "l1 :- l2(A).".parse());
    }
}
