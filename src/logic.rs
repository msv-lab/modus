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

use nom_locate::LocatedSpan;

use crate::logic::parser::Span;
use crate::sld;
use crate::unification::Rename;

use std::convert::TryInto;
use std::fmt;
use std::fmt::Debug;
use std::str;
use std::sync::atomic::{AtomicU32, Ordering};
use std::{collections::HashSet, hash::Hash};

impl fmt::Display for IRTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IRTerm::Constant(s) => write!(f, "\"{}\"", s),
            IRTerm::UserVariable(s) => write!(f, "{}", s),
            // there may be aux variables after translating to IR
            IRTerm::AuxiliaryVariable(i) => write!(f, "__AUX_{}", i),
            IRTerm::RenamedVariable(i, t) => write!(f, "{}_{}", t, i),
        }
    }
}

pub static AVAILABLE_VARIABLE_INDEX: AtomicU32 = AtomicU32::new(0);

impl Rename<IRTerm> for IRTerm {
    fn rename(&self) -> IRTerm {
        match self {
            IRTerm::Constant(_) => (*self).clone(),
            _ => {
                let index = AVAILABLE_VARIABLE_INDEX.fetch_add(1, Ordering::SeqCst);
                IRTerm::RenamedVariable(index, Box::new((*self).clone()))
            }
        }
    }
}

impl sld::Auxiliary for IRTerm {
    fn aux() -> IRTerm {
        let index = AVAILABLE_VARIABLE_INDEX.fetch_add(1, Ordering::SeqCst);
        IRTerm::AuxiliaryVariable(index)
    }
}

/// A predicate symbol
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Predicate(pub String);

impl Predicate {
    /// True if this predicate symbol represents an operator.
    pub fn is_operator(&self) -> bool {
        self.0.starts_with("_operator_")
    }
}

impl From<String> for Predicate {
    fn from(s: String) -> Self {
        Predicate(s)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum IRTerm {
    Constant(String),
    UserVariable(String),
    AuxiliaryVariable(u32),
    RenamedVariable(u32, Box<IRTerm>),
}

impl IRTerm {
    pub fn as_constant(&self) -> Option<&str> {
        match self {
            IRTerm::Constant(c) => Some(&c[..]),
            _ => None,
        }
    }

    /// Gets the original IRTerm from a renamed one, or returns itself.
    pub fn get_original(&self) -> &IRTerm {
        match self {
            IRTerm::RenamedVariable(_, t) => t.get_original(),
            t => t,
        }
    }
}

/// Structure that holds information about the position of some section of the source code.
///
/// Not to be confused with `parser::Span`.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct SpannedPosition {
    /// The relative offset of this spanned position from the original input.
    pub offset: usize,

    /// Length of this spanned position. Assumes ASCII text (i.e. each character is a byte).
    pub length: usize,
}

impl From<Span<'_>> for SpannedPosition {
    fn from(s: Span) -> Self {
        SpannedPosition {
            length: s.fragment().len(),
            offset: s.location_offset(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Literal<T = IRTerm> {
    pub position: Option<SpannedPosition>,
    pub predicate: Predicate,
    pub args: Vec<T>,
}

#[cfg(test)]
impl<T: PartialEq> Literal<T> {
    /// Checks for equality, ignoring the position fields.
    pub fn eq_ignoring_position(&self, other: &Literal<T>) -> bool {
        self.predicate == other.predicate && self.args == other.args
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Signature(pub Predicate, pub u32);

#[derive(Clone, PartialEq, Debug)]
pub struct Clause<T = IRTerm> {
    pub head: Literal<T>,
    pub body: Vec<Literal<T>>,
}

#[cfg(test)]
impl<T: PartialEq> Clause<T> {
    fn eq_ignoring_position(&self, other: &Clause<T>) -> bool {
        self.head.eq_ignoring_position(&other.head)
            && self
                .body
                .iter()
                .enumerate()
                .all(|(i, l)| l.eq_ignoring_position(&other.body[i]))
    }
}

pub trait Ground {
    fn is_ground(&self) -> bool;
}

impl IRTerm {
    pub fn variables(&self) -> HashSet<IRTerm> {
        // the 'variables' of an IRTerm is just itself, if it's not a constant
        let mut set = HashSet::<IRTerm>::new();
        if let IRTerm::Constant(_) = self {
        } else {
            set.insert(self.clone());
        }
        set
    }
}

impl Literal {
    pub fn signature(&self) -> Signature {
        Signature(self.predicate.clone(), self.args.len().try_into().unwrap())
    }
    pub fn variables(&self) -> HashSet<IRTerm> {
        self.args
            .iter()
            .map(|r| r.variables())
            .reduce(|mut l, r| {
                l.extend(r);
                l
            })
            .unwrap_or_default()
    }

    pub fn with_position(self, position: Option<SpannedPosition>) -> Literal {
        Literal { position, ..self }
    }
}

impl Clause {
    pub fn variables(&self) -> HashSet<IRTerm> {
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

impl Ground for IRTerm {
    fn is_ground(&self) -> bool {
        matches!(self, IRTerm::Constant(_))
    }
}

impl Ground for Literal {
    fn is_ground(&self) -> bool {
        self.variables().is_empty()
    }
}

impl Ground for Clause {
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

fn display_sep<T: fmt::Display>(seq: &[T], sep: &str) -> String {
    return seq
        .iter()
        .map(|t| t.to_string())
        .collect::<Vec<String>>()
        .join(sep);
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.args {
            [] => write!(f, "{}", self.predicate),
            _ => write!(f, "{}({})", self.predicate, display_sep(&self.args, ", ")),
        }
    }
}

impl fmt::Display for Clause {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} :- {}", self.head, display_sep(&self.body, ", "))
    }
}

impl str::FromStr for Clause {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let span = Span::new(s);
        match parser::clause(parser::term)(span) {
            Result::Ok((_, o)) => Ok(o),
            Result::Err(e) => Result::Err(format!("{}", e)),
        }
    }
}

impl str::FromStr for Literal {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let span = Span::new(s);
        match parser::literal(parser::term)(span) {
            Result::Ok((_, o)) => Ok(o),
            Result::Err(e) => Result::Err(format!("{}", e)),
        }
    }
}

/// The parser for the IR is only for convenience in writing tests.
pub mod parser {

    use super::*;

    use nom::{
        branch::alt,
        bytes::complete::{tag, take_until},
        character::complete::{alpha1, alphanumeric1, space0},
        combinator::{cut, map, opt, recognize},
        error::VerboseError,
        multi::{many0, separated_list0, separated_list1},
        sequence::{delimited, pair, preceded, separated_pair, terminated},
        Offset, Slice,
    };

    pub type Span<'a> = LocatedSpan<&'a str>;

    /// Redeclaration that uses VerboseError instead of the default nom::Error.
    pub type IResult<T, O> = nom::IResult<T, O, VerboseError<T>>;

    /// Creates a parser that returns a `SpannedPosition` that spans the consumed input
    /// of a given parser. Also returns the actual output of the parser.
    pub fn recognized_span<'a, P, T>(
        mut inner: P,
    ) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, (SpannedPosition, T)>
    where
        P: FnMut(Span<'a>) -> IResult<Span<'a>, T>,
    {
        move |i| {
            let original_i = i.clone();

            let (i, o) = inner(i)?;

            let index = original_i.offset(&i);
            let recognized_section = original_i.slice(..index);
            let spanned_pos: SpannedPosition = recognized_section.into();

            Ok((i, (spanned_pos, o)))
        }
    }

    fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, O>
    where
        F: FnMut(Span<'a>) -> IResult<Span<'a>, O>,
    {
        delimited(space0, inner, space0)
    }

    fn constant(i: Span) -> IResult<Span, Span> {
        delimited(tag("\""), take_until("\""), tag("\""))(i)
    }

    fn variable(i: Span) -> IResult<Span, Span> {
        literal_identifier(i)
    }

    pub fn term(i: Span) -> IResult<Span, IRTerm> {
        alt((
            map(constant, |s| IRTerm::Constant(s.fragment().to_string())),
            map(variable, |s| IRTerm::UserVariable(s.fragment().to_string())),
        ))(i)
    }

    //TODO: I need to think more carefully how to connect this to stage name
    pub fn literal_identifier(i: Span) -> IResult<Span, Span> {
        recognize(pair(
            alpha1,
            many0(alt((alphanumeric1, tag("_"), tag("-")))),
        ))(i)
    }

    /// Parses a literal with a generic term type.
    pub fn literal<'a, FT: 'a, T>(term: FT) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, Literal<T>>
    where
        FT: FnMut(Span<'a>) -> IResult<Span<'a>, T> + Clone,
    {
        move |i| {
            let (i, (spanned_pos, (name, args))) = recognized_span(pair(
                literal_identifier,
                opt(delimited(
                    terminated(tag("("), space0),
                    separated_list1(ws(tag(",")), term.clone()),
                    cut(preceded(space0, tag(")"))),
                )),
            ))(i)?;

            Ok((
                i,
                match args {
                    Some(args) => Literal {
                        position: Some(spanned_pos),
                        predicate: Predicate(name.fragment().to_string()),
                        args,
                    },
                    None => Literal {
                        position: Some(spanned_pos),
                        predicate: Predicate(name.fragment().to_string()),
                        args: Vec::new(),
                    },
                },
            ))
        }
    }

    pub fn clause<'a, FT: 'a, T>(term: FT) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, Clause<T>>
    where
        FT: FnMut(Span) -> IResult<Span, T> + Clone,
    {
        map(
            separated_pair(
                literal(term.clone()),
                ws(tag(":-")),
                separated_list0(ws(tag(",")), literal(term)),
            ),
            |(head, body)| Clause { head, body },
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_term() {
        let inp = "\"\"";

        let expected = IRTerm::Constant("".into());
        let actual: IRTerm = parser::term(Span::new(inp)).unwrap().1;

        assert_eq!(expected, actual);
    }

    #[test]
    fn literals() {
        let l1 = Literal {
            position: None,
            predicate: Predicate("l1".into()),
            args: vec![IRTerm::Constant("c".into())],
        };

        assert_eq!("l1(\"c\")", l1.to_string());

        let actual: Literal = "l1(\"c\")".parse().unwrap();
        assert!(l1.eq_ignoring_position(&actual));
    }

    #[test]
    fn literal_with_variable() {
        let l1 = Literal {
            position: None,
            predicate: Predicate("l1".into()),
            args: vec![
                IRTerm::Constant("".into()),
                IRTerm::UserVariable("X".into()),
            ],
        };

        assert_eq!("l1(\"\", X)", l1.to_string());

        let actual: Literal = "l1(\"\", X)".parse().unwrap();
        assert!(l1.eq_ignoring_position(&actual));
    }

    #[test]
    fn span_of_literal() {
        let spanned_pos = SpannedPosition {
            length: 22,
            offset: 0,
        };

        let actual: Literal = "l1(\"test_constant\", X)".parse().unwrap();
        assert_eq!(Some(spanned_pos), actual.position);
    }

    #[test]
    fn simple_rule() {
        let c = IRTerm::Constant("c".into());
        let va = IRTerm::UserVariable("A".into());
        let vb = IRTerm::UserVariable("B".into());
        let l1 = Literal {
            position: None,
            predicate: Predicate("l1".into()),
            args: vec![va.clone(), vb.clone()],
        };
        let l2 = Literal {
            position: None,
            predicate: Predicate("l2".into()),
            args: vec![va.clone(), c.clone()],
        };
        let l3 = Literal {
            position: None,
            predicate: Predicate("l3".into()),
            args: vec![vb.clone(), c.clone()],
        };
        let r = Clause {
            head: l1,
            body: vec![l2, l3],
        };

        assert_eq!("l1(A, B) :- l2(A, \"c\"), l3(B, \"c\")", r.to_string());

        let actual: Clause = "l1(A, B) :- l2(A, \"c\"), l3(B, \"c\")".parse().unwrap();
        assert!(r.eq_ignoring_position(&actual));
    }

    #[test]
    fn nullary_predicate() {
        let va = IRTerm::UserVariable("A".into());
        let l1 = Literal {
            position: None,
            predicate: Predicate("l1".into()),
            args: Vec::new(),
        };
        let l2 = Literal {
            position: None,
            predicate: Predicate("l2".into()),
            args: vec![va.clone()],
        };
        let r = Clause {
            head: l1,
            body: vec![l2],
        };

        assert_eq!("l1 :- l2(A)", r.to_string());

        let actual: Clause = "l1 :- l2(A)".parse().unwrap();
        assert!(r.eq_ignoring_position(&actual))
    }
}
