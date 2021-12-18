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

use crate::unification::Rename;
use crate::{modusfile, sld};

use std::convert::TryInto;
use std::fmt;
use std::fmt::{Debug, Display};
use std::rc::Rc;
use std::str;
use std::sync::atomic::{AtomicU32, Ordering};
use std::{collections::HashSet, hash::Hash};

use self::source_span::Span;

impl fmt::Display for IRTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IRTerm::Constant(s) => write!(f, "\"{}\"", s),
            IRTerm::UserVariable(s) => write!(f, "{}", s),
            // there may be aux variables after translating to IR
            IRTerm::AuxiliaryVariable(i) => write!(f, "__AUX_{}", i),
            _ => unimplemented!(),
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
}

pub mod source_span {
    use std::{ops::{Range, RangeFrom, RangeTo}, rc::Rc};

    use nom::{InputIter, InputLength, InputTake, Needed, Slice};
    use nom_locate::LocatedSpan;
    use owned_chars::{OwnedCharIndices, OwnedChars, OwnedCharsExt};

    #[derive(Clone, PartialEq, Eq, Hash, Debug)]
    pub struct SourceSpanType(pub Rc<String>);

    impl From<&SourceSpanType> for String {
        fn from(span: &SourceSpanType) -> Self {
            (*(*span).0).clone()
        }
    }

    impl From<SourceSpanType> for String {
        fn from(span: SourceSpanType) -> Self {
            (*span.0).clone()
        }
    }

    impl From<String> for SourceSpanType {
        fn from(s: String) -> Self {
            SourceSpanType(Rc::new(s))
        }
    }

    impl From<&str> for SourceSpanType {
        fn from(s: &str) -> Self {
            SourceSpanType(Rc::new(s.to_owned().into()))
        }
    }

    impl nom::InputTake for SourceSpanType {
        fn take(&self, count: usize) -> Self {
            self.slice(..count)
        }

        fn take_split(&self, count: usize) -> (Self, Self) {
            (self.slice(count..), self.slice(..count))
        }
    }

    impl nom::InputLength for SourceSpanType {
        fn input_len(&self) -> usize {
            self.0.len()
        }
    }

    impl nom::InputTakeAtPosition for SourceSpanType {
        type Item = char;

        fn split_at_position<P, E: nom::error::ParseError<Self>>(&self, predicate: P) -> nom::IResult<Self, Self, E>
        where
            P: Fn(Self::Item) -> bool {
            match self.position(predicate) {
                Some(n) => Ok((*self).take_split(n)),
                None => Err(nom::Err::Incomplete(nom::Needed::new(1))),
            }
        }

        fn split_at_position1<P, E: nom::error::ParseError<Self>>(
            &self,
            predicate: P,
            e: nom::error::ErrorKind,
        ) -> nom::IResult<Self, Self, E>
        where
            P: Fn(Self::Item) -> bool {
            match self.position(predicate) {
                Some(0) => Err(nom::Err::Error(E::from_error_kind(self.clone(), e))),
                Some(n) => Ok(self.take_split(n)),
                None => Err(nom::Err::Incomplete(nom::Needed::new(1))),
            }
        }

        fn split_at_position_complete<P, E: nom::error::ParseError<Self>>(
            &self,
            predicate: P,
        ) -> nom::IResult<Self, Self, E>
        where
            P: Fn(Self::Item) -> bool {
            match self.split_at_position(predicate) {
                Err(nom::Err::Incomplete(_)) => Ok(self.take_split(self.input_len())),
                res => res,
            }
        }

        fn split_at_position1_complete<P, E: nom::error::ParseError<Self>>(
            &self,
            predicate: P,
            e: nom::error::ErrorKind,
        ) -> nom::IResult<Self, Self, E>
        where
            P: Fn(Self::Item) -> bool {
            match self.position(predicate) {
                Some(0) => Err(nom::Err::Error(E::from_error_kind(self.clone(), e))),
                Some(n) => Ok(self.take_split(n)),
                None => {
                    if self.input_len() == 0 {
                        Err(nom::Err::Error(E::from_error_kind(self.clone(), e)))
                    } else {
                        Ok(self.take_split(self.input_len()))
                    }
                }
            }
        }
    }

    impl nom::Slice<Range<usize>> for SourceSpanType {
        fn slice(&self, range: Range<usize>) -> Self {
            let s: &str = &self.0;
            SourceSpanType(Rc::new(s[range].to_owned()))
        }
    }

    impl nom::Slice<RangeTo<usize>> for SourceSpanType {
        fn slice(&self, range: RangeTo<usize>) -> Self {
            let s: &str = &self.0;
            SourceSpanType(Rc::new(s[range].to_owned()))
        }
    }

    impl nom::Slice<RangeFrom<usize>> for SourceSpanType {
        fn slice(&self, range: RangeFrom<usize>) -> Self {
            let s: &str = &self.0;
            SourceSpanType(Rc::new(s[range].to_owned()))
        }
    }

    impl nom::Compare<&str> for SourceSpanType {
        fn compare(&self, t: &str) -> nom::CompareResult {
            let s1 = self.0.as_str();
            s1.compare(t)
        }

        fn compare_no_case(&self, t: &str) -> nom::CompareResult {
            let s1 = self.0.as_str();
            s1.compare_no_case(t)
        }
    }

    impl nom::InputIter for SourceSpanType {
        type Item = char;
        type Iter = OwnedCharIndices;
        type IterElem = OwnedChars;

        #[inline]
        fn iter_indices(&self) -> Self::Iter {
            (*(*self).0).clone().into_char_indices()
        }

        #[inline]
        fn iter_elements(&self) -> Self::IterElem {
            (*(*self).0).clone().into_chars()
        }

        fn position<P>(&self, predicate: P) -> Option<usize>
        where
            P: Fn(Self::Item) -> bool,
        {
            for (o, c) in self.0.char_indices() {
                if predicate(c) {
                    return Some(o);
                }
            }
            None
        }

        #[inline]
        fn slice_index(&self, count: usize) -> Result<usize, Needed> {
            let mut cnt = 0;
            for (index, _) in self.0.char_indices() {
                if cnt == count {
                    return Ok(index);
                }
                cnt += 1;
            }
            if cnt == count {
                return Ok(self.0.len());
            }
            Err(Needed::Unknown)
        }
    }

    impl nom::Offset for SourceSpanType {
        fn offset(&self, second: &Self) -> usize {
            self.0.offset(&second.0)
        }
    }

    impl nom::FindSubstring<SourceSpanType> for SourceSpanType {
        fn find_substring(&self, substr: SourceSpanType) -> Option<usize> {
            let s: &str = &self.0;
            s.find_substring(&substr.0)
        }
    }

    impl<R: std::str::FromStr> nom::ParseTo<R> for SourceSpanType {
        fn parse_to(&self) -> Option<R> {
            let s: &str = &self.0;
            s.parse().ok()
        }
    }

    impl nom::AsBytes for SourceSpanType {
        fn as_bytes(&self) -> &[u8] {
            self.0.as_bytes()
        }
    }

    pub type Span = LocatedSpan<SourceSpanType>;
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Literal<T = IRTerm> {
    pub position: Option<Span>,
    pub predicate: Predicate,
    pub args: Vec<T>,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Signature(pub Predicate, pub u32);

#[derive(Clone, PartialEq, Debug)]
pub struct Clause<T = IRTerm> {
    pub head: Literal<T>,
    pub body: Vec<Literal<T>>,
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
        return if let IRTerm::Constant(_) = self {
            true
        } else {
            false
        };
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
        let span = Span::new(s.into());
        match parser::clause(parser::term)(span) {
            Result::Ok((_, o)) => Ok(o),
            Result::Err(e) => Result::Err(format!("{}", e)),
        }
    }
}

impl str::FromStr for Literal {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let span = Span::new(s.into());
        match parser::literal(parser::term)(span) {
            Result::Ok((_, o)) => Ok(o),
            Result::Err(e) => Result::Err(format!("{}", e)),
        }
    }
}

/// The parser for the IR is only for convenience in writing tests.
pub mod parser {

    use super::*;

    use nom::{branch::alt, bytes::complete::{is_not, tag}, character::complete::{alpha1, alphanumeric1, one_of, space0}, combinator::{map, opt, recognize, value}, error::VerboseError, multi::{many0, separated_list0, separated_list1}, sequence::{delimited, pair, preceded, separated_pair, terminated}};
    use nom_locate::position;

    /// Redeclaration that uses VerboseError instead of the default nom::Error.
    pub type IResult<T, O> = nom::IResult<T, O, VerboseError<T>>;

    fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(Span) -> IResult<Span, O>
    where
        F: FnMut(Span) -> IResult<Span, O>,
    {
        delimited(space0, inner, space0)
    }

    fn constant(i: Span) -> IResult<Span, Span> {
        delimited(tag("\""), is_not("\""), tag("\""))(i)
    }

    fn variable(i: Span) -> IResult<Span, Span> {
        literal_identifier(i)
    }

    pub fn term(i: Span) -> IResult<Span, IRTerm> {
        alt((
            map(constant, |s| IRTerm::Constant(s.fragment().0.to_string())),
            map(variable, |s| IRTerm::UserVariable(s.fragment().0.to_string())),
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
    pub fn literal<FT: 'static, T>(term: FT) -> impl FnMut(Span) -> IResult<Span, Literal<T>>
    where
        FT: FnMut(Span) -> IResult<Span, T> + Clone,
    {
        move |i| {
            let (i, pos) = position(i)?;

            let x = map(
                pair(
                    literal_identifier,
                    opt(delimited(
                        terminated(tag("("), space0),
                        separated_list1(ws(tag(",")), term.clone()),
                        preceded(space0, tag(")")),
                    )),
                ),
                |(name, args)| match args {
                    Some(args) => Literal {
                        position: Some(pos.clone()),
                        predicate: Predicate(name.fragment().0.to_string()),
                        args,
                    },
                    None => Literal {
                        position: Some(pos.clone()),
                        predicate: Predicate(name.fragment().0.to_string()),
                        args: Vec::new(),
                    },
                },
            )(i); x
        }
    }

    pub fn clause<FT: 'static, T>(term: FT) -> impl FnMut(Span) -> IResult<Span, Clause<T>>
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
        assert_eq!(Ok(r), "l1(A, B) :- l2(A, \"c\"), l3(B, \"c\")".parse());
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
        assert_eq!(Ok(r), "l1 :- l2(A)".parse());
    }
}
