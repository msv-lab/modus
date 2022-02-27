// Modus, a language for building container images
// Copyright (C) 2022 University College London

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.

// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <https://www.gnu.org/licenses/>.

use codespan_reporting::diagnostic::Diagnostic;
use codespan_reporting::diagnostic::Label;
use nom::character::complete::line_ending;
use nom::character::complete::not_line_ending;
use nom_supreme::error::BaseErrorKind;
use nom_supreme::error::ErrorTree;

use std::collections::HashSet;
use std::fmt;
use std::str;

use crate::logic;
use crate::logic::parser::Span;
use crate::logic::Predicate;
use crate::logic::SpannedPosition;
use crate::sld;

/// Represents expressions that could be found in the body of a ModusClause.
/// Each enum variant will have some notion of span and whether it's negated.
/// False would mean it is negated.
#[derive(Clone, PartialEq, Debug)]
pub enum Expression {
    Literal(Literal),

    // An operator applied to an expression
    OperatorApplication(Option<SpannedPosition>, Box<Expression>, Operator),

    // A conjunction of expressions.
    And(
        Option<SpannedPosition>,
        bool,
        Box<Expression>,
        Box<Expression>,
    ),

    // A disjunction of expressions.
    Or(
        Option<SpannedPosition>,
        bool,
        Box<Expression>,
        Box<Expression>,
    ),
}

impl Expression {
    #[cfg(test)]
    fn eq_ignoring_position(&self, other: &Expression) -> bool {
        match (self, other) {
            (Expression::Literal(l), Expression::Literal(r)) => l.eq_ignoring_position(&r),
            (
                Expression::OperatorApplication(_, e1, op1),
                Expression::OperatorApplication(_, e2, op2),
            ) => e1.eq_ignoring_position(e2) && op1.eq_ignoring_position(op2),
            (Expression::And(_, p1, l1, r1), Expression::And(_, p2, l2, r2))
            | (Expression::Or(_, p1, l1, r1), Expression::Or(_, p2, l2, r2)) => {
                p1 == p2 && l1.eq_ignoring_position(l2) && r1.eq_ignoring_position(r2)
            }
            (s, o) => s.eq(o),
        }
    }

    pub fn get_spanned_position(&self) -> &Option<SpannedPosition> {
        match self {
            Expression::Literal(lit) => &lit.position,
            Expression::OperatorApplication(s, ..) => &s,
            Expression::And(s, ..) => &s,
            Expression::Or(s, ..) => &s,
        }
    }

    pub fn without_position(&self) -> Self {
        match self {
            Expression::Literal(lit) => Expression::Literal(Literal {
                position: None,
                ..lit.clone()
            }),
            Expression::OperatorApplication(_, e, op) => Expression::OperatorApplication(
                None,
                Box::new(e.without_position()),
                op.clone().with_position(None),
            ),
            Expression::And(_, positive, e1, e2) => Expression::And(
                None,
                positive.clone(),
                Box::new(e1.without_position()),
                Box::new(e2.without_position()),
            ),
            Expression::Or(_, positive, e1, e2) => Expression::Or(
                None,
                positive.clone(),
                Box::new(e1.without_position()),
                Box::new(e2.without_position()),
            ),
        }
    }

    pub fn literals(&self) -> HashSet<Literal> {
        match self {
            Expression::Literal(lit) => vec![lit.clone()].into_iter().collect(),
            Expression::OperatorApplication(_, e, _) => e.literals(),
            Expression::And(_, _, e1, e2) | Expression::Or(_, _, e1, e2) => e1
                .literals()
                .into_iter()
                .chain(e2.literals().into_iter())
                .collect(),
        }
    }

    /// Negates at the current expression level.
    /// So, does not apply De Morgan's laws.
    pub fn negate_current(&self) -> Expression {
        match &self {
            Expression::Literal(lit) => Expression::Literal(Literal {
                positive: !lit.positive,
                ..lit.clone()
            }),
            Expression::And(s, curr_p, l, r) => {
                Expression::And(s.clone(), !curr_p, l.clone(), r.clone())
            }
            Expression::Or(s, curr_p, l, r) => {
                Expression::Or(s.clone(), !curr_p, l.clone(), r.clone())
            }
            Expression::OperatorApplication(..) => {
                panic!("Attempted to negate at operator application level.")
            }
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct ModusClause {
    pub head: Literal,
    // If None, this clause is a fact.
    pub body: Option<Expression>,
}

#[cfg(test)]
impl ModusClause {
    fn eq_ignoring_position(&self, other: &ModusClause) -> bool {
        if let (Some(expr1), Some(expr2)) = (&self.body, &other.body) {
            self.head.eq_ignoring_position(&other.head) && expr1.eq_ignoring_position(&expr2)
        } else {
            self.head.eq_ignoring_position(&other.head) && self.body.eq(&other.body)
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum ModusTerm {
    Constant(String),
    /// A format string with '\$' left unhandled. This should be dealt with when
    /// converting to the IR.
    FormatString {
        /// The position of this term, beginning from the 'f' in the source
        position: SpannedPosition,
        /// The input string literal, as in the source code, i.e. the escape characters
        /// have not been converted.
        format_string_literal: String,
    },
    UserVariable(String),
    AnonymousVariable,
}

impl ModusTerm {
    pub fn is_variable(&self) -> bool {
        match self {
            ModusTerm::FormatString { .. } | ModusTerm::UserVariable(_) => true,
            _ => false,
        }
    }
}

impl fmt::Display for ModusTerm {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ModusTerm::Constant(s) => write!(f, "\"{}\"", s),
            ModusTerm::UserVariable(s) => write!(f, "{}", s),
            ModusTerm::FormatString {
                position: _,
                format_string_literal,
            } => write!(f, "\"{}\"", format_string_literal),
            ModusTerm::AnonymousVariable => write!(f, "_"),
        }
    }
}

impl From<ModusTerm> for logic::IRTerm {
    fn from(modus_term: ModusTerm) -> Self {
        match modus_term {
            ModusTerm::Constant(c) => logic::IRTerm::Constant(c),
            ModusTerm::FormatString {
                position: _,
                format_string_literal: _,
            } => unreachable!("BUG: analysis should've handled this case."),
            ModusTerm::UserVariable(v) => logic::IRTerm::UserVariable(v),
            ModusTerm::AnonymousVariable => sld::Auxiliary::aux(true),
        }
    }
}

impl fmt::Display for logic::Literal<ModusTerm> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.args {
            [] => write!(f, "{}", self.predicate),
            _ => write!(
                f,
                "{}({})",
                self.predicate,
                self.args
                    .iter()
                    .map(|a| a.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

type Literal = logic::Literal<ModusTerm>;

impl From<Literal> for logic::Literal {
    fn from(modus_literal: Literal) -> Self {
        Self {
            positive: modus_literal.positive,
            position: modus_literal.position,
            predicate: modus_literal.predicate,
            args: modus_literal
                .args
                .into_iter()
                .map(|arg| arg.into())
                .collect(),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Operator {
    pub position: Option<SpannedPosition>,
    pub predicate: Predicate,
    pub args: Vec<ModusTerm>,
}

impl Operator {
    #[cfg(test)]
    pub fn eq_ignoring_position(&self, other: &Operator) -> bool {
        self.predicate == other.predicate && self.args == other.args
    }

    pub fn with_position(self, position: Option<SpannedPosition>) -> Operator {
        Operator { position, ..self }
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &*self.args {
            [] => write!(f, "{}", self.predicate),
            _ => write!(
                f,
                "{}({})",
                self.predicate,
                self.args
                    .iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

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

/// Combines nom_supreme's error tree type, codespan's reporting and some custom logic
/// that selects only a subset of a span to produce better error messages.
fn better_convert_error(e: ErrorTree<Span>) -> Vec<Diagnostic<()>> {
    fn generate_base_label(span: &Span, kind: &BaseErrorKind) -> Label<()> {
        let length = if let BaseErrorKind::Expected(nom_supreme::error::Expectation::Tag(t)) = kind
        {
            t.len()
        } else {
            // Default to displaying a single character if we do not know what's expected.
            // (Displaying the full span could be the entire rest of the source file.)
            1
        };
        Label::primary((), span.location_offset()..span.location_offset() + length)
    }

    let mut diags = Vec::new();
    match e {
        ErrorTree::Base { location, kind } => {
            let diag = Diagnostic::error()
                .with_message(kind.to_string())
                .with_labels(vec![generate_base_label(&location, &kind)]);
            diags.push(diag);
        }
        ErrorTree::Stack { base, contexts } => {
            let mut labels = Vec::new();
            let diag;

            let base_range;
            if let ErrorTree::Base { location, kind } = *base {
                labels.push(generate_base_label(&location, &kind));
                base_range = labels[0].range.clone();
                diag = Diagnostic::error().with_message(kind.to_string());
            } else {
                panic!("base of the error stack was not ErrorTree::Base")
            }

            for (span, stack_context) in contexts.iter() {
                labels.push(
                    Label::secondary((), span.location_offset()..base_range.end)
                        .with_message(stack_context.to_string()),
                );
            }
            diags.push(diag.with_labels(labels))
        }
        ErrorTree::Alt(alts) => {
            diags.extend(
                alts.into_iter()
                    .flat_map(|alt_tree| better_convert_error(alt_tree)),
            );
        }
    }
    diags
}

impl str::FromStr for Modusfile {
    type Err = Vec<Diagnostic<()>>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let span = Span::new(s);
        match parser::modusfile(span) {
            Result::Ok((_, o)) => Ok(o),
            Result::Err(nom::Err::Error(e) | nom::Err::Failure(e)) => Err(better_convert_error(e)),
            _ => unimplemented!(),
        }
    }
}

impl str::FromStr for Expression {
    type Err = Vec<Diagnostic<()>>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let span = Span::new(s);
        match parser::body(span) {
            Ok((_, o)) => Ok(o),
            Err(nom::Err::Error(e) | nom::Err::Failure(e)) => Err(better_convert_error(e)),
            _ => unimplemented!(),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::OperatorApplication(_, expr, op) => {
                write!(f, "({})::{}", expr.to_string(), op)
            }
            Expression::Literal(l) => write!(f, "{}", l.to_string()),
            Expression::And(_, positive, expr1, expr2) => {
                // Explicit parenthesization when printing to output, looks a bit
                // verbose but shouldn't affect user code.
                write!(
                    f,
                    "{}({}, {})",
                    if *positive { "" } else { "!" },
                    expr1,
                    expr2
                )
            }
            Expression::Or(_, positive, expr1, expr2) => {
                write!(
                    f,
                    "{}({}; {})",
                    if *positive { "" } else { "!" },
                    expr1,
                    expr2
                )
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
        let span = Span::new(s);
        match parser::modus_clause(span) {
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
        let span = Span::new(s);
        match logic::parser::literal(parser::modus_term, parser::token_sep0)(span) {
            Result::Ok((_, o)) => Ok(o),
            Result::Err(e) => Result::Err(format!("{}", e)),
        }
    }
}

pub mod parser {
    use crate::logic::parser::{literal, literal_identifier, recognized_span, IResult};
    use crate::logic::Predicate;

    use super::*;

    use nom::bytes::complete::{escaped, is_a};
    use nom::character::complete::{multispace0, none_of, one_of};
    use nom::combinator::{cut, opt, recognize};
    use nom::error::context;
    use nom::multi::{many0_count, many1, separated_list1};
    use nom::sequence::{pair, tuple};
    use nom::{
        branch::alt,
        combinator::{eof, map},
        multi::many0,
        sequence::{delimited, preceded, separated_pair, terminated},
    };
    use nom_supreme::tag::complete::tag;

    fn comment(s: Span) -> IResult<Span, Span> {
        recognize(delimited(
            tag("#"),
            opt(not_line_ending),
            alt((line_ending, eof)),
        ))(s)
    }

    #[test]
    fn test_comment_oneline() {
        let input = Span::new("# comment");
        let (rest, _) = comment(input).unwrap();
        assert!(rest.is_empty());
    }

    fn comments(s: Span) -> IResult<Span, Vec<Span>> {
        delimited(
            multispace0,
            many0(terminated(comment, multispace0)),
            multispace0,
        )(s)
    }

    #[test]
    fn test_comments() {
        let s = Span::new("# comment\n# comment\n");
        let (rest, _) = comments(s).unwrap();
        assert!(rest.is_empty());
    }

    #[test]
    fn test_comments_oneline() {
        let s = Span::new("# comment");
        let (rest, _) = comments(s).unwrap();
        assert!(rest.is_empty());
    }

    #[test]
    fn test_comments_empty() {
        let s = Span::new("");
        comments(s).unwrap();
    }

    /// Parse either nothing, or anything that can separate tokens, including
    /// spaces, comments, etc.
    pub fn token_sep0(s: Span) -> IResult<Span, ()> {
        map(comments, |_| ())(s)
    }

    #[test]
    fn test_token_sep0_empty() {
        let s = Span::new("");
        token_sep0(s).unwrap();
    }

    #[test]
    fn test_token_sep0() {
        token_sep0(Span::new(" ")).unwrap();
        token_sep0(Span::new(" \n#")).unwrap();
        token_sep0(Span::new("#\n\n")).unwrap();
        token_sep0(Span::new("   ")).unwrap();
    }

    fn head(i: Span) -> IResult<Span, Literal> {
        context(stringify!(head), literal(modus_term, token_sep0))(i)
    }

    /// Parses `<term1> = <term2>` into a builtin call, `string_eq(term1, term2)`.
    /// Supports the negated version with `!=`.
    fn unification_sugar(i: Span) -> IResult<Span, Literal> {
        map(
            recognized_span(tuple((
                modus_term,
                delimited(token_sep0, alt((tag("!="), tag("="))), token_sep0),
                cut(modus_term),
            ))),
            |(spanned_pos, (t1, op, t2))| Literal {
                positive: op.fragment().len() == 1,
                position: Some(spanned_pos),
                predicate: Predicate("string_eq".to_owned()),
                args: vec![t1, t2],
            },
        )(i)
    }

    fn modus_literal(i: Span) -> IResult<Span, Expression> {
        map(literal(modus_term, token_sep0), Expression::Literal)(i)
    }

    /// Parses a parenthesized expression, taking into account any preceding negation.
    fn parenthesized_expr(i: Span) -> IResult<Span, Expression> {
        let l_paren_with_comments = |i| terminated(tag("("), comments)(i);
        let r_paren_with_comments = |i| preceded(comments, cut(tag(")")))(i);

        map(
            pair(
                many0_count(terminated(nom::character::complete::char('!'), token_sep0)),
                delimited(l_paren_with_comments, body, r_paren_with_comments),
            ),
            |(neg_count, expr)| {
                if neg_count % 2 == 0 {
                    expr
                } else {
                    // negate the expression
                    expr.negate_current()
                }
            },
        )(i)
    }

    /// Parses an operator based on a literal, failing if negation is encountered.
    fn operator(i: Span) -> IResult<Span, Operator> {
        map(
            recognized_span(pair(
                terminated(literal_identifier, token_sep0),
                opt(delimited(
                    terminated(tag("("), token_sep0),
                    separated_list1(
                        terminated(tag(","), token_sep0),
                        terminated(modus_term, token_sep0),
                    ),
                    cut(terminated(tag(")"), token_sep0)),
                )),
            )),
            |(spanned_pos, (name, args))| Operator {
                position: Some(spanned_pos),
                predicate: Predicate(name.fragment().to_string()),
                args: args.unwrap_or(Vec::new()),
            },
        )(i)
    }

    fn expression_inner(i: Span) -> IResult<Span, Expression> {
        let unification_expr_parser = map(unification_sugar, Expression::Literal);
        // These inner expression parsers can fully recurse.
        let op_application_parser = map(
            pair(
                alt((modus_literal, parenthesized_expr)),
                // :: separated list of operators
                many1(recognized_span(preceded(
                    delimited(token_sep0, tag("::"), token_sep0),
                    cut(operator),
                ))),
            ),
            |(expr, ops_with_span)| {
                ops_with_span.into_iter().fold(expr, |acc, (span, op)| {
                    // span for an op application is span for the expression and operator(s)
                    let new_span: Option<SpannedPosition> = acc
                        .get_spanned_position()
                        .as_ref()
                        .map(|s| SpannedPosition {
                            offset: s.offset,
                            length: span.offset + span.length - s.offset,
                        });
                    Expression::OperatorApplication(new_span, Box::new(acc), op)
                })
            },
        );
        alt((
            unification_expr_parser,
            op_application_parser,
            modus_literal,
            parenthesized_expr,
        ))(i)
    }

    pub fn body(i: Span) -> IResult<Span, Expression> {
        let comma_separated_exprs = map(
            separated_list1(delimited(comments, tag(","), comments), expression_inner),
            |es| {
                es.into_iter()
                    .reduce(|e1, e2| {
                        // this was just parsed from source code, so it should have a position
                        let s1 = e1.get_spanned_position().as_ref().unwrap();
                        let s2 = e2.get_spanned_position().as_ref().unwrap();

                        // This span should include the comma/spacing/etc between e1/e2
                        let computed_span = SpannedPosition {
                            offset: s1.offset,
                            length: s2.offset + s2.length - s1.offset,
                        };
                        Expression::And(Some(computed_span), true, Box::new(e1), Box::new(e2))
                    })
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
                    .reduce(|e1, e2| {
                        let s1 = e1.get_spanned_position().as_ref().unwrap();
                        let s2 = e2.get_spanned_position().as_ref().unwrap();

                        let computed_span = SpannedPosition {
                            offset: s1.offset,
                            length: s2.offset + s2.length - s1.offset,
                        };
                        Expression::Or(Some(computed_span), true, Box::new(e1), Box::new(e2))
                    })
                    .expect("Converting list to expression pairs.")
            },
        );
        // Parses the body as a semicolon separated list of comma separated inner expressions.
        // This resolves ambiguity by making commas/and higher precedence.
        preceded(comments, semi_separated_exprs)(i)
    }

    fn fact(i: Span) -> IResult<Span, ModusClause> {
        // Custom definition of fact since datalog facts are normally "head :- ", but Moduslog
        // defines it as "head."
        context(
            stringify!(fact),
            map(
                terminated(
                    head,
                    // NOTE: this is a failure ('cut') assuming the rule parser failed,
                    // however if this is tried *before* the rule parser, this shouldn't be a
                    // failure. This is just one of the subtleties of a parser combinator.
                    cut(terminated(nom::character::complete::char('.'), token_sep0)),
                ),
                |h| ModusClause {
                    head: h,
                    body: None,
                },
            ),
        )(i)
    }

    fn rule(i: Span) -> IResult<Span, ModusClause> {
        context(
            stringify!(rule),
            map(
                separated_pair(
                    head,
                    delimited(token_sep0, tag(":-"), token_sep0),
                    context(
                        "rule_body",
                        cut(terminated(
                            body,
                            terminated(nom::character::complete::char('.'), token_sep0),
                        )),
                    ),
                ),
                |(head, body)| ModusClause {
                    head,
                    body: Some(body),
                },
            ),
        )(i)
    }

    /// Processes the given string, converting escape substrings into the proper characters.
    ///
    /// This also supports string continuation, This allows users to write strings like: "Hello, \
    ///                                                                                   World!"
    /// which is actually just "Hello, World!".
    pub fn process_raw_string(s: &str) -> String {
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

    const STRING_ESCAPE_CHARS: &str = "\"\\nrt0\n";
    const FORMAT_STRING_ESCAPE_CHARS: &str = "$\"\\nrt0\n";

    /// Parses a string that possibly contains escaped characters, but doesn't actually
    /// convert the escape characters.
    fn string_content(i: Span) -> IResult<Span, String> {
        let escape_parser = escaped(none_of("\\\""), '\\', one_of(STRING_ESCAPE_CHARS));
        let (i, o) = opt(escape_parser)(i)?;
        let parsed_str: &str = o.map(|span| *span.fragment()).unwrap_or("");
        Ok((i, parsed_str.to_owned()))
    }

    fn format_string_content(i: Span) -> IResult<Span, String> {
        let escape_parser = escaped(none_of("\\\""), '\\', one_of(FORMAT_STRING_ESCAPE_CHARS));
        let (i, o) = opt(escape_parser)(i)?;
        let parsed_str: &str = o.map(|span| *span.fragment()).unwrap_or("");
        Ok((i, parsed_str.to_owned()))
    }

    pub fn modus_const(i: Span) -> IResult<Span, String> {
        context(
            stringify!(modus_const),
            delimited(tag("\""), string_content, cut(tag("\""))),
        )(i)
    }

    /// Parses a substring outside of the expansion part of a format string's content.
    pub fn outside_format_expansion(i: Span) -> IResult<Span, Span> {
        // We want to parse until we see '$', except if it was preceded with escape char.
        recognize(opt(escaped(none_of("\\$"), '\\', one_of("$\"\\nrt0\n"))))(i)
    }

    fn modus_format_string(i: Span) -> IResult<Span, (SpannedPosition, String)> {
        context(
            stringify!(modus_format_string),
            // TODO: check that the token(s) inside "${...}" conform to the variable syntax.
            // Note that if it's escaped, "\${...}", it does not need to conform to the variable syntax.
            recognized_span(delimited(tag("f\""), format_string_content, cut(tag("\"")))),
        )(i)
    }

    pub fn variable_identifier(i: Span) -> IResult<Span, Span> {
        literal_identifier(i)
    }

    fn modus_var(i: Span) -> IResult<Span, Span> {
        context(stringify!(modus_var), variable_identifier)(i)
    }

    pub fn string_interpolation(i: Span) -> IResult<Span, Span> {
        delimited(
            terminated(tag("${"), token_sep0),
            modus_var,
            cut(preceded(token_sep0, tag("}"))),
        )(i)
    }

    pub fn modus_term(i: Span) -> IResult<Span, ModusTerm> {
        alt((
            map(modus_const, ModusTerm::Constant),
            map(modus_format_string, |(position, format_string_literal)| {
                ModusTerm::FormatString {
                    position,
                    format_string_literal,
                }
            }),
            map(is_a("_"), |_| ModusTerm::AnonymousVariable),
            map(modus_var, |s| {
                ModusTerm::UserVariable(s.fragment().to_string())
            }),
        ))(i)
    }

    pub fn modus_clause(i: Span) -> IResult<Span, ModusClause> {
        alt((rule, fact))(i)
    }

    pub fn modusfile(i: Span) -> IResult<Span, Modusfile> {
        map(
            terminated(
                many0(preceded(token_sep0, modus_clause)),
                terminated(token_sep0, eof),
            ),
            Modusfile,
        )(i)
    }
}

#[cfg(test)]
mod tests {
    use rand::Rng;
    use serial_test::serial;

    use super::*;

    type Rule = ModusClause;

    #[test]
    fn fact() {
        let l1 = Literal {
            positive: true,
            position: None,
            predicate: logic::Predicate("l1".into()),
            args: Vec::new(),
        };
        let c = ModusClause {
            head: l1,
            body: None,
        };

        assert_eq!("l1.", c.to_string());

        let actual: ModusClause = "l1.".parse().unwrap();
        assert!(c.eq_ignoring_position(&actual));
    }

    #[test]
    fn rule() {
        let l1 = Literal {
            positive: true,
            position: None,
            predicate: logic::Predicate("l1".into()),
            args: Vec::new(),
        };
        let l2 = Literal {
            positive: true,
            position: None,
            predicate: logic::Predicate("l2".into()),
            args: Vec::new(),
        };
        let l3 = Literal {
            positive: true,
            position: None,
            predicate: logic::Predicate("l3".into()),
            args: Vec::new(),
        };
        let c = Rule {
            head: l1,
            body: Expression::And(None, true, Box::new(l2.into()), Box::new(l3.into())).into(),
        };

        assert_eq!("l1 :- (l2, l3).", c.to_string());

        let actual1: Rule = "l1 :- l2, l3.".parse().unwrap();
        assert!(c.eq_ignoring_position(&actual1));
        let actual2: Rule = "l1 :- l2,\n\tl3.".parse().unwrap();
        assert!(c.eq_ignoring_position(&actual2));
    }

    #[test]
    fn rule_with_or() {
        let l1: Literal = "l1".parse().unwrap();
        let l2: Literal = "l2".parse().unwrap();
        let c = Rule {
            head: "foo".parse().unwrap(),
            body: Expression::Or(None, true, Box::new(l1.into()), Box::new(l2.into())).into(),
        };

        assert_eq!("foo :- (l1; l2).", c.to_string());

        let actual: Rule = "foo :- l1; l2.".parse().unwrap();
        assert!(c.eq_ignoring_position(&actual));
    }

    #[test]
    fn rule_with_operator() {
        let foo = Literal {
            positive: true,
            position: None,
            predicate: logic::Predicate("foo".into()),
            args: Vec::new(),
        };
        let a = Literal {
            positive: true,
            position: None,
            predicate: logic::Predicate("a".into()),
            args: Vec::new(),
        };
        let b = Literal {
            positive: true,
            position: None,
            predicate: logic::Predicate("b".into()),
            args: Vec::new(),
        };
        let merge = Operator {
            position: None,
            predicate: logic::Predicate("merge".into()),
            args: Vec::new(),
        };
        let r1 = Rule {
            head: foo.clone(),
            body: Expression::OperatorApplication(
                None,
                Expression::And(None, true, Box::new(a.clone().into()), Box::new(b.into())).into(),
                merge.clone(),
            )
            .into(),
        };
        let r2 = Rule {
            head: foo,
            body: Expression::OperatorApplication(None, Box::new(Expression::Literal(a)), merge)
                .into(),
        };

        assert_eq!("foo :- ((a, b))::merge.", r1.to_string());
        let actual1 = "foo :- ((a, b))::merge.".parse().unwrap();
        assert!(r1.eq_ignoring_position(&actual1));
        let actual2 = "foo :- (a, b)::merge.".parse().unwrap();
        assert!(r1.eq_ignoring_position(&actual2));

        assert_eq!("foo :- (a)::merge.", r2.to_string());
        let actual3 = "foo :- a::merge.".parse().unwrap();
        assert!(r2.eq_ignoring_position(&actual3));
        let actual3 = "foo :- ( a )::merge.".parse().unwrap();
        assert!(r2.eq_ignoring_position(&actual3));
    }

    #[test]
    #[serial]
    fn modusclause_to_clause() {
        crate::translate::reset_operator_pair_id();
        let foo = Literal {
            positive: true,
            position: None,
            predicate: logic::Predicate("foo".into()),
            args: Vec::new(),
        };
        let a = Literal {
            positive: true,
            position: None,
            predicate: logic::Predicate("a".into()),
            args: Vec::new(),
        };
        let b = Literal {
            positive: true,
            position: None,
            predicate: logic::Predicate("b".into()),
            args: Vec::new(),
        };
        let merge = Operator {
            position: None,
            predicate: logic::Predicate("merge".into()),
            args: Vec::new(),
        };
        let r = Rule {
            head: foo,
            body: Expression::OperatorApplication(
                None,
                Expression::And(None, true, Box::new(a.into()), Box::new(b.into())).into(),
                merge,
            )
            .into(),
        };
        assert_eq!("foo :- ((a, b))::merge.", r.to_string());

        // Convert to the simpler syntax
        let c: Vec<logic::Clause> = (&r).into();
        assert_eq!(1, c.len());
        assert_eq!(
            r#"foo :- _operator_merge_begin("0"), a, b, _operator_merge_end("0")"#,
            c[0].to_string()
        );
    }

    #[test]
    #[serial]
    fn modusclause_to_clause_with_or() {
        crate::translate::reset_operator_pair_id();
        let foo: Literal = "foo".parse().unwrap();
        let a: Literal = "a".parse().unwrap();
        let b: Literal = "b".parse().unwrap();
        let merge = Operator {
            position: None,
            predicate: logic::Predicate("merge".into()),
            args: Vec::new(),
        };
        let r1 = Rule {
            head: foo.clone(),
            body: Expression::OperatorApplication(
                None,
                Expression::Or(
                    None,
                    true,
                    Box::new(a.clone().into()),
                    Box::new(b.clone().into()),
                )
                .into(),
                merge,
            )
            .into(),
        };
        let r2 = Rule {
            head: foo.clone(),
            body: Expression::And(
                None,
                true,
                Box::new(a.clone().into()),
                Box::new(Expression::And(
                    None,
                    true,
                    Box::new(b.clone().into()),
                    Box::new(Expression::Or(
                        None,
                        true,
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
        assert_eq!(
            r#"foo :- _operator_merge_begin("0"), a, _operator_merge_end("0")"#,
            c1[0].to_string()
        );
        assert_eq!(
            r#"foo :- _operator_merge_begin("1"), b, _operator_merge_end("1")"#,
            c1[1].to_string()
        );

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
        let (_, s1) = parser::modus_const(Span::new(inp1.into())).unwrap();
        let (_, s2) = parser::modus_const(Span::new(inp2.into())).unwrap();
        let (_, s3) = parser::modus_const(Span::new(inp3.into())).unwrap();

        assert_eq!(s1, r#"Hello\nWorld"#);
        assert_eq!(s2, r#"Tabs\tare\tbetter\tthan\tspaces"#);
        assert_eq!(
            s3,
            r#"Testing \
                       multiline."#
        );
    }

    #[test]
    fn anonymous_variables() {
        let expected = Literal {
            positive: true,
            position: None,
            predicate: Predicate("l".into()),
            args: vec![
                ModusTerm::Constant("foo".to_string()),
                ModusTerm::AnonymousVariable,
            ],
        };
        let actual: Literal = "l(\"foo\", _)".parse().unwrap();
        assert!(expected.eq_ignoring_position(&actual));
    }

    #[test]
    fn modus_expression() {
        let a: Literal = "a".parse().unwrap();
        let b: Literal = "b".parse().unwrap();
        let c: Literal = "c".parse().unwrap();
        let d: Literal = "d".parse().unwrap();

        let e1 = Expression::And(
            None,
            true,
            Expression::Literal(a).into(),
            Expression::Literal(b).into(),
        );
        let e2 = Expression::And(
            None,
            true,
            Expression::Literal(c).into(),
            Expression::Literal(d).into(),
        );

        let expr = Expression::Or(None, false, e1.into(), e2.into());

        let expr_str = "!((a, b); (c, d))";
        assert_eq!(expr_str, expr.to_string());

        let rule: ModusClause = format!("foo :- {}.", expr_str).parse().unwrap();
        assert!(expr.eq_ignoring_position(&rule.body.unwrap()))
    }

    #[test]
    fn modus_unification() {
        let modus_clause: ModusClause = "foo(X, Y, A, B) :- X = Y, A != B.".parse().unwrap();
        let expected_body = Expression::And(
            None,
            true,
            Box::new(Expression::Literal("string_eq(X, Y)".parse().unwrap())),
            Box::new(Expression::Literal("!string_eq(A, B)".parse().unwrap())),
        );
        assert!(expected_body.eq_ignoring_position(&modus_clause.body.unwrap()));
    }

    #[test]
    fn modus_negated_unification() {
        let inp = "foo(X, Y) :- X != Y.";
        let expected_lit: Literal = "!string_eq(X, Y)".parse().unwrap();
        let actual: Expression = inp.parse().map(|r: ModusClause| r.body).unwrap().unwrap();
        assert!(Expression::Literal(expected_lit).eq_ignoring_position(&actual));
    }

    #[test]
    fn multiple_clause_with_different_ops() {
        let foo = Literal {
            positive: true,
            position: None,
            predicate: logic::Predicate("foo".into()),
            args: vec![ModusTerm::UserVariable("x".to_owned())],
        };
        let bar = Literal {
            positive: true,
            position: None,
            predicate: logic::Predicate("bar".into()),
            args: Vec::new(),
        };
        let baz = Literal {
            positive: true,
            position: None,
            predicate: logic::Predicate("baz".into()),
            args: Vec::new(),
        };
        let a = Rule {
            head: logic::Literal {
                positive: true,
                position: None,
                predicate: logic::Predicate("a".to_owned()),
                args: vec![],
            },
            body: Some(Expression::And(
                None,
                true,
                Box::new(Expression::OperatorApplication(
                    None,
                    Box::new(Expression::And(
                        None,
                        true,
                        Box::new(foo.into()),
                        Box::new(bar.into()),
                    )),
                    Operator {
                        position: None,
                        predicate: logic::Predicate("setenv".into()),
                        args: vec![
                            ModusTerm::Constant("a".to_owned()),
                            ModusTerm::Constant("foobar".to_owned()),
                        ],
                    },
                )),
                Box::new(Expression::OperatorApplication(
                    None,
                    Box::new(baz.into()),
                    Operator {
                        position: None,
                        predicate: logic::Predicate("setenv".into()),
                        args: vec![
                            ModusTerm::Constant("a".to_owned()),
                            ModusTerm::Constant("baz".to_owned()),
                        ],
                    },
                )),
            )),
        };

        let actual: Rule = r#"a:-(foo(x),bar)::setenv("a","foobar"), (baz)::setenv("a", "baz")."#
            .parse()
            .unwrap();
        assert!(a.eq_ignoring_position(&actual));
    }

    #[test]
    fn op_application_chained_with_spaces() {
        let r1: Rule = "a :- foo::set_env::in_env.".parse().unwrap();
        let r2: Rule = "a :- foo :: set_env :: in_env.".parse().unwrap();
        let r3: Rule = "a :- foo\n::\nset_env\n::\nin_env.".parse().unwrap();

        let expected = Rule {
            head: logic::Literal {
                positive: true,
                position: None,
                predicate: logic::Predicate("a".to_owned()),
                args: vec![],
            },
            body: Some(Expression::OperatorApplication(
                None,
                Box::new(Expression::OperatorApplication(
                    None,
                    Box::new(Expression::Literal(logic::Literal {
                        positive: true,
                        position: None,
                        predicate: logic::Predicate("foo".into()),
                        args: Vec::new(),
                    })),
                    Operator {
                        position: None,
                        predicate: logic::Predicate("set_env".into()),
                        args: Vec::new(),
                    },
                )),
                Operator {
                    position: None,
                    predicate: logic::Predicate("in_env".into()),
                    args: Vec::new(),
                },
            )),
        };

        assert!(expected.eq_ignoring_position(&r1));
        assert!(expected.eq_ignoring_position(&r2));
        assert!(expected.eq_ignoring_position(&r3));
    }

    #[test]
    fn test_spaces() {
        fn add_spaces(tokens: &[&str], spaces: &[&str]) -> String {
            let mut s = String::new();
            let mut rng = rand::thread_rng();
            for token in tokens {
                if token.is_empty() {
                    continue;
                }
                if spaces.len() == 1 {
                    s.push_str(spaces[0]);
                } else {
                    s.push_str(spaces[rng.gen_range(0..spaces.len())]);
                }
                s.push_str(token.trim());
            }
            s
        }

        fn do_test(lines: &str) {
            let tokens = lines.lines().collect::<Vec<_>>();
            fn should_parse(s: &str) {
                if let Err(e) = s.parse::<Modusfile>() {
                    panic!(
                        "Failed to parse: Error:\n{e:?}\nModusfile:\n{s}",
                        e = e,
                        s = s
                    );
                }
            }
            should_parse(&add_spaces(&tokens, &[""]));
            should_parse(&add_spaces(&tokens, &[" "]));
            should_parse(&add_spaces(&tokens, &["\n"]));
            should_parse(&add_spaces(&tokens, &["# Comment\n"]));
            should_parse(&add_spaces(&tokens, &["\n# Comment\n"]));
            should_parse(&add_spaces(&tokens, &["", " ", "\n", "\n# Comment\n"]));
        }

        do_test(
            r#"
            final
            :-
            from
            (
            "alpine"
            )
            .
        "#,
        );

        do_test(
            r#"
            final
            :-
            from
            (
            "alpine"
            )
            ::
            set_workdir
            (
            "/tmp"
            )
            ,
            run
            (
            "pwd"
            )
            .
        "#,
        );

        do_test(
            r#"
            final
            :-
            a
            ,
            b
            ,
            c
            ;
            d
            ,
            (
            e
            ,
            f
            (
            V1
            ,
            V2
            )
            ,
            V1
            =
            V2
            )
            ;
            g
            ,
            h
            ::i
            ,
            j
            .
        "#,
        )
    }

    #[test]
    fn reports_error_in_rule() {
        let modus_file: Result<Modusfile, Vec<Diagnostic<()>>> =
            "foo(X) :- bar(X), baz(X), .".parse();
        assert!(modus_file.is_err());

        let diags = modus_file.err().unwrap();
        assert_eq!(1, diags.len());
        assert_eq!(
            diags[0].severity,
            codespan_reporting::diagnostic::Severity::Error
        );
        assert!(diags[0].labels[1].message.contains("body"));
        assert!(diags[0].labels[2].message.contains("rule"));
    }
}
