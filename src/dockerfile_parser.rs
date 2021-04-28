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
use fp_core::compose::*;

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
        one_of
    },
    branch::alt,
    sequence::{pair, delimited, terminated, preceded, tuple},
    multi::{many0, many1, separated_list1},
    combinator::{value, recognize, map, opt, eof},
    error::ParseError,
    IResult
};

#[derive(Clone, PartialEq, Debug)]
pub enum Tag {
    Latest,
    Custom(String)
}

#[derive(Clone, PartialEq, Debug)]
pub struct ParentImage {
    image: String,
    tag: Tag,
}

impl fmt::Display for ParentImage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.tag {
            Tag::Latest => write!(f, "{}", self.image),
            Tag::Custom(s) => write!(f, "{}:{}", self.image, s)
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Instruction {
    From(ParentImage),
    Run(String),
    Cmd(String),
    Label(String),
    Maintainer(String),
    Expose(String),
    Env(String),
    Add(String),
    Copy(String),
    Entrypoint(String),
    Volume(String),
    User(String),
    Workdir(String),
    Arg(String),
    Onbuild(String),
    Stopsignal(String),
    Healthcheck(String),
    Shell(String)
}

#[derive(Clone, PartialEq, Debug)]
pub struct Dockerfile {
    instructions: Vec<Instruction>
}

impl Dockerfile {
    //FIXME: do I need this?
    fn from_instrs(is: Vec<Instruction>) -> Dockerfile {
        Dockerfile{instructions: is}
    }
}

impl str::FromStr for Dockerfile {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match dockerfile(s) {
            Result::Ok((_, o)) => Ok(o),
            Result::Err(e) => Result::Err(format!("{}", e)),
        }
    }
}

impl fmt::Display for Dockerfile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for i in self.instructions.iter() {
            match i {
                Instruction::Arg(s) => write!(f, "ARG {}\n", s),
                Instruction::Copy(s) => write!(f, "COPY {}\n", s),
                Instruction::From(image) => write!(f, "FROM {}\n", image),
                Instruction::Run(s) => write!(f, "RUN {}\n", s),
                Instruction::Env(s) => write!(f, "ENV {}\n", s),
                _ => todo!()
            }?;
        }
        Ok(())
    }
}

fn line_continuation(i: &str) -> IResult<&str, ()> {
    value(
        (), // Output is thrown away.
        tuple((char('\\'), space0, line_ending))
    )(i)
}

fn empty_line(i: &str) -> IResult<&str, ()> {
    value(
        (), // Output is thrown away.
        alt((preceded(space0, line_ending), preceded(space1, eof)))
    )(i)
}

fn ignored_line(i: &str) -> IResult<&str, ()> {
    value(
        (), // Output is thrown away.
        alt((comment_line, empty_line))
    )(i)
}

// one line continuation followed by an arbitrary number of comments
fn continuation_with_comments(i: &str) -> IResult<&str, ()> {
    value(
        (), // Output is thrown away.
        terminated(line_continuation, many0(comment_line))
    )(i)
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
        many0(alt((space, continuation_with_comments)))
    )(i)
}

fn mandatory_space(i: &str) -> IResult<&str, ()> {
    value(
        (), // Output is thrown away.
        delimited(many0(continuation_with_comments), space, optional_space)
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

fn comment(i: &str) -> IResult<&str, &str> {
    preceded(char('#'), is_not("\n\r"))(i)
}

fn comment_line(i: &str) -> IResult<&str, ()> {
    value(
        (), // Output is thrown away.
        delimited(space0, comment, alt((line_ending, eof)))
    )(i)
}

pub fn alias_identifier(i: &str) -> IResult<&str, &str> {
    recognize(
      pair(
        alpha1,
        many0(alt((alphanumeric1, tag("_"), tag("-"))))
      )
    )(i)
}

pub fn image_identifier(i: &str) -> IResult<&str, &str> {
    recognize(
      pair(
        alt((alphanumeric1, tag("_"))),
        many0(alt((alphanumeric1, tag("_"), tag("-"), tag("/"))))
      )
    )(i)
}

pub fn tag_identifier(i: &str) -> IResult<&str, &str> {
    recognize(
        many0(alt((alphanumeric1, tag("_"), tag("-"), tag("."))))
    )(i)
}

//TODO: ${...} can be inside names
fn parent_image(i: &str) -> IResult<&str, ParentImage> {
    map(pair(image_identifier, opt(preceded(tag(":"), tag_identifier))),
    |r| match r {
        (n, None) => ParentImage{ image: n.into(), tag: Tag::Latest },
        (n, Some(t)) => ParentImage{ image: n.into(), tag: Tag::Custom(t.into()) }
    })
    (i)
}

fn multiline_string(i: &str) -> IResult<&str, String> {
    let one_line = map(many1(alt((is_not("\\\n\r"), recognize(tuple((char('\\'), many0(space), is_not(" \n\r"))))))), |s| s.join(""));
    let body = map(separated_list1(many1(line_continuation), one_line), |s| s.join(""));
    preceded(many0(line_continuation), body)(i)
}

fn from_instr(i: &str) -> IResult<&str, Instruction> {
    let body = map(parent_image, Instruction::From);
    preceded(pair(tag_no_case("FROM"), mandatory_space), body)(i)
}

fn env_instr(i: &str) -> IResult<&str, Instruction> {
    let body = map(multiline_string, Instruction::Env);
    preceded(pair(tag_no_case("ENV"), mandatory_space), body)(i)
}

fn copy_instr(i: &str) -> IResult<&str, Instruction> {
    let body = map(multiline_string, Instruction::Copy);
    preceded(pair(tag_no_case("COPY"), mandatory_space), body)(i)
}

fn arg_instr(i: &str) -> IResult<&str, Instruction> {
    let body = map(multiline_string, Instruction::Arg);
    preceded(pair(tag_no_case("ARG"), mandatory_space), body)(i)
}

fn run_instr(i: &str) -> IResult<&str, Instruction> {
    let body = map(multiline_string, Instruction::Run);
    preceded(pair(tag_no_case("RUN"), mandatory_space), body)(i)
}

fn instruction(i: &str) -> IResult<&str, Instruction> {
    alt((
        terminated(from_instr, alt((line_ending, eof))),
        terminated(copy_instr, alt((line_ending, eof))),
        terminated(arg_instr, alt((line_ending, eof))),
        terminated(run_instr, alt((line_ending, eof))),
        terminated(env_instr, alt((line_ending, eof)))
    ))(i)
}

fn dockerfile(i: &str) -> IResult<&str, Dockerfile> {
    map(terminated(many0(preceded(many0(ignored_line), instruction)), many0(ignored_line)), Dockerfile::from_instrs)(i)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ubuntu_latest() -> ParentImage {
        ParentImage{ image: "ubuntu".into(), tag: Tag::Latest }
    }

    fn ubuntu_20_04() -> ParentImage {
        ParentImage{ image: "ubuntu".into(), tag: Tag::Custom("20.04".into()) }
    }

    #[test]
    fn only_simple_from() {
        let f = Instruction::From(ubuntu_latest());
        let e = Dockerfile::from_instrs(vec![f]);
        assert_eq!(Ok(e), "FROM ubuntu\n".parse());
    }

    #[test]
    fn only_from_with_tag() {
        let f = Instruction::From(ubuntu_20_04());
        let e = Dockerfile::from_instrs(vec![f]);
        assert_eq!(Ok(e), "FROM ubuntu:20.04\n".parse());
    }

    #[test]
    fn two_instructions() {
        let f = Instruction::From(ubuntu_latest());
        let r = Instruction::Run("ls".into());
        let e = Dockerfile::from_instrs(vec![f, r]);
        assert_eq!(Ok(e), "FROM ubuntu\nRUN ls\n".parse());
    }

    #[test]
    fn no_newline() {
        let f = Instruction::From(ubuntu_latest());
        let r = Instruction::Run("ls".into());
        let e = Dockerfile::from_instrs(vec![f, r]);
        assert_eq!(Ok(e), "FROM ubuntu\nRUN ls".parse());
    }

    #[test]
    fn empty_line() {
        let f = Instruction::From(ubuntu_latest());
        let r = Instruction::Run("ls".into());
        let e = Dockerfile::from_instrs(vec![f, r]);
        assert_eq!(Ok(e), "\nFROM ubuntu\n  \nRUN ls\n \n".parse());
    }

    #[test]
    fn comment() {
        let f = Instruction::From(ubuntu_latest());
        let e = Dockerfile::from_instrs(vec![f]);
        assert_eq!(Ok(e), "# hello world\nFROM ubuntu".parse());
    }

    #[test]
    fn from_line_continuation_with_comment() {
        let f = Instruction::From(ubuntu_latest());
        let e = Dockerfile::from_instrs(vec![f]);
        assert_eq!(Ok(e), "FROM\\\n# hello world\n ubuntu".parse());
    }

    #[test]
    fn from_line_continuation() {
        let f = Instruction::From(ubuntu_latest());
        let e = Dockerfile::from_instrs(vec![f]);
        assert_eq!(Ok(e), "FROM\\ \n ubuntu".parse());
    }

    #[test]
    fn run_line_continuation_beginning() {
        let f = Instruction::From(ubuntu_latest());
        let r = Instruction::Run("ls -la".into());
        let e = Dockerfile::from_instrs(vec![f, r]);
        assert_eq!(Ok(e), "FROM ubuntu\nRUN\\\n ls -la".parse());
    }

    #[test]
    fn run_line_continuation_middle() {
        let f = Instruction::From(ubuntu_latest());
        let r = Instruction::Run("ls -la".into());
        let e = Dockerfile::from_instrs(vec![f, r]);
        assert_eq!(Ok(e), "FROM ubuntu\nRUN ls \\\n-la".parse());
    }
}
