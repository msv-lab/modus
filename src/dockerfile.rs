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
        one_of
    },
    branch::alt,
    sequence::{pair, delimited, terminated, preceded, tuple},
    multi::{many0, many1, separated_list1},
    combinator::{value, recognize, map, opt, eof},
    IResult
};

use crate::values::{ Image, image };
use crate::common_parsers::{
    repo_identifier,
    tag_identifier,
    alias_identifier
};


#[derive(Clone, PartialEq, Debug)]
pub enum ResolvedParent {
    Image(Image),
    Stage(String)
}

#[derive(Clone, PartialEq, Debug)]
pub struct UnresolvedParent(String);

#[derive(Clone, PartialEq, Debug)]
pub struct From<P> {
    pub parent: P,
    pub alias: Option<String>
}

#[derive(Clone, PartialEq, Debug)]
pub struct Copy(String);

#[derive(Clone, PartialEq, Debug)]
pub struct Run(String);

#[derive(Clone, PartialEq, Debug)]
pub struct Env(String);

#[derive(Clone, PartialEq, Debug)]
pub struct Arg(String);

#[derive(Clone, PartialEq, Debug)]
pub struct Workdir(String);

#[derive(Clone, PartialEq, Debug)]
pub enum DockerInstruction<P> {
    From(From<P>),
    Run(Run),
    //Cmd(String),
    // Label(String),
    // Maintainer(String),
    // Expose(String),
    Env(Env),
    // Add(String),
    Copy(Copy),
    // Entrypoint(String),
    // Volume(String),
    // User(String),
    Workdir(Workdir),
    Arg(Arg),
    // Onbuild(String),
    // Stopsignal(String),
    // Healthcheck(String),
    // Shell(String)
}

#[derive(Clone, PartialEq, Debug)]
pub struct Dockerfile<P>(pub Vec<DockerInstruction<P>>);

impl<P> fmt::Display for From<P> 
where
    P: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.alias {
            Some(a) => write!(f, "{} AS {}", self.parent, a),
            None => write!(f, "{}", self.parent)
        }
    }
}

impl fmt::Display for ResolvedParent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            ResolvedParent::Image(i) => write!(f, "{}", i),
            ResolvedParent::Stage(s) => write!(f, "{}", s),
        }
    }
}

impl fmt::Display for UnresolvedParent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for Run {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for Copy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for Env {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for Arg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for Workdir {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl str::FromStr for Dockerfile<UnresolvedParent> {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match dockerfile(s) {
            Result::Ok((_, o)) => Ok(o),
            Result::Err(e) => Result::Err(format!("{}", e)),
        }
    }
}

impl<T> fmt::Display for Dockerfile<T> 
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for i in self.0.iter() {
            match i {
                DockerInstruction::Arg(s) => writeln!(f, "ARG {}", s),
                DockerInstruction::Copy(s) => writeln!(f, "COPY {}", s),
                DockerInstruction::From(image) => writeln!(f, "FROM {}", image),
                DockerInstruction::Run(s) => writeln!(f, "RUN {}", s),
                DockerInstruction::Env(s) => writeln!(f, "ENV {}", s),
                DockerInstruction::Workdir(s) => writeln!(f, "WORKDIR {}", s),
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

pub fn ignored_line(i: &str) -> IResult<&str, ()> {
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

pub fn mandatory_space(i: &str) -> IResult<&str, ()> {
    value(
        (), // Output is thrown away.
        delimited(many0(continuation_with_comments), space, optional_space)
    )(i)
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


//TODO: ${...} can be inside names
//TODO: I need to test alias parsing

fn parent(i: &str) -> IResult<&str, UnresolvedParent> {
    map(recognize(image), |s| UnresolvedParent(s.into()))(i)
}

fn from_content(i: &str) -> IResult<&str, From<UnresolvedParent>> {
    map(pair(
        parent,
        opt(map(preceded(delimited(optional_space, tag("AS"), space), alias_identifier), String::from))
        ),
        |(parent, alias)| From{ parent, alias }
    )(i)
}

pub fn multiline_string(i: &str) -> IResult<&str, String> {
    let one_line = map(many1(alt((is_not("\\\n\r"), recognize(tuple((char('\\'), many0(space), is_not(" \n\r"))))))), |s| s.join(""));
    let body = map(separated_list1(many1(line_continuation), one_line), |s| s.join(""));
    preceded(many0(line_continuation), body)(i)
}

pub fn from_instr(i: &str) -> IResult<&str, From<UnresolvedParent>> {
    preceded(pair(tag_no_case("FROM"), mandatory_space), from_content)(i)
}

pub fn env_instr(i: &str) -> IResult<&str, Env> {
    let body = map(multiline_string, Env);
    preceded(pair(tag_no_case("ENV"), mandatory_space), body)(i)
}

pub fn copy_instr(i: &str) -> IResult<&str, Copy> {
    let body = map(multiline_string, Copy);
    preceded(pair(tag_no_case("COPY"), mandatory_space), body)(i)
}

pub fn arg_instr(i: &str) -> IResult<&str, Arg> {
    let body = map(multiline_string, Arg);
    preceded(pair(tag_no_case("ARG"), mandatory_space), body)(i)
}

pub fn run_instr(i: &str) -> IResult<&str, Run> {
    let body = map(multiline_string, Run);
    preceded(pair(tag_no_case("RUN"), mandatory_space), body)(i)
}

pub fn workdir_instr(i: &str) -> IResult<&str, Workdir> {
    let body = map(multiline_string, Workdir);
    preceded(pair(tag_no_case("WORKDIR"), mandatory_space), body)(i)
}

fn docker_instruction(i: &str) -> IResult<&str, DockerInstruction<UnresolvedParent>> {
    alt((
        map(terminated(from_instr, alt((line_ending, eof))), DockerInstruction::From),
        map(terminated(copy_instr, alt((line_ending, eof))), DockerInstruction::Copy),
        map(terminated(arg_instr, alt((line_ending, eof))), DockerInstruction::Arg),
        map(terminated(run_instr, alt((line_ending, eof))), DockerInstruction::Run),
        map(terminated(env_instr, alt((line_ending, eof))), DockerInstruction::Env),
        map(terminated(workdir_instr, alt((line_ending, eof))), DockerInstruction::Workdir)
    ))(i)
}

fn dockerfile(i: &str) -> IResult<&str, Dockerfile<UnresolvedParent>> {
    map(terminated(many0(preceded(many0(ignored_line), docker_instruction)),
                   many0(ignored_line)),
        Dockerfile)(i)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn from_ubuntu_latest() -> From<UnresolvedParent> {
        From{ parent: UnresolvedParent("ubuntu".into()), alias: None }
    }

    fn from_ubuntu_20_04() -> From<UnresolvedParent> {
        From{ parent: UnresolvedParent("ubuntu:20.04".into()), alias: None }
    }

    #[test]
    fn only_simple_from() {
        let f = DockerInstruction::From(from_ubuntu_latest());
        let e = Dockerfile(vec![f]);
        assert_eq!(Ok(e), "FROM ubuntu\n".parse());
    }

    #[test]
    fn only_from_with_tag() {
        let f = DockerInstruction::From(from_ubuntu_20_04());
        let e = Dockerfile(vec![f]);
        assert_eq!(Ok(e), "FROM ubuntu:20.04\n".parse());
    }

    #[test]
    fn two_instructions() {
        let f = DockerInstruction::From(from_ubuntu_latest());
        let r = DockerInstruction::Run(Run("ls".into()));
        let e = Dockerfile(vec![f, r]);
        assert_eq!(Ok(e), "FROM ubuntu\nRUN ls\n".parse());
    }

    #[test]
    fn no_newline() {
        let f = DockerInstruction::From(from_ubuntu_latest());
        let r = DockerInstruction::Run(Run("ls".into()));
        let e = Dockerfile(vec![f, r]);
        assert_eq!(Ok(e), "FROM ubuntu\nRUN ls".parse());
    }

    #[test]
    fn empty_line() {
        let f = DockerInstruction::From(from_ubuntu_latest());
        let r = DockerInstruction::Run(Run("ls".into()));
        let e = Dockerfile(vec![f, r]);
        assert_eq!(Ok(e), "\nFROM ubuntu\n  \nRUN ls\n \n".parse());
    }

    #[test]
    fn comment() {
        let f = DockerInstruction::From(from_ubuntu_latest());
        let e = Dockerfile(vec![f]);
        assert_eq!(Ok(e), "# hello world\nFROM ubuntu".parse());
    }

    #[test]
    fn from_line_continuation_with_comment() {
        let f = DockerInstruction::From(from_ubuntu_latest());
        let e = Dockerfile(vec![f]);
        assert_eq!(Ok(e), "FROM\\\n# hello world\n ubuntu".parse());
    }

    #[test]
    fn from_line_continuation() {
        let f = DockerInstruction::From(from_ubuntu_latest());
        let e = Dockerfile(vec![f]);
        assert_eq!(Ok(e), "FROM\\ \n ubuntu".parse());
    }

    #[test]
    fn run_line_continuation_beginning() {
        let f = DockerInstruction::From(from_ubuntu_latest());
        let r = DockerInstruction::Run(Run("ls -la".into()));
        let e = Dockerfile(vec![f, r]);
        assert_eq!(Ok(e), "FROM ubuntu\nRUN\\\n ls -la".parse());
    }

    #[test]
    fn run_line_continuation_middle() {
        let f = DockerInstruction::From(from_ubuntu_latest());
        let r = DockerInstruction::Run(Run("ls -la".into()));
        let e = Dockerfile(vec![f, r]);
        assert_eq!(Ok(e), "FROM ubuntu\nRUN ls \\\n-la".parse());
    }
}
