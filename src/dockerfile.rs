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

#[derive(Clone, PartialEq, Debug)]
pub struct DockerTag(Tag);

#[derive(Clone, PartialEq, Debug)]
pub enum Tag {
    Latest,
    Custom(String)
}


#[derive(Clone, PartialEq, Debug)]
pub struct DockerParent(Parent);

#[derive(Clone, PartialEq, Debug)]
pub struct Image {
    name: String,
    tag: Tag,
}

impl fmt::Display for Image {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.tag {
            Tag::Latest => write!(f, "{}", self.name),
            Tag::Custom(s) => write!(f, "{}:{}", self.name, s)
        }
    }
}

impl str::FromStr for Image {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match image(s) {
            Result::Ok((_, o)) => Ok(o),
            Result::Err(e) => Result::Err(format!("{}", e)),
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Parent {
    Image(Image),
    ImageOrStage(String),
    Stage(String)
}

impl fmt::Display for Parent {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self {
            Parent::Image(i) => write!(f, "{}", i),
            Parent::Stage(s) => write!(f, "{}", s),
            Parent::ImageOrStage(s) => write!(f, "{}", s)
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct From {
    pub parent: Parent,
    pub alias: Option<String>
}

impl fmt::Display for From {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.alias {
            Some(a) => write!(f, "{} AS {}", self.parent, a),
            None => write!(f, "{}", self.parent)
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Run(String);

impl fmt::Display for Run {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Copy(String);

impl fmt::Display for Copy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Env(String);

impl fmt::Display for Env {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub struct Arg(String);


impl fmt::Display for Arg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum DockerInstruction {
    From(From),
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
    // Workdir(String),
    Arg(Arg),
    // Onbuild(String),
    // Stopsignal(String),
    // Healthcheck(String),
    // Shell(String)
}

#[derive(Clone, PartialEq, Debug)]
pub struct Dockerfile(pub Vec<DockerInstruction>);

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
        for i in self.0.iter() {
            match i {
                DockerInstruction::Arg(s) => write!(f, "ARG {}\n", s),
                DockerInstruction::Copy(s) => write!(f, "COPY {}\n", s),
                DockerInstruction::From(image) => write!(f, "FROM {}\n", image),
                DockerInstruction::Run(s) => write!(f, "RUN {}\n", s),
                DockerInstruction::Env(s) => write!(f, "ENV {}\n", s),
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
//TODO: I need to test alias parsing

pub fn parent(i: &str) -> IResult<&str, Parent> {
    map(pair(image_identifier, opt(preceded(tag(":"), tag_identifier))),
        |(n, ot)| match ot {
            None => Parent::ImageOrStage(n.into()),
            Some(t) => Parent::Image( Image{ name: n.into(), tag: Tag::Custom(t.into()) })
        }
    )(i)
}

pub fn image(i: &str) -> IResult<&str, Image> {
    map(pair(image_identifier, opt(preceded(tag(":"), tag_identifier))),
        |(n, ot)| match ot {
            None => Image{ name: n.into(), tag: Tag::Latest },
            Some(t) => Image{ name: n.into(), tag: Tag::Custom(t.into()) }
        }
    )(i)
}

fn from_content(i: &str) -> IResult<&str, From> {
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

pub fn from_instr(i: &str) -> IResult<&str, From> {
    preceded(pair(tag_no_case("FROM"), mandatory_space), from_content)(i)
}

pub fn env_instr(i: &str) -> IResult<&str, Env> {
    let body = map(multiline_string, |s| Env(s));
    preceded(pair(tag_no_case("ENV"), mandatory_space), body)(i)
}

pub fn copy_instr(i: &str) -> IResult<&str, Copy> {
    let body = map(multiline_string, |s| Copy(s));
    preceded(pair(tag_no_case("COPY"), mandatory_space), body)(i)
}

pub fn arg_instr(i: &str) -> IResult<&str, Arg> {
    let body = map(multiline_string, |s| Arg(s));
    preceded(pair(tag_no_case("ARG"), mandatory_space), body)(i)
}

pub fn run_instr(i: &str) -> IResult<&str, Run> {
    let body = map(multiline_string, |s| Run (s));
    preceded(pair(tag_no_case("RUN"), mandatory_space), body)(i)
}

fn docker_instruction(i: &str) -> IResult<&str, DockerInstruction> {
    alt((
        map(terminated(from_instr, alt((line_ending, eof))), DockerInstruction::From),
        map(terminated(copy_instr, alt((line_ending, eof))), DockerInstruction::Copy),
        map(terminated(arg_instr, alt((line_ending, eof))), DockerInstruction::Arg),
        map(terminated(run_instr, alt((line_ending, eof))), DockerInstruction::Run),
        map(terminated(env_instr, alt((line_ending, eof))), DockerInstruction::Env)
    ))(i)
}

fn dockerfile(i: &str) -> IResult<&str, Dockerfile> {
    map(terminated(many0(preceded(many0(ignored_line), docker_instruction)), many0(ignored_line)), Dockerfile)(i)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn from_ubuntu_latest() -> From {
        From{ parent: Parent::ImageOrStage("ubuntu".into()), alias: None }
    }

    fn from_ubuntu_20_04() -> From {
        From{ parent: Parent::Image( Image { name: "ubuntu".into(), tag: Tag::Custom("20.04".into()) }), alias: None }
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
