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

#[derive(Clone, PartialEq, Debug)]
pub struct Image {
    registry: String,
    namespace: String,
    repo: String,
    tag: String,
}

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
pub struct Arg(pub String);

#[derive(Clone, PartialEq, Debug)]
pub struct Workdir(String);

#[derive(Clone, PartialEq, Debug)]
pub enum Instruction<P> {
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
pub struct Dockerfile<P>(pub Vec<Instruction<P>>);

pub type ResolvedDockerfile = Dockerfile<ResolvedParent>;

impl Image {
    pub fn from_repo_tag(repo: String, tag: String) -> Image {
        Image {
            registry: String::new(), 
            namespace: String::new(),
            repo,
            tag
        }
    }
}

impl fmt::Display for Image {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.registry.is_empty() {
            write!(f, "{}/", self.registry)?;
        }
        if !self.namespace.is_empty() {
            write!(f, "{}/", self.namespace)?;
        }
        write!(f, "{}", self.repo)?;
        if self.tag.is_empty() {
            write!(f, ":latest") // being implicit about tag
        } else {
            write!(f, ":{}", self.tag)
        }
    }
}

impl str::FromStr for Image {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match parser::image(s) {
            Result::Ok((_, o)) => Ok(o),
            Result::Err(e) => Result::Err(format!("{}", e)),
        }
    }
}

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
        match parser::dockerfile(s) {
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
                Instruction::Arg(s) => writeln!(f, "ARG {}", s),
                Instruction::Copy(s) => writeln!(f, "COPY {}", s),
                Instruction::From(image) => writeln!(f, "\nFROM {}", image),
                Instruction::Run(s) => writeln!(f, "RUN {}", s),
                Instruction::Env(s) => writeln!(f, "ENV {}", s),
                Instruction::Workdir(s) => writeln!(f, "WORKDIR {}", s),
            }?;
        }
        Ok(())
    }
}

pub mod parser {
    use super::*;

    use nom::{
        IResult,
        branch::alt,
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
        combinator::{eof, map, opt, peek, recognize, value},
        multi::{many0, many1, separated_list1},
        sequence::{pair, delimited, terminated, preceded, tuple}
    };

    //TODO: need to double-check
    pub fn alias_identifier(i: &str) -> IResult<&str, &str> {
        recognize(
        pair(
            alpha1,
            many0(alt((alphanumeric1, tag("_"), tag("-"))))
        )
        )(i)
    }

    //TODO: need to double-check
    pub fn repo_identifier(i: &str) -> IResult<&str, &str> {
        recognize(
        pair(
            alt((alphanumeric1, tag("_"))),
            many0(alt((alphanumeric1, tag("_"), tag("-"), tag("/"))))
        )
        )(i)
    }

    //TODO: need to double-check
    pub fn tag_identifier(i: &str) -> IResult<&str, &str> {
        recognize(
            many0(alt((alphanumeric1, tag("_"), tag("-"), tag("."))))
        )(i)
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
            alt((preceded(space0, line_ending), preceded(space1, peek(eof))))
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
            delimited(space0, comment, alt((line_ending, peek(eof))))
        )(i)
    }


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

    fn docker_instruction(i: &str) -> IResult<&str, Instruction<UnresolvedParent>> {
        alt((
            map(terminated(from_instr, alt((line_ending, peek(eof)))), Instruction::From),
            map(terminated(copy_instr, alt((line_ending, peek(eof)))), Instruction::Copy),
            map(terminated(arg_instr, alt((line_ending, peek(eof)))), Instruction::Arg),
            map(terminated(run_instr, alt((line_ending, peek(eof)))), Instruction::Run),
            map(terminated(env_instr, alt((line_ending, peek(eof)))), Instruction::Env),
            map(terminated(workdir_instr, alt((line_ending, peek(eof)))), Instruction::Workdir)
        ))(i)
    }

    pub fn dockerfile(i: &str) -> IResult<&str, Dockerfile<UnresolvedParent>> {
        map(terminated(many0(preceded(many0(ignored_line), docker_instruction)),
                    terminated(many0(ignored_line), eof)),
            Dockerfile)(i)
    }
 
    pub fn host_identifier(i: &str) -> IResult<&str, &str> {
        recognize(
            delimited(
                many0(alt((alphanumeric1, tag("_"), tag("-")))),
                tag("."), //needs to be at least one dot
                many0(alt((alphanumeric1, tag("_"), tag("-"), tag("."))))
        ))(i)
    }
    
    //FIXME: this is very approximate
    pub fn image(i: &str) -> IResult<&str, Image> {
        map(pair(opt(terminated(host_identifier, tag("/"))),
                pair(opt(recognize(many1(terminated(many0(alt((alphanumeric1, tag("_"), tag("-")))), tag("/"))))),
                     pair(repo_identifier,
                          opt(preceded(tag(":"), tag_identifier))))),
            |(registry, (namespace, (repo, tag)))| Image {
                registry: registry.unwrap_or("").into(),
                namespace: match namespace {
                    Some(s) => {
                        let mut n = s.to_string();
                        n.pop();
                        n
                    },
                    None => String::new()
                },
                repo: repo.into(), 
                tag: tag.unwrap_or("").into()
            }
        )(i)
    }
      
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
        let f = Instruction::From(from_ubuntu_latest());
        let e = Dockerfile(vec![f]);
        assert_eq!(Ok(e), "FROM ubuntu\n".parse());
    }

    #[test]
    fn only_from_with_tag() {
        let f = Instruction::From(from_ubuntu_20_04());
        let e = Dockerfile(vec![f]);
        assert_eq!(Ok(e), "FROM ubuntu:20.04\n".parse());
    }

    #[test]
    fn two_instructions() {
        let f = Instruction::From(from_ubuntu_latest());
        let r = Instruction::Run(Run("ls".into()));
        let e = Dockerfile(vec![f, r]);
        assert_eq!(Ok(e), "FROM ubuntu\nRUN ls\n".parse());
    }

    #[test]
    fn no_newline() {
        let f = Instruction::From(from_ubuntu_latest());
        let r = Instruction::Run(Run("ls".into()));
        let e = Dockerfile(vec![f, r]);
        assert_eq!(Ok(e), "FROM ubuntu\nRUN ls".parse());
    }

    #[test]
    fn empty_line() {
        let f = Instruction::From(from_ubuntu_latest());
        let r = Instruction::Run(Run("ls".into()));
        let e = Dockerfile(vec![f, r]);
        assert_eq!(Ok(e), "\nFROM ubuntu\n  \nRUN ls\n \n".parse());
    }

    #[test]
    fn comment() {
        let f = Instruction::From(from_ubuntu_latest());
        let e = Dockerfile(vec![f]);
        assert_eq!(Ok(e), "# hello world\nFROM ubuntu".parse());
    }

    #[test]
    fn from_line_continuation_with_comment() {
        let f = Instruction::From(from_ubuntu_latest());
        let e = Dockerfile(vec![f]);
        assert_eq!(Ok(e), "FROM\\\n# hello world\n ubuntu".parse());
    }

    #[test]
    fn from_line_continuation() {
        let f = Instruction::From(from_ubuntu_latest());
        let e = Dockerfile(vec![f]);
        assert_eq!(Ok(e), "FROM\\ \n ubuntu".parse());
    }

    #[test]
    fn run_line_continuation_beginning() {
        let f = Instruction::From(from_ubuntu_latest());
        let r = Instruction::Run(Run("ls -la".into()));
        let e = Dockerfile(vec![f, r]);
        assert_eq!(Ok(e), "FROM ubuntu\nRUN\\\n ls -la".parse());
    }

    #[test]
    fn run_line_continuation_middle() {
        let f = Instruction::From(from_ubuntu_latest());
        let r = Instruction::Run(Run("ls -la".into()));
        let e = Dockerfile(vec![f, r]);
        assert_eq!(Ok(e), "FROM ubuntu\nRUN ls \\\n-la".parse());
    }

    #[test]
    fn parse_simple_image() {
        let i = Image::from_repo_tag("ubuntu".into(), "latest".into());
        assert_eq!(Ok(i), "ubuntu:latest".parse());
    }

    #[test]
    fn parse_complex_image() {
        let i = Image {
            registry: "registry.access.redhat.com".into(),
            namespace: "rhel7".into(),
            repo: "rhel".into(), 
            tag: "7.3-53".into()
        };
        assert_eq!(Ok(("", i)), parser::image("registry.access.redhat.com/rhel7/rhel:7.3-53"));
    }

    #[test]
    fn parse_long_namespace() {
        let i = Image {
            registry: "".into(),
            namespace: "a/b/c".into(),
            repo: "r".into(), 
            tag: "".into()
        };
        assert_eq!(Ok(("", i)), parser::image("a/b/c/r"));
    }

}
