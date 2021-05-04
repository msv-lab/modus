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

use std::fmt;
use std::str;

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

use crate::common_parsers::{
    repo_identifier,
    tag_identifier,
};


#[derive(Clone, PartialEq, Debug)]
pub struct Number(i32); //TODO: use arbitrary-precision arithmetic?

#[derive(Clone, PartialEq, Debug)]
pub struct Text(pub String);

#[derive(Clone, PartialEq, Debug)]
pub struct Image {
    registry: String,
    namespace: String,
    repo: String,
    tag: String,
}

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

#[derive(Clone, PartialEq, Debug)]
pub struct Version {
    major: u32,
    minor: u32,
    patch: u32,
    pre_release: String,
    build: String,
}

impl fmt::Display for Image {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !self.registry.is_empty() {
            write!(f, "{}/", self.registry);
        }
        if !self.namespace.is_empty() {
            write!(f, "{}/", self.namespace);
        }
        write!(f, "{}", self.repo);
        if self.tag.is_empty() {
            write!(f, ":latest") // being implicit about tag
        } else {
            write!(f, ":{}", self.tag)
        }
    }
}

impl fmt::Display for Text {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}/", self.0)
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

pub fn parse_image_literal(s: &str) -> Option<Image> {
    match image_literal(s) {
        Result::Ok((_, o)) => Some(o),
        Result::Err(_) => None,
    }
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

pub fn image_literal(i: &str) -> IResult<&str, Image> {
    delimited(tag("i\""), image, tag("\""))(i)
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_simple_image() {
        let i = Image::from_repo_tag("ubuntu".into(), "latest".into());
        assert_eq!(Ok(i), "ubuntu:latest".parse());
    }

    #[test]
    fn parse_complex_image_literal() {
        let i = Image {
            registry: "registry.access.redhat.com".into(),
            namespace: "rhel7".into(),
            repo: "rhel".into(), 
            tag: "7.3-53".into()
        };
        assert_eq!(Some(i), parse_image_literal("i\"registry.access.redhat.com/rhel7/rhel:7.3-53\""));
    }

    #[test]
    fn parse_long_namespace() {
        let i = Image {
            registry: "".into(),
            namespace: "a/b/c".into(),
            repo: "r".into(), 
            tag: "".into()
        };
        assert_eq!(Some(i), parse_image_literal("i\"a/b/c/r\""));
    }

}