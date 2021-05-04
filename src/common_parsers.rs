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
