use chumsky::error::SimpleReason;
use chumsky::prelude::*;
use miette::Diagnostic;
use miette::LabeledSpan;
use miette::SourceSpan;
use thiserror::Error;

#[derive(Debug)]
pub enum Expr {
    Var(String),
    Call(Vec<Expr>),
    StringLiteral(String),
    ColorLiteral(String),
}

const fn assci_char_set(set: &'static str) -> [bool; 256] {
    let mut out = [false; 256];
    let mut idx = 0;
    let bset = set.as_bytes();
    while idx < bset.len() {
        out[bset[idx] as usize] = true;
        idx += 1;
    }
    out
}

fn in_assci_set(c: char, set: &[bool; 256]) -> bool {
    let true = c.is_ascii()
    else {
      return false;
    };
    set[u32::from(c) as usize]
}

fn is_leading_var_char(c: &char) -> bool {
    const CSET: [bool; 256] = assci_char_set("!$%&*+-./:<=>?@^_~");
    c.is_alphabetic() || in_assci_set(*c, &CSET)
}

fn is_var_char(c: &char) -> bool {
    is_leading_var_char(c) || c.is_ascii_digit()
}

fn varible() -> impl Parser<char, Expr, Error = Simple<char>> {
    filter(is_leading_var_char)
        .chain(filter(is_var_char).repeated())
        .collect()
        .map(Expr::Var)
        .padded()
        .padded_by(comment())
}

fn comment() -> impl Parser<char, (), Error = Simple<char>> + Clone {
    just("//")
        .then(take_until(text::newline()))
        .padded()
        .repeated()
        .ignored()
}

fn string_literal() -> impl Parser<char, Expr, Error = Simple<char>> {
    just("\"")
        .ignore_then(take_until(just("\"").ignored()))
        .map(|t| t.0)
        .collect()
        .map(Expr::StringLiteral)
        .padded()
        .padded_by(comment())
}

fn is_hex_digit(c: &char) -> bool {
    c.is_ascii_hexdigit()
}

fn hex_color_literal() -> impl Parser<char, Expr, Error = Simple<char>> {
    just("#")
        .ignore_then(filter(is_hex_digit).repeated().exactly(6))
        .collect()
        .map(Expr::ColorLiteral)
        .padded()
        .padded_by(comment())
}

pub fn expr() -> impl Parser<char, Vec<Expr>, Error = Simple<char>> {
    recursive(|e| {
        choice((
            varible(),
            string_literal(),
            hex_color_literal(),
            e.delimited_by(just('('), just(')')).map(Expr::Call),
        ))
        .repeated()
        .at_least(1)
    })
    .padded()
    .padded_by(comment())
}

pub fn to_parse_error(source: &str, err: &Simple<char>) -> ParseError {
    match err.reason() {
        SimpleReason::Unexpected => {
            let mut lbls: Vec<_> = err.label().into_iter().map(|_| err.span().into()).collect();
            if lbls.is_empty() && err.found().is_none() {
                lbls = vec![(source.len() - 1..source.len()).into()];
            }
            ParseError::Unexpected {
                found: err.found().copied(),
                expected: err.expected().flatten().cloned().collect(),
                label: lbls.into_iter().next(),
            }
        }
        SimpleReason::Unclosed { span, .. } => ParseError::Unmatched {
            begin: LabeledSpan::at(span.clone(), "Opening paren here"),
            end: LabeledSpan::at(err.span(), "mismatch here"),
        },
        _ => {
            ParseError::Generic {
                msg: err.to_string(),
            }
        }
    }
}

#[derive(Error, Debug, Diagnostic)]
pub enum ParseError {
    #[error("Unexpected Syntax: expected one of {expected:?}, found {found:?}")]
    #[diagnostic(code(parser::unexpected_input))]
    Unexpected {
        found: Option<char>,
        expected: Vec<char>,
        #[label]
        label: Option<SourceSpan>,
    },
    #[error("Unmatched Paren")]
    #[diagnostic(code(parser::unmatched_paren))]
    Unmatched {
        begin: LabeledSpan,
        end: LabeledSpan,
    },

    #[error("Parse Error: {msg}")]
    #[diagnostic(code(parser::generic_error))]
    Generic {
        msg: String,
    },
}

#[derive(Debug, Error, Diagnostic)]
#[error("Parsing Errors")]
#[diagnostic(code(parse_error))]
pub struct ParseErrors {
    #[source_code]
    pub source_code: String,

    #[related]
    pub all: Vec<ParseError>,
}

pub fn parse(source: &str) -> Result<Vec<Expr>, ParseErrors> {
    expr()
        .parse(source)
        .map_err(|errs| ParseErrors {
            source_code: source.into(),
            all: errs.iter().map(|err| to_parse_error(source, err)).collect()})
}
