use chumsky::error::SimpleReason;
use chumsky::prelude::*;
use miette::miette;
use miette::LabeledSpan;
use miette::Report;

#[derive(Debug)]
enum Expr {
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

fn expr() -> impl Parser<char, Vec<Expr>, Error = Simple<char>> {
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

fn render_error(source: &String, err: &Simple<char>) -> Report {
    match err.reason() {
        SimpleReason::Unexpected => {
            let mut expect_tokens = vec![];
            for expected in err.expected() {
                let Some(c) = expected
                else {
                  continue;
                };
                expect_tokens.push(format!("{}", c));
            }
            let expected_msg: String = expect_tokens.as_slice().join(", ");

            let help = if let Some(found) = err.found() {
                format!("expected one of {}, found {}", expected_msg, found)
            } else {
                format!("expected one of {}, found nothing", expected_msg)
            };
            let mut lbls: Vec<_> = err
                .label()
                .into_iter()
                .map(|e| LabeledSpan::at(err.span(), e))
                .collect();
            if lbls.is_empty() && err.found().is_none() {
                lbls = vec![LabeledSpan::at(
                    source.len()-1..source.len(),
                    "at end of input",
                )]
            }
            miette!(labels = lbls, help = help, "Unexpected input").with_source_code(source.clone())
        }
        SimpleReason::Unclosed { .. } => {
            todo!()
        }
        _ => {
            todo!()
        }
    }
}

fn main() -> Result<(), Report> {
    let src: String = "(test->5 arstarst".into();
    println!(
        "{:?}",
        expr()
            .parse(src.as_str())
            .map_err(|err| render_error(&src, &err[0]))?
    );
    Ok(())
}
