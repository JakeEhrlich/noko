use chumsky::error::SimpleReason;
use chumsky::prelude::*;
use miette::Diagnostic;
use miette::LabeledSpan;
use miette::SourceSpan;
use std::ops::Range;
use thiserror::Error;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Expr<Span = Range<usize>> {
    Var(String, Span),
    Call(Vec<Expr<Span>>, Span),
    StringLiteral(String, Span),
    ColorLiteral(String, Span),
}

impl<Span> Expr<Span> {
    pub fn span(&self) -> &Span {
        match self {
            Expr::Var(_, s) => s,
            Expr::Call(_, s) => s,
            Expr::StringLiteral(_, s) => s,
            Expr::ColorLiteral(_, s) => s,
        }
    }
    #[cfg(test)]
    fn remove_spans(self) -> Expr<()> {
        match self {
            Expr::Var(s, _) => Expr::Var(s, ()),
            Expr::Call(v, _) => Expr::Call(v.into_iter().map(|e| e.remove_spans()).collect(), ()),
            Expr::StringLiteral(s, _) => Expr::StringLiteral(s, ()),
            Expr::ColorLiteral(s, _) => Expr::ColorLiteral(s, ()),
        }
    }
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
    let true = c.is_ascii() else {
        return false;
    };
    set[u32::from(c) as usize]
}

fn is_leading_var_char(c: &char) -> bool {
    const CSET: [bool; 256] = assci_char_set("!$%&*+-.:<=>?@^_~");
    c.is_alphabetic() || in_assci_set(*c, &CSET)
}

fn is_var_char(c: &char) -> bool {
    is_leading_var_char(c) || c.is_ascii_digit()
}

fn varible() -> impl Parser<char, Expr, Error = Simple<char>> {
    filter(is_leading_var_char)
        .chain(filter(is_var_char).repeated())
        .collect()
        .map_with_span(Expr::Var)
        .padded()
        .padded_by(comment())
}

fn comment() -> impl Parser<char, (), Error = Simple<char>> + Clone {
    just("//")
        .then(take_until(text::newline().or(end())))
        .padded()
        .repeated()
        .ignored()
}

fn string_literal() -> impl Parser<char, Expr, Error = Simple<char>> {
    just("\"")
        .ignore_then(take_until(just("\"").ignored()))
        .map(|t| t.0)
        .collect()
        .map_with_span(Expr::StringLiteral)
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
        .map_with_span(Expr::ColorLiteral)
        .padded()
        .padded_by(comment())
}

pub fn expr() -> impl Parser<char, Vec<Expr>, Error = Simple<char>> {
    recursive(|e| {
        choice((
            varible(),
            string_literal(),
            hex_color_literal(),
            e.delimited_by(just('('), just(')'))
                .padded()
                .padded_by(comment())
                .map_with_span(Expr::Call),
        ))
        .repeated()
        .at_least(1)
        .padded()
        .padded_by(comment())
    })
}

pub fn to_parse_error(source: &str, err: &Simple<char>) -> ParseError {
    match err.reason() {
        SimpleReason::Unexpected => {
            let mut lbls: Vec<_> = err.label().into_iter().map(|_| err.span().into()).collect();
            if lbls.is_empty() && err.found().is_none() {
                let start = if source.is_empty() {
                    0
                } else {
                    source.len() - 1
                };
                lbls = vec![(start..source.len()).into()];
            }
            let mut expected: Vec<_> = err.expected().flatten().cloned().collect();
            expected.sort();
            ParseError::Unexpected {
                found: err.found().copied(),
                expected,
                label: lbls.into_iter().next(),
            }
        }
        SimpleReason::Unclosed { span, .. } => ParseError::Unmatched {
            begin: LabeledSpan::at(span.clone(), "Opening paren here"),
            end: LabeledSpan::at(err.span(), "mismatch here"),
        },
        _ => ParseError::Generic {
            msg: err.to_string(),
        },
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
    Generic { msg: String },
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
        .then_ignore(end())
        .parse(source)
        .map_err(|errs| ParseErrors {
            source_code: source.into(),
            all: errs.iter().map(|err| to_parse_error(source, err)).collect(),
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    fn print_ast(ast: &Expr) -> String {
        match ast {
            Expr::Var(name, _) => name.clone(),
            Expr::StringLiteral(lit, _) => format!("\"{}\"", lit),
            Expr::ColorLiteral(lit, _) => format!("#{}", lit),
            Expr::Call(nodes, _) => {
                format!(
                    "({})",
                    nodes.iter().map(print_ast).collect::<Vec<_>>().join(" ")
                )
            }
        }
    }

    #[test]
    fn check_print() {
        insta::assert_debug_snapshot!(print_ast(&parse("((:) (:))").unwrap()[0]));
    }

    fn expr_strategy() -> impl Strategy<Value = Expr> {
        let leaf = prop_oneof![
            // For cases without data, `Just` is all you need
            "[a-zA-Z!$%&*+\\-.:<=>?@\\^_~][a-zA-Z0-9!$%&*+\\-.:<=>?@\\^_~]*"
                .prop_map(|s| Expr::Var(s, 0..0)),
            "[a-fA-F0-9]{6}".prop_map(|s| Expr::ColorLiteral(s, 0..0)),
            "[^\"\\\\]*".prop_map(|s| Expr::StringLiteral(s, 0..0)),
        ];
        // prop::collection::vec(expr_strategy(), 0..10).prop_map(Expr::Call)
        leaf.prop_recursive(
            8,   // 8 levels deep
            256, // Shoot for maximum size of 256 nodes
            10,  // We put up to 10 items per collection
            |inner| {
                prop_oneof![prop::collection::vec(inner, 1..10).prop_map(|v| Expr::Call(v, 0..0))]
            },
        )
    }

    proptest! {
      #![proptest_config(ProptestConfig::with_cases(500))]
      #[test]
      fn is_inverse(expr in expr_strategy()) {
        assert_eq!(vec![expr.clone().remove_spans()],
                   parse(&print_ast(&expr))?
                  .into_iter().map(|e| e.remove_spans())
                  .collect::<Vec<_>>());
      }
    }

    #[test]
    fn check_var() {
        insta::assert_debug_snapshot!(parse("this-is-a-test"));
        insta::assert_debug_snapshot!(parse("  this-is-a-test  "));
        insta::assert_debug_snapshot!(parse("// arstarstarst\nthis-is-a-test//arstarst"));
        insta::assert_debug_snapshot!(parse("this-is-a-test // arstarst //arstrst"));
        insta::assert_debug_snapshot!(parse("   this-is-a-test"));
        insta::assert_debug_snapshot!(parse("\n\nthis-is-a-test\n\n"));
        insta::assert_debug_snapshot!(parse("str->int"));
        insta::assert_debug_snapshot!(parse("+"));
        insta::assert_debug_snapshot!(parse("this-is-a-test//arstarst"));
    }

    #[test]
    fn check_color() {
        insta::assert_debug_snapshot!(parse("#aFaFFA//arstRSTarst arst arst"));
        insta::assert_debug_snapshot!(parse("  #111111  "));
        insta::assert_debug_snapshot!(parse("#a1b2c3"));
        insta::assert_debug_snapshot!(parse("#aaaaaa // arstarst //arstrst"));
        insta::assert_debug_snapshot!(parse("   #aaaaaa"));
        insta::assert_debug_snapshot!(parse("\n\n#aaaaaa\n\n"));
    }

    #[test]
    fn check_str_lit() {
        insta::assert_debug_snapshot!(parse("\"this is a test\""));
        insta::assert_debug_snapshot!(parse("\"\""));
    }

    #[test]
    fn check_call() {
        insta::assert_debug_snapshot!(parse("(this-is-a-test \"RST\" #aaaaaa)"));
        insta::assert_debug_snapshot!(parse("()"));
        insta::assert_debug_snapshot!(parse(""));
        insta::assert_debug_snapshot!(parse("( s s s ) arst RSt"));
        insta::assert_debug_snapshot!(parse("   (arstarst arst arst)"));
        insta::assert_debug_snapshot!(parse("\n\n(test test2)\n\n"));
        insta::assert_debug_snapshot!(parse("(   str->int \"10\"//arst\n)"));
    }

    #[test]
    fn check_nested() {
        insta::assert_debug_snapshot!(parse("((this-is-a-test) \"RST\" #aaaaaa)"));
        insta::assert_debug_snapshot!(parse("((arst arst) arst (arst arst))"));
        insta::assert_debug_snapshot!(parse(" ( (arst))"));
        insta::assert_debug_snapshot!(parse("( (arst) s (arst ) ) arst RSt"));
        insta::assert_debug_snapshot!(parse("((:)(:))"));
    }

    #[test]
    fn chech_negaitve() {
        insta::assert_debug_snapshot!(parse("((this-is-a-test \"RST\" #aaaaaa)"));
        insta::assert_debug_snapshot!(parse("arst arst) arst (arst arst))"));
        insta::assert_debug_snapshot!(parse(" ( (#ufnst))"));
        insta::assert_debug_snapshot!(parse(" ( (#111111111111))"));
        insta::assert_debug_snapshot!(parse("( (#111) s (arst) ) arst RSt"));
    }
}
