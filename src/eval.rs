use crate::parser::Expr;
use data_encoding::HEXLOWER_PERMISSIVE;
use levenshtein::levenshtein;
use miette::Diagnostic;
use owo_colors::Style;
use std::collections::HashMap;
use std::ops::Range;
use std::sync::Arc;
use thiserror::Error;

pub type EvalFunc = &'static fn(&[Value]) -> Value;

// This is a high level byte code for a stack
// machine. We might optimize this later into
// something even more efficent if needed.
enum ByteCode<'a> {
    Call(EvalFunc, usize),
    PushConstant(&'a Value),
    PushLiteral(Value),
}

// TODO: Add time, duration, size, maybe other things
pub enum Value {
    Color { r: u8, g: u8, b: u8 },
    Style(Arc<Style>),
    String(Arc<String>),
    Integer(i64),
}

#[derive(Error, Debug, Diagnostic)]
pub enum EvalError {
    #[error("Constant is not registerd")]
    #[diagnostic(code(eval::unregisterd_constant))]
    UnregistredConstant {
        #[help]
        help: Option<String>,

        #[label("constant seen here")]
        constant: Range<usize>,
    },

    #[error("Invalid hex color code")]
    #[diagnostic(code(eval::invalid_hex))]
    InvalidHex {
        #[label("here")]
        hex: Range<usize>,
    },

    #[error("Empty parens are not valid")]
    #[diagnostic(code(eval::empty_parens))]
    EmptyParens {
        #[label("here")]
        parens: Range<usize>,
    },

    #[error("Attempt to call a literal. Only registerd functions can be called.")]
    #[diagnostic(code(eval::call_literal))]
    CallLiteral {
        #[label("here")]
        literal: Range<usize>,
    },

    #[error("Function is not registerd")]
    #[diagnostic(code(eval::unregisterd_function))]
    UnregistredFunction {
        #[help]
        help: Option<String>,

        #[label("function seen here")]
        function: Range<usize>,
    },
}

fn find_closest_match<T>(found: &String, dict: &HashMap<String, T>) -> Option<(String, usize)> {
    dict.keys()
        .map(|s| (s, levenshtein(s, found)))
        .min_by_key(|p| p.1)
        .map(|p| (p.0.clone(), p.1))
}

fn handle_unreg_constant(
    found: &String,
    constants: &HashMap<String, Value>,
    reg: &HashMap<String, EvalFunc>,
) -> Option<String> {
    // First handle mismatched name spaces
    if reg.get(found).is_some() {
        // TODO: Add url to documentation on this
        // TODO: Add an error code help command that can be called
        return Some(format!(
            "{} is a function, not a constant, did you mean to call it?",
            found
        ));
    }

    // Now that we know there isn't a match anywhere lest find the closest match
    let Some((closest_const, cdist)) = find_closest_match(found, constants) else {
        return None;
    };

    // We should also look through functions to see if we find a match there
    let Some((closest_func, fdist)) = find_closest_match(found, reg) else {
        // If there's no best function we can return early
        if (cdist as f64) < 0.33 * (found.len() as f64) {
            return Some(format!("did you mean {} perhaps?", closest_const));
        } else {
            return None;
        }
    };

    if fdist < cdist {
        if (fdist as f64) < 0.33 * (found.len() as f64) {
            return Some(format!(
                "did you mean {} perhaps? note that {} is a function not a constant",
                closest_func, closest_func
            ));
        } else {
            return None;
        }
    } else {
        if (cdist as f64) < 0.33 * (found.len() as f64) {
            return Some(format!("did you mean {} perhaps?", closest_const));
        } else {
            return None;
        }
    }
}

fn handle_unreg_function(
    found: &String,
    constants: &HashMap<String, Value>,
    reg: &HashMap<String, EvalFunc>,
) -> Option<String> {
    // First handle mismatched name spaces
    if constants.get(found).is_some() {
        // TODO: Add url to documentation on this
        // TODO: Add an error code help command that can be called
        return Some(format!(
            "{} is a constant, not a function and cannot be called",
            found
        ));
    }

    // Now that we know there isn't a match anywhere lest find the closest match
    let Some((closest_func, fdist)) = find_closest_match(found, reg) else {
        return None;
    };

    // We should also look through functions to see if we find a match there
    let Some((closest_const, cdist)) = find_closest_match(found, constants) else {
        // If there's no best function we can return early
        if (fdist as f64) < 0.33 * (found.len() as f64) {
            return Some(format!("did you mean {} perhaps?", closest_func));
        } else {
            return None;
        }
    };

    if cdist < fdist {
        if (cdist as f64) < 0.33 * (found.len() as f64) {
            return Some(format!(
                "did you mean {} perhaps? note that {} is a constant not a function",
                closest_const, closest_const
            ));
        } else {
            return None;
        }
    } else {
        if (fdist as f64) < 0.33 * (found.len() as f64) {
            return Some(format!("did you mean {} perhaps?", closest_func));
        } else {
            return None;
        }
    }
}

fn to_byte_code<'a>(
    constants: &'a HashMap<String, Value>,
    reg: &HashMap<String, EvalFunc>,
    expr: &Expr,
    out: &mut Vec<ByteCode<'a>>,
) -> Result<(), EvalError> {
    match expr {
        Expr::Var(name, span) => {
            let Some(value) = constants.get(name) else {
                return Err(EvalError::UnregistredConstant {
                    help: handle_unreg_constant(name, constants, reg),
                    constant: span.clone(),
                });
            };
            out.push(ByteCode::PushConstant(value));
        }
        Expr::Call(vec, span) => {
            if vec.is_empty() {
                return Err(EvalError::EmptyParens {
                    parens: span.clone(),
                });
            }
            let Expr::Var(func_name, func_span) = &vec[0] else {
                return Err(EvalError::CallLiteral {
                    literal: vec[0].span().clone(),
                });
            };
            let Some(eval_func) = reg.get(func_name) else {
                return Err(EvalError::UnregistredFunction {
                    help: handle_unreg_function(func_name, constants, reg),
                    function: func_span.clone(),
                });
            };
            // Push everything.
            for expr in &vec[1..] {
                // TODO: Return multiple errors here
                to_byte_code(constants, reg, expr, out)?;
            }
            out.push(ByteCode::Call(*eval_func, vec.len() - 1));
        }
        Expr::StringLiteral(str, _) => {
            out.push(ByteCode::PushLiteral(Value::String(Arc::new(str.clone()))))
        }
        Expr::ColorLiteral(color, span) => {
            let data = HEXLOWER_PERMISSIVE
                .decode(color.as_bytes())
                .map_err(|_| EvalError::InvalidHex { hex: span.clone() })?;
            if data.len() != 3 {
                return Err(EvalError::InvalidHex { hex: span.clone() });
            }
            let color = Value::Color {
                r: data[0],
                g: data[1],
                b: data[2],
            };
            out.push(ByteCode::PushLiteral(color));
        }
    };
    Ok(())
}
