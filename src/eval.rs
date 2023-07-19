use crate::parser::Expr;
use data_encoding::HEXLOWER_PERMISSIVE;
use levenshtein::levenshtein;
use miette::Diagnostic;
//	se owo_colors::Style;
use smallvec::smallvec;
use smallvec::SmallVec;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::ops::Range;
use std::sync::Arc;
use thiserror::Error;

// TODO: Add error handling
pub type EvalFunc = fn(&[Value]) -> Value;

// This is a high level byte code for a stack
// machine. We might optimize this later into
// something even more efficent if needed.
#[derive(PartialEq)]
enum ByteCode {
    Call(EvalFunc, usize),
    PushConstant(Value),
    PushVar(String),
}

impl std::fmt::Debug for ByteCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ByteCode::Call(_, usize) => write!(f, "call <func> {}", usize),
            ByteCode::PushConstant(v) => write!(f, "pushc {:?}", v),
            ByteCode::PushVar(v) => write!(f, "pushvar {}", v),
        }
    }
}

// TODO: Add time, duration, size, maybe other things
#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Color { r: u8, g: u8, b: u8 },
    //Style(Arc<Style>),
    String(Arc<String>),
    Integer(i64),
}

#[derive(Error, Debug, Diagnostic)]
pub enum EvalError {
    #[error("Value is not registerd")]
    #[diagnostic(code(eval::unregisterd_constant))]
    UnregistredValue {
        #[help]
        help: Option<String>,

        #[label("value seen here")]
        value: Range<usize>,
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

fn find_closest_match<T: AsRef<str>, Iter: Iterator<Item = T>>(
    found: &str,
    iter: Iter,
) -> Option<(String, usize)> {
    iter.map(|s| (s.as_ref().into(), levenshtein(s.as_ref(), found)))
        .min_by_key(|p| p.1)
        .map(|p| (p.0, p.1))
}

fn handle_unreg_value(
    found: &String,
    constants: &HashMap<String, Value>,
    funcs: &HashMap<String, EvalFunc>,
    var: &HashSet<String>,
) -> Option<String> {
    // First handle mismatched name spaces
    if funcs.get(found).is_some() {
        // TODO: Add url to documentation on this
        // TODO: Add an error code help command that can be called
        return Some(format!(
            "'{}' is a function, not a value, did you mean to call it?",
            found
        ));
    }

    // Now that we know there isn't a match anywhere lest find the closest match
    let Some((closest_value, vdist)) = find_closest_match(found, constants.keys().chain(var))
    else {
        return None;
    };

    // We should also look through functions to see if we find a match there
    let Some((closest_func, fdist)) = find_closest_match(found, funcs.keys()) else {
        // If there's no best function we can return early
        if (vdist as f64) < 0.5 * (found.len() as f64) {
            return Some(format!("did you mean '{}' perhaps?", closest_value));
        } else {
            return None;
        }
    };

    if fdist < vdist {
        if (fdist as f64) < 0.5 * (found.len() as f64) {
            Some(format!(
                "did you mean '{}' perhaps? note that '{}' is a function not a value",
                closest_func, closest_func
            ))
        } else {
            None
        }
    } else if (vdist as f64) < 0.5 * (found.len() as f64) {
        Some(format!("did you mean '{}' perhaps?", closest_value))
    } else {
        None
    }
}

fn handle_unreg_function(
    found: &String,
    constants: &HashMap<String, Value>,
    funcs: &HashMap<String, EvalFunc>,
    vars: &HashSet<String>,
) -> Option<String> {
    // First handle mismatched name spaces
    if constants.get(found).is_some() {
        // TODO: Add url to documentation on this
        // TODO: Add an error code help command that can be called
        return Some(format!(
            "'{}' is a value, not a function and cannot be called",
            found
        ));
    }

    if vars.contains(found) {
        // TODO: Add url to documentation on this
        // TODO: Add an error code help command that can be called
        return Some(format!(
            "'{}' is a variable, not a function and cannot be called",
            found
        ));
    }
    // Now that we know there isn't a match anywhere lest find the closest match
    let Some((closest_func, fdist)) = find_closest_match(found, funcs.keys()) else {
        return None;
    };

    // We should also look through functions to see if we find a match there
    let Some((closest_value, vdist)) = find_closest_match(found, constants.keys().chain(vars))
    else {
        // If there's no best function we can return early
        if (fdist as f64) < 0.5 * (found.len() as f64) {
            return Some(format!("did you mean '{}' perhaps?", closest_func));
        } else {
            return None;
        }
    };

    if vdist < fdist {
        if (vdist as f64) < 0.5 * (found.len() as f64) {
            Some(format!(
                "did you mean '{}' perhaps? note that '{}' is a value not a function",
                closest_value, closest_value
            ))
        } else {
            None
        }
    } else if (fdist as f64) < 0.5 * (found.len() as f64) {
        Some(format!("did you mean '{}' perhaps?", closest_func))
    } else {
        None
    }
}

fn to_byte_code(
    constants: &HashMap<String, Value>,
    funcs: &HashMap<String, EvalFunc>,
    vars: &HashSet<String>,
    expr: &Expr,
    out: &mut Vec<ByteCode>,
) -> Result<(), EvalError> {
    match expr {
        Expr::Var(name, span) => {
            let Some(value) = constants.get(name) else {
                if vars.contains(name) {
                    out.push(ByteCode::PushVar(name.clone()));
                    return Ok(());
                }
                return Err(EvalError::UnregistredValue {
                    help: handle_unreg_value(name, constants, funcs, vars),
                    value: span.clone(),
                });
            };
            out.push(ByteCode::PushConstant(value.clone()));
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
            let Some(eval_func) = funcs.get(func_name) else {
                return Err(EvalError::UnregistredFunction {
                    help: handle_unreg_function(func_name, constants, funcs, vars),
                    function: func_span.clone(),
                });
            };
            // Push everything.
            for expr in &vec[1..] {
                // TODO: Return multiple errors here
                to_byte_code(constants, funcs, vars, expr, out)?;
            }
            out.push(ByteCode::Call(*eval_func, vec.len() - 1));
        }
        Expr::StringLiteral(str, _) => {
            out.push(ByteCode::PushConstant(Value::String(Arc::new(str.clone()))))
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
            out.push(ByteCode::PushConstant(color));
        }
    };
    Ok(())
}

// It would be nice if we could make this smaller
// by fitting it in smaller just 16-bits instead
// of 32. I'd say we want to reserve 3-bits for
// instructions, 8 or 9 bits for function indexes,
// and we want to allow up to 5-bits for argument
// sizes, although we could use a utf-8 like encoding
// so that the common case of up to 6 is handled and
// 7 endodes that the following word is an argument length
// (we can do something very similar with function indexes).
// 3 + 5 + 8 == 16 which would work just right. Since thats
// just a little bit tight we would want to consider the "escape"
// hatch for all of those to allow encoding of larger form instructions
#[derive(Clone, Debug, Copy)]
enum LowLevelByteCode {
    Call(u16, u8),
    Pushc(u16),
    Pushv(u16),
}

// This is pure pre-mature optimization over using ByteCode...oh well
pub struct TemplateProgram {
    bc: SmallVec<[LowLevelByteCode; 256]>,
    consts: SmallVec<[Value; 32]>,
    funcs: SmallVec<[EvalFunc; 32]>,
    vars: SmallVec<[Value; 32]>,
    stack: RefCell<SmallVec<[Value; 32]>>,
    var_names: HashMap<String, usize>,
}

// template programs are meant to be constructed once and then
// called over and over again in a tight loop. Variables are chaged
// between calls but even that is meant to be optimized as much
// as possible. (benchmarking needed, currently pre-maturely optimized).
impl TemplateProgram {
    pub fn new(
        vars: Vec<String>,
        consts: HashMap<String, Value>,
        funcs: HashMap<String, EvalFunc>,
        ast: &Expr,
    ) -> Result<TemplateProgram, EvalError> {
        let mut bc = vec![];
        to_byte_code(
            &consts,
            &funcs,
            &vars.iter().cloned().collect(),
            ast,
            &mut bc,
        )?;
        Ok(to_low_level_bytecode(vars, bc))
    }

    pub fn set_var(&mut self, var: &str, value: Value) {
        // This could be optimized a ton. Perfect hash functions,
        // filtering on first letters, etc...could all be used.
        // The set of keys is very small so I wonder if we could
        // just generate a couple constants randomly, test if it
        // makes a perfect hash function, and try again if it
        // fails. That's most likely good enough. Something like
        // take 64-bit chunks (padded with zero bytes), multiply
        // that by some randomly generated number, and then xor
        // that with the running state. The inital state would
        // always be zero.
        if let Some(idx) = self.var_names.get(var) {
            self.vars[*idx] = value;
        }
    }

    pub fn eval(&self) -> Value {
        // First we get a mutable refrence to our stack.
        // it comes pre-allocated and initlized but it
        // may have garbage values siting around inside
        // of it. That's ok, we don't care.
        let mut stack_ref = self.stack.borrow_mut();
        let stack = &mut (*stack_ref)[..];
        let funcs = &self.funcs[..];
        let consts = &self.consts[..];
        let vars = &self.vars[..];
        // The stack grows downwards so that arguments are
        // in the expected order on the stack
        let mut top: usize = stack.len();
        for instr in &self.bc {
            match *instr {
                LowLevelByteCode::Call(idx, num_args) => {
                    // The stack grows backwards so we call with the slice
                    // from the top to the end. This way the implementing
                    // function gets access to the stack directly.
                    let v = funcs[idx as usize](&stack[top..]);
                    // Now we have to "deallocate" the arguments
                    top += num_args as usize;
                    // Next we have to "allocate" space for the answer
                    top -= 1;
                    // Now we can overwirte the latest top of the stack
                    stack[top] = v;
                }
                LowLevelByteCode::Pushc(idx) => {
                    // allocate a spot for the constant
                    top -= 1;
                    stack[top] = consts[idx as usize].clone();
                }
                LowLevelByteCode::Pushv(idx) => {
                    top -= 1;
                    stack[top] = vars[idx as usize].clone();
                }
            }
        }

        stack[top].clone()
    }
}

fn to_low_level_bytecode(vars: Vec<String>, hh_bc: Vec<ByteCode>) -> TemplateProgram {
    let mut bc = smallvec![];
    let mut consts = smallvec![];
    let mut funcs = smallvec![];
    // TODO: Add a value map to dedup consts as well
    let mut func_map = HashMap::new();
    let mut stack_size: usize = 0;
    let mut max_stack_size = stack_size;
    let mut var_names = HashMap::new();
    for (i, var) in vars.into_iter().enumerate() {
        var_names.insert(var, i);
    }
    let vars = smallvec![Value::Integer(0); var_names.len()];
    for instr in hh_bc {
        match instr {
            ByteCode::Call(f, num_args) => {
                stack_size -= 1;
                match func_map.get(&f) {
                    Some(idx) => bc.push(LowLevelByteCode::Call(*idx as u16, num_args as u8)),
                    None => {
                        func_map.insert(f, funcs.len());
                        bc.push(LowLevelByteCode::Call(funcs.len() as u16, num_args as u8));
                        funcs.push(f);
                    }
                }
            }
            ByteCode::PushConstant(v) => {
                bc.push(LowLevelByteCode::Pushc(consts.len() as u16));
                consts.push(v);
                stack_size += 1;
            }
            ByteCode::PushVar(v) => {
                let idx = var_names
                    .get(&v)
                    .expect("first byte code pass should have caught this");
                bc.push(LowLevelByteCode::Pushv(*idx as u16));
                stack_size += 1;
            }
        }
        max_stack_size = std::cmp::max(max_stack_size, stack_size);
    }
    TemplateProgram {
        bc,
        consts,
        funcs,
        vars,
        var_names,
        stack: RefCell::new(smallvec![Value::Integer(0); max_stack_size]),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    //use crate::parser;

    fn head(args: &[Value]) -> Value {
        if args.is_empty() {
            Value::Integer(-1)
        } else {
            args[0].clone()
        }
    }

    fn last(args: &[Value]) -> Value {
        if args.is_empty() {
            Value::Integer(-1)
        } else {
            args[args.len() - 1].clone()
        }
    }

    fn create_env() -> (HashMap<String, Value>, HashMap<String, EvalFunc>) {
        let mut consts = HashMap::new();
        let mut funcs: HashMap<_, EvalFunc> = HashMap::new();
        consts.insert("a".into(), Value::String(Arc::new("a".into())));
        consts.insert(
            "b".into(),
            Value::Color {
                r: 33,
                g: 33,
                b: 33,
            },
        );
        consts.insert("close".into(), Value::Integer(1));
        funcs.insert("head".into(), head);
        funcs.insert("last".into(), last);
        (consts, funcs)
    }

    fn test(code: &str) -> Result<Vec<ByteCode>, EvalError> {
        let ast = crate::parser::parse(code).unwrap()[0].clone();
        let mut bc = vec![];
        let vars = HashSet::new();
        let (consts, funcs) = create_env();
        to_byte_code(&consts, &funcs, &vars, &ast, &mut bc)?;
        Ok(bc)
    }

    #[test]
    fn test_basic() {
        insta::assert_debug_snapshot!(test("a"));
        insta::assert_debug_snapshot!(test("(head a b a)"));
        insta::assert_debug_snapshot!(test("(head a b #ffffff)"));
        insta::assert_debug_snapshot!(test("(last (head) \"this is a str\")"));
    }

    #[test]
    fn test_basic_fail() {
        insta::assert_debug_snapshot!(test("z"));
        insta::assert_debug_snapshot!(test("(str->int a b a)"));
        insta::assert_debug_snapshot!(test("(a b #ffffff)"));
        insta::assert_debug_snapshot!(test("(#ffffff)"));
    }

    #[test]
    fn test_basic_fail_help() {
        insta::assert_debug_snapshot!(test("clope"));
        insta::assert_debug_snapshot!(test("heaf"));
        insta::assert_debug_snapshot!(test("(clope a)"));
        insta::assert_debug_snapshot!(test("(head heaf)"));
    }
}
