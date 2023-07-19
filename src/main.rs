mod eval;
mod parser;

use miette::Report;
use std::collections::HashMap;

fn main() -> Result<(), Report> {
    let src: String = "(test->5 arstarst".into();
    let ast = parser::parse(src.as_str())?;
    let call = parser::Expr::Call(ast, 0..src.len());
    let mut tp =
        eval::TemplateProgram::new(vec!["a".into()], HashMap::new(), HashMap::new(), &call)?;
    tp.set_var("a", eval::Value::Integer(20));
    dbg!(tp.eval());
    Ok(())
}
