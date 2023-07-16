mod eval;
mod parser;

use miette::Report;

fn main() -> Result<(), Report> {
    let src: String = "(test->5 arstarst".into();
    match parser::parse(src.as_str()) {
        Ok(expr) => println!("{:?}", expr),
        Err(err) => {
            Err(err)?;
        }
    }
    Ok(())
}
