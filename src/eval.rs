mod parser;

use parser::Expr;

pub type EvalFunc = fn(Vec<Value>) -> Value;

enum ByteCode {
  Call(EvalFunc),
  PushConstant(Box<Value>),
}

// TODO: Add time, duration, size, maybe other things
pub enum Value {
  Color{r: u8, g: u8, b: u8},
  Style(Arc<Style>),
  String(Arc<String>),
  Integer(i64),
}

// fn to_byte_code(constants: &HashMap<String, Value>, reg: &HashMap<String, EvalFunc>, expr: &Expr) -> Vec<ByteCode> {
//   match expr {
//     Expr::Var(name) => {
              
//     }
//   }
// }

