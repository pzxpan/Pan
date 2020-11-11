use pan_bytecode::value;
use pan_parser::ast::Expression;
use pan_bytecode::value::Value;

pub fn get_const_value(expr: &Expression) -> Value {
    Value::Nil
    // match expr {
    //     Expression::BoolLiteral(_, value) => Value::Bool(*value),
    //     Expression::StringLiteral( value) => Value::Str(*value),
    // }
}
