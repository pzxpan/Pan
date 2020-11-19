use crate::ctype::CType;
use pan_parser::ast::Loc;
use pan_parser::ast::Expression;

pub fn get_number_type(ty: CType) -> i32 {
    return match ty {
        CType::Bool => { 1 }
        CType::Char => { 4 }
        CType::I8 => { -8 }
        CType::U8 => { 8 }
        CType::I16 => { -16 }
        CType::U16 => { 16 }
        CType::I32 => { -32 }
        CType::U32 => { 32 }
        CType::I64 => { -64 }
        CType::U64 => { 64 }
        CType::I128 => { -128 }
        CType::U128 => { 128 }
        CType::Float => { 1000 }
        CType::Str => { 2000 }
        _ => { 20001 }
    };
}

pub fn get_pos_lambda_name(loc: Loc) -> String {
    let mut name = "lambda".to_string();
    name.push_str("_");
    name.push_str(&*loc.1.to_string());
    name.push_str("_");
    name.push_str(&*loc.2.to_string());
    return name;
}

pub fn get_pos_name(name: String, loc: Loc) -> String {
    let mut name = name;
    name.push_str("_");
    name.push_str(&*loc.1.to_string());
    name.push_str("_");
    name.push_str(&*loc.2.to_string());
    return name;
}

pub fn get_attribute_vec(expression: &Expression) -> Vec<String> {
    let mut v = vec![];
    // let mut attri_name = "".to_string();
    // if let Expression::Attribute(_, _, name, ..) = expr {
    //     attri_name = name.as_ref().unwrap().name.clone();
    // }
    let mut expr = expression.clone();
    // Attribute(Loc(1, 29, 29),
    //  Attribute(Loc(1, 29, 24), Variable(Identifier { loc: Loc(1, 29, 18), name: "person" }), Some(Identifier { loc: Loc(1, 29, 24), name: "house" }), None),
    //  Some(Identifier { loc: Loc(1, 29, 29), name: "idea" }), None)
    loop {
        println!("ddd:{:?},", expr);
        if let Expression::Attribute(loc, ex, name, ..) = expr.clone() {
            println!("fuckxxx:{:?}", ex);
            v.push(name.as_ref().unwrap().name.clone());
            if let Expression::Attribute(loc, ex2, name2, ..) = *ex.clone() {
                expr = ex.as_ref().clone();
            } else {
                v.push(ex.expr_name());
                break;
            }
        } else {
            break;
        }
    }
    println!("vvv:{:?}", v);
    v.reverse();
    v
}

pub fn get_mod_name(path: String) -> String {
    let file_name = path.split('/').last().unwrap().to_string();
    let len = file_name.len();
    let a = &file_name[..len - 4];
    return String::from(a);
}





