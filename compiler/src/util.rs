use crate::ctype::CType;
use pan_parser::ast::{Loc, Identifier, MutOrOwn};
use pan_parser::ast::Expression;
use crate::symboltable::SymbolMutability;

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

pub fn get_attribute_vec(expression: &Expression) -> Vec<(String, Expression)> {
    let mut v: Vec<(String, Expression)> = vec![];

    let mut expr = expression.clone();
    loop {
        if let Expression::Attribute(loc, ex, name, idx) = expr.clone() {
            if name.is_some() {
                v.push((name.as_ref().unwrap().name.clone(), Expression::Error));
            } else {
                v.push((idx.unwrap().to_string(), Expression::Error));
            }

            if let Expression::Attribute(loc, ex2, name2, idx) = *ex.clone() {
                expr = ex.as_ref().clone();
            } else if let Expression::FunctionCall(_, name, ..) = *ex.clone() {
                v.push((name.expr_name(), *ex.clone()));
                //函数插入两边，因为需要取返回值类型，相当于停顿了一下再来
                v.push(("".to_string(), Expression::Error));
            } else {
                v.push((ex.expr_name(), Expression::Error));
                break;
            }
        } else if let Expression::FunctionCall(_, name, ..) = expr.clone() {
            v.push((name.expr_name(), expr.clone()));
            v.push(("".to_string(), Expression::Error));
        } else {
            break;
        }
    }
    v.reverse();
    v
}

pub fn get_mod_name(path: String) -> String {
    let file_name = path.split('/').last().unwrap().to_string();
    let len = file_name.len();
    let a = &file_name[..len - 4];
    return String::from(a);
}


pub fn get_package_name(idents: &Vec<Identifier>) -> String {
    let mut s = idents.iter().fold("".to_string(), |mut ss, s| {
        ss.push_str("$");
        ss.push_str(&s.name);
        return ss;
    });

    return s;
}

pub fn get_full_name(package: &String, s: &str) -> String {
    // let mut tmp = package.clone();
    // tmp.push_str("$");
    // tmp.push_str(s);
    return String::from(s);
    // return tmp;
}

pub fn get_last_name(package: &String) -> String {
    return package.split_terminator("$").last().unwrap().to_string();
}

pub fn get_mutability(mut_or_own: Option<MutOrOwn>, ty: &CType) -> SymbolMutability {
    let mut is_mut = false;
    let mut is_own = false;
    if mut_or_own.is_some() {
        is_mut = mut_or_own.unwrap() == MutOrOwn::Mut;
        is_own = !is_mut;
    }
    let mut is_ref = false;
    if ty >= &CType::Str {
        is_ref = true;
    }
    return if is_ref {
        if is_mut { SymbolMutability::MutRef } else { SymbolMutability::ImmRef }
    } else if is_own { SymbolMutability::Moved } else { if is_mut { SymbolMutability::Mut } else { SymbolMutability::Immutable } };
}






