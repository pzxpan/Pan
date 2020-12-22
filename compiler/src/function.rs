use pan_parser::ast::{Expression, FunctionDefinition};
use crate::symboltable::{SymbolTableResult, SymbolTableBuilder, SymbolTableError, SymbolUsage};
use crate::ctype::CType;
use pan_parser::ast;
use std::collections::HashMap;
use crate::variable_type::HasType;
use crate::resolve_symbol::{resolve_enum_generic_fn, resolve_function_call_generic, resolve_enum_generic};
use crate::util::get_attribute_vec;
use crate::function_call::function_call;

pub trait fun_attri {
    fn verify_fun_return(&self, builder: &mut SymbolTableBuilder) -> SymbolTableResult;
}

pub trait package_part_fun {
    fn resovle_mod_function(&self, package_name: String, builder: &mut SymbolTableBuilder) -> SymbolTableResult;
}


impl fun_attri for Expression {
    fn verify_fun_return(&self, builder: &mut SymbolTableBuilder) -> SymbolTableResult {
        let mut r_ty = CType::Any;
        let loc = self.loc();
        if self.expr_name().eq("self") {
            let ty = builder.get_self_type().unwrap();
            return if builder.ret_ty.name().eq(&ty.name()) {
                Ok(())
            } else {
                Err(SymbolTableError {
                    error: format!("返回值的Self类型与定义的类型{:?}不匹配", builder.ret_ty),
                    location: loc,
                })
            };
        } else {
            if let ast::Expression::FunctionCall(_, name, args) = self {
                if let ast::Expression::Variable(_) = name.as_ref() {
                    r_ty = builder.get_register_type(name.expr_name())?;
                    if let CType::Enum(ety) = r_ty.clone() {
                        let mut v = HashMap::new();
                        for arg in args.iter().enumerate() {
                            v.insert(name.expr_name(), arg.1.get_type(&builder.tables)?);
                        }
                        r_ty = CType::Enum(resolve_enum_generic_fn(ety, v));
                    } else if let CType::Reference(name, tys) = r_ty.clone() {
                        let ty = builder.get_register_type("self".to_string())?;
                        let r = ty.attri_name_type(name);
                        if r.0 > 0 {
                            r_ty = ty;
                        }
                    }
                    r_ty = r_ty.ret_type().clone();
                } else if let ast::Expression::Attribute(_, n, Some(ident), _) = name.as_ref() {
                    r_ty = builder.get_register_type(n.expr_name())?;
                    r_ty = name.resolve_chained_call(&r_ty, builder)?;
                    // r_ty = builder.resovle_method(name.as_ref(), &r_ty)?;
                    let attri_name = get_attribute_vec(name).last().unwrap().0.clone();
                    let a = r_ty.attri_name_type(attri_name);
                    let mut attri_type_name: Vec<String> = Vec::new();
                    if let CType::Reference(_, nty) = a.1 {
                        for ty in nty.iter() {
                            if let CType::Generic(n, _) = ty {
                                attri_type_name.push(n.clone());
                            }
                        }
                    }
                    if let CType::Enum(ety) = r_ty.clone() {
                        let mut v = HashMap::new();
                        for arg in args.iter().enumerate() {
                            v.insert(attri_type_name.get(arg.0).unwrap().clone(), builder.get_register_type(arg.1.expr_name())?);
                        }
                        r_ty = CType::Enum(resolve_enum_generic_fn(ety, v));
                    }
                    r_ty = r_ty.ret_type().clone();
                }
                if let CType::Args(name, tys) = r_ty.clone() {
                    let mut ty = builder.get_register_type(name)?;
                    if let CType::Generic(name, real_ty) = ty.clone() {
                        ty = real_ty.as_ref().clone();
                    }
                    if ty.is_generic() {
                        if ty.is_fun() {
                            let mut v = Vec::new();
                            for arg in args.iter() {
                                v.push(builder.get_register_type(arg.expr_name())?)
                            }
                            r_ty = resolve_function_call_generic(&CType::Any, &ty, &v, &builder.tables)?;
                        }
                    }
                }
            } else {
                r_ty = self.get_type(&builder.tables)?;
                if let CType::Args(name, tys) = r_ty.clone() {
                    let ty = builder.get_register_type(name)?;
                    if tys.is_empty() {
                        r_ty = ty.clone();
                    }

                    if let CType::Enum(e) = ty {
                        r_ty = CType::Enum(resolve_enum_generic(e, tys));
                    } else if let CType::Fn(e) = ty {
                        //TODO
                    } else if let CType::Struct(e) = ty {
                        //TODO
                    }
                }
            }
        }
        if r_ty != builder.ret_ty {
            if builder.in_lambda {
                let ty = builder.get_register_type(builder.lambda_name.clone())?;
                //  println!("lambda_ty:{:?}", ty);
                return Ok(());
            }
            return Err(SymbolTableError {
                error: format!("函数返回值的类型{:?}与定义的类型{:?}不匹配", r_ty, builder.ret_ty),
                location: loc,
            });
        }
        Ok(())
    }
}

impl package_part_fun for FunctionDefinition {
    fn resovle_mod_function(&self, package_name: String, builder: &mut SymbolTableBuilder) -> SymbolTableResult {
        let tt = self.get_type(&builder.tables)?;
        if let Some(name) = &self.name {
            if let Some(expression) = &self.returns {
                let ty = expression.get_type(&builder.tables)?;
                // self.scan_expression(expression, &ExpressionContext::Load)?;
            }
            builder.ret_ty = tt.ret_type().clone();
            let fun_name = name.name.clone();
            builder.register_package_item(&name.name, tt.clone(), SymbolUsage::Attribute, self.loc, vec![package_name, fun_name])?;
            builder.enter_function(&name.name, false, &self.params, self.loc.1)?;
            if self.body.is_some() {
                builder.scan_statement(&self.body.as_ref().unwrap())?;
                // if def.generics.is_empty() {
                //     self.get_body_return_ty(&def.as_ref().body.as_ref().unwrap(), tt.ret_type(), false)?;
                // }
            }
            builder.leave_scope();
        }
        Ok(())
    }
}
