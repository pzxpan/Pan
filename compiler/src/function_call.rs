use pan_parser::ast::{Expression, FunctionDefinition};
use crate::symboltable::{SymbolTableResult, SymbolTableBuilder, SymbolTableError, SymbolUsage};
use crate::ctype::CType;
use pan_parser::ast;
use crate::variable_type::HasType;
use crate::util::{get_attribute_vec, get_package_layer};

pub trait function_call {
    fn resolve_function_call(&self, ty: &CType, builder: &mut SymbolTableBuilder) -> SymbolTableResult;
    //a.b.c();
    fn resolve_chained_call(&self, ty: &CType, builder: &mut SymbolTableBuilder) -> Result<CType, SymbolTableError>;
}

impl function_call for Expression {
    fn resolve_function_call(&self, ty: &CType, builder: &mut SymbolTableBuilder) -> SymbolTableResult {
        if let ast::Expression::FunctionCall(_, name, args) = self {
            let args_type = ty.param_type();
            let len = args_type.len();
            // println!("real_ty:{:?},expected_ty:{:?},", args, args_type);
            for (i, (ety, is_default, is_varargs, ref_mut)) in args_type.iter().enumerate() {
                if let Some(e) = args.get(i) {
                    let mut cty = e.get_type(&builder.tables)?;
                    if let Expression::FunctionCall(..) = e {
                        cty = cty.ret_type().clone();
                    }
                    if ety != &cty {
                        if let CType::Fn(f) = ety.clone() {
                            if let CType::Lambda(l) = cty.clone() {
                                if f.get_fn_args_ret_str().eq(&l.get_fn_args_ret_str()) {
                                    continue;
                                }
                            }
                        }
                        if ety != &CType::Any {
                            if *is_varargs {
                                if let CType::Array(_) = ety {
                                    continue;
                                }
                            }
                            //TODO 引用自身的类型;
                            if let CType::Args(s, _) = ety.clone() {
                                if cty.clone() == builder.get_register_type(s.clone())? {
                                    continue;
                                }
                            }
                            //Todo ety如果是无参函数类型，让其过，没搞明白，这里要咋样处理,是否允许定义函数类型时带参数呢，还是只能推导;以函数为参数的函数，其函数参数如何确定，需要处理;

                            if let CType::Generic(_, fnty) = ety {
                                // println!("fnty:{:?},cty:{:?}", fnty, cty);
                                if fnty.as_ref() == &CType::Any || fnty.as_ref() == &cty {
                                    continue;
                                }
                            }

                            return Err(SymbolTableError {
                                error: format!("第{:?}个参数不匹配,期望类型为{:?},实际类型为:{:?}", i + 1, ety, cty),
                                location: self.loc().clone(),
                            });
                        }
                    }
                } else {
                    if !*is_default {
                        if !ety.is_varargs() && i != len - 1 {
                            return Err(SymbolTableError {
                                error: format!("缺少第{:?}个参数，参数类型为{:?}", i + 1, ety),
                                location: self.loc().clone(),
                            });
                        }
                    }
                }

                if let Some(Expression::Variable(ast::Identifier { name, loc })) = args.get(i) {
                    builder.verify_mutability(name.clone(), ref_mut.clone(), loc.clone())?;
                } else if let Some(Expression::FunctionCall(..)) = args.get(i) {
                    //TODO
                } else if let Some(Expression::Attribute(..)) = args.get(i) {}
            }
            // self.scan_expressions(args, &ExpressionContext::Load)?;
        }
        Ok(())
    }
    fn resolve_chained_call(&self, ty: &CType, builder: &mut SymbolTableBuilder) -> Result<CType, SymbolTableError> {
        let v = get_attribute_vec(self);
        let mut cty = ty.clone();
        let mut attri_type = 0;
        let len = v.len();
        for (idx, name) in v.iter().enumerate() {
            if name.0.clone().is_empty() {
                continue;
            }
            if idx < len - 1 {
                if let CType::Package(_) = cty.clone() {
                    let attri_name = v.get(idx + 1).unwrap().clone();
                    cty = get_package_layer(&cty, attri_name.0).unwrap();
                } else if let CType::Struct(_) = cty.clone() {
                    let attri_name = v.get(idx + 1).unwrap().clone();
                    let tmp = cty.attri_name_type(attri_name.0.clone());
                    attri_type = tmp.0;
                    //println!("tmp:{:?}", tmp);
                    if attri_type < 1 {
                        return Err(SymbolTableError {
                            error: format!("{:?}中找不到{:?}的函数", name.0, attri_name.0),
                            location: self.loc(),
                        });
                    } else if attri_type > 1 {
                        builder.verify_fun_visible(&cty, name.0.clone(), attri_name.0.clone())?;
                    } else {
                        builder.verify_field_visible(&cty, name.0.clone(), attri_name.0.clone())?;
                    }
                    cty = tmp.1.clone();
                } else if let CType::Enum(_) = cty.clone() {
                    let attri_name = v.get(idx + 1).unwrap().clone();
                    let tmp = cty.attri_name_type(attri_name.0.clone());
                    attri_type = tmp.0;
                    // println!("tmp:{:?}", tmp);
                    if attri_type < 1 {
                        return Err(SymbolTableError {
                            error: format!("{:?}中找不到{:?}的函数", name.0, attri_name.0),
                            location: self.loc(),
                        });
                    }
                    if attri_type > 2 {
                        builder.verify_fun_visible(&cty, name.0.clone(), attri_name.0.clone())?;
                        cty = tmp.1.clone();
                    } else {
                        builder.verify_field_visible(&cty, name.0.clone(), attri_name.0.clone())?;
                    }
                    return Ok(cty);
                } else if let CType::Fn(fntype) = cty.clone() {
                    name.1.resolve_function_call(&cty, builder)?;
                    //  self.resolve_fn(&name.1, &cty.clone())?;
                    cty = cty.ret_type().clone();
                } else {
                    return Err(SymbolTableError {
                        error: format!("只有是struct、函数、和enum时才有函数类型"),
                        location: self.loc(),
                    });
                }
            }
        }
        Ok(cty)
    }
}
