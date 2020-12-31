use pan_parser::ast::Expression;
use inkwell::values::BasicValueEnum;
use crate::code_gen::CodeGen;
use crate::util::get_attribute_vec;
use pan_compiler::ctype::CType;

pub fn resolve_compile_field<'a, 'ctx>(code_gen: &mut CodeGen<'a, 'ctx>, expr: &Expression) -> Result<BasicValueEnum<'ctx>, &'static str> {
    let mut cty = code_gen.lookup_name(&expr.expr_name()).ty.clone();
    let mut v = get_attribute_vec(expr);
    let len = v.len();
    for (idx, name) in v.iter().enumerate() {
        if idx < len - 1 {
            //暂未处理
            // if let CType::Package(_) = cty.clone() {
            //     return self.resolve_include_package_attribute(&cty, &v);
            // } else
            if let CType::Struct(_) = cty.clone() {
                if name.0.eq("self") {
                    let self_ptr = code_gen.variables.get("self").unwrap();
                    let ptr = code_gen.builder.build_load(self_ptr.clone().0, "self");
                    // println!("vvvv222:{:#?},", self.variables);
                    let attri_name = v.get(idx + 1).unwrap().clone();
                    let tmp = cty.attri_name_type(attri_name.0.clone());
                    let v_idx = code_gen.context.i32_type().const_int(0 as u64, false);
                    let num_idx = code_gen.context.i32_type().const_int(tmp.2 as u64, false);
                    // let ptr = BasicValueEnum::PointerValue();
                    let r = unsafe { code_gen.builder.build_gep(ptr.into_pointer_value(), &[v_idx, num_idx], "index") };
                    cty = tmp.1.clone();
                    return Ok(code_gen.builder.build_load(r, &attri_name.0.clone()));
                } else {
                    let attri_name = v.get(idx + 1).unwrap().clone();
                    let tmp = cty.attri_name_type(attri_name.0.clone());
                    let ptr = code_gen.variables.get(&name.0).unwrap().clone().0;
                    let num_idx = code_gen.context.i32_type().const_int(tmp.2 as u64, false);
                    let v_idx = code_gen.context.i32_type().const_int(0 as u64, false);
                    let r = unsafe { code_gen.builder.build_in_bounds_gep(ptr, &[v_idx, num_idx], "index") };
                    cty = tmp.1.clone();
                    return Ok(code_gen.builder.build_load(r, "tmp"));
                }
            } else if let CType::Tuple(n) = cty.clone() {
                let attri_name = v.get(idx + 1).unwrap().clone();
                let index = attri_name.0.parse::<i32>().unwrap();
                let tmp = cty.attri_index(index);
                cty = tmp.clone();
            } else if let CType::Enum(_) = cty.clone() {}
        }
    }
    return Err("sss");
}

pub fn resolve_codegen_store<'a, 'ctx>(code_gen: &mut CodeGen<'a, 'ctx>, expr: &Expression, value: BasicValueEnum<'ctx>) -> Result<(), &'static str> {
    if let Expression::Variable(a) = expr {
        let alloc = code_gen.variables.get(&a.name).unwrap().0;
        code_gen.builder.build_store(alloc, value);
    } else if let Expression::Attribute(_, name, attri, idx) = expr {
        if idx.is_none() {
            let mut cty = code_gen.lookup_name(&name.expr_name()).ty.clone();
            let mut v = get_attribute_vec(expr);
            let len = v.len();
            for (idx, name) in v.iter().enumerate() {
                if idx < len - 1 {
                    //暂未处理
                    // if let CType::Package(_) = cty.clone() {
                    //     return self.resolve_include_package_attribute(&cty, &v);
                    // } else
                    if let CType::Struct(_) = cty.clone() {
                        if name.0.eq("self") {
                            let self_ptr = code_gen.variables.get("self").unwrap();
                            let ptr = code_gen.builder.build_load(self_ptr.clone().0, "self");
                            // println!("vvvv222:{:#?},", self.variables);
                            let attri_name = v.get(idx + 1).unwrap().clone();
                            let tmp = cty.attri_name_type(attri_name.0.clone());
                            let v_idx = code_gen.context.i32_type().const_int(0 as u64, false);
                            let num_idx = code_gen.context.i32_type().const_int(tmp.2 as u64, false);
                            // let ptr = BasicValueEnum::PointerValue();
                            let r = unsafe { code_gen.builder.build_gep(ptr.into_pointer_value(), &[v_idx, num_idx], "index") };
                            cty = tmp.1.clone();
                            code_gen.builder.build_store(r, value);

                        } else {
                            let attri_name = v.get(idx + 1).unwrap().clone();
                            let tmp = cty.attri_name_type(attri_name.0.clone());
                            let ptr = code_gen.variables.get(&name.0).unwrap().clone().0;
                            let num_idx = code_gen.context.i32_type().const_int(tmp.2 as u64, false);
                            let v_idx = code_gen.context.i32_type().const_int(0 as u64, false);
                            let r = unsafe { code_gen.builder.build_in_bounds_gep(ptr, &[v_idx, num_idx], "index") };
                            cty = tmp.1.clone();
                            code_gen.builder.build_store(r, value);
                        }
                    } else if let CType::Tuple(n) = cty.clone() {
                        let attri_name = v.get(idx + 1).unwrap().clone();
                        let index = attri_name.0.parse::<i32>().unwrap();
                        let tmp = cty.attri_index(index);
                        cty = tmp.clone();
                    } else if let CType::Enum(_) = cty.clone() {}
                }
            }
        }
    }
    return Ok(());
}

