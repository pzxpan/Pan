use pan_parser::ast::{Expression, FunctionDefinition, Import, Identifier};
use crate::symboltable::{SymbolTableResult, SymbolTableBuilder, SymbolTableError, SymbolUsage};
use crate::ctype::CType;
use pan_parser::ast;
use crate::variable_type::HasType;
use crate::util::{resolve_import_symbol_table, get_package_layer};
use std::collections::HashSet;

pub trait import_item {
    fn resolve_single_import(&self, builder: &mut SymbolTableBuilder) -> SymbolTableResult;
    fn resolve_import_item(&self, cty: &CType, builder: &mut SymbolTableBuilder) -> SymbolTableResult;
    fn resolve_type(&self, ty: &CType, builder: &mut SymbolTableBuilder) -> Result<CType, SymbolTableError>;
}

impl import_item for Import {
    fn resolve_single_import(&self, builder: &mut SymbolTableBuilder) -> SymbolTableResult {
        match self {
            Import::Plain(b, v, is_all) => {
                let ty = builder.get_register_type(self.top_name())?;
                let cty = self.resolve_type(&ty, builder)?;
                self.resolve_import_item(&cty, builder)?;
                Ok(())
                // let last_item = v.last().unwrap();
                // let item_name = last_item.name.clone();
                // println!("top_ty::{:?},last_item:{:?},", ty, item_name);
                // resovle_import(builder, top_name.name.clone(), &v[1..], ty, *is_all, item_name, Option::None)?;
            }
            _ => {
                Ok(())
            }
        }
    }

    fn resolve_import_item(&self, cty: &CType, builder: &mut SymbolTableBuilder) -> SymbolTableResult {
        let top_name = self.top_name();
        let as_name = self.as_name();
        let item_name = self.last_name();
        let prefix = self.prefix();
        println!("dddprefix:{:?}", prefix);
        if self.is_all() {
            if let CType::Package(ty) = cty {
                for (is_pub, name, ty) in ty.enums.iter() {
                    if *is_pub {
                        builder.register_name(name, ty.clone(), SymbolUsage::Import, self.loc())?;
                    }
                }
                for (is_pub, name, ty) in ty.bounds.iter() {
                    if *is_pub {
                        builder.register_name(name, ty.clone(), SymbolUsage::Import, self.loc())?;
                    }
                }
                for (is_pub, name, ty) in ty.structs.iter() {
                    if *is_pub {
                        if top_name.eq("std") {
                            let mut v = vec!["std".to_string()];
                            v.extend_from_slice(&prefix[..]);
                            builder.register_package_item(name, ty.clone(), SymbolUsage::Import, self.loc(), v)?;
                        } else {
                            builder.register_name(name, ty.clone(), SymbolUsage::Import, self.loc())?;
                        }
                    }
                }
                for (is_pub, name, ty) in ty.funs.iter() {
                    if *is_pub {
                        if top_name.eq("std") {
                            let mut v = vec!["std".to_string()];
                            v.extend_from_slice(&prefix[..]);
                            builder.register_package_item(name, ty.clone(), SymbolUsage::Import, self.loc(), v)?;
                        } else {
                            builder.register_name(name, ty.clone(), SymbolUsage::Import, self.loc())?;
                        }
                    }
                }
                for (is_pub, name, ty) in ty.consts.iter() {
                    if *is_pub {
                        builder.register_name(name, ty.clone(), SymbolUsage::Import, self.loc())?;
                    }
                }
                for (is_pub, name, ty) in ty.submods.iter() {
                    if *is_pub {
                        builder.register_name(name, ty.clone(), SymbolUsage::Import, self.loc())?;
                    }
                }
            } else if let CType::Struct(sty) = cty {
                for (name, ty) in &sty.static_methods {
                    builder.register_name(name, ty.clone(), SymbolUsage::Import, self.loc());
                }
            } else if let CType::Enum(ety) = cty {
                for (name, ty) in &ety.static_methods {
                    builder.register_name(name, ty.clone(), SymbolUsage::Import, self.loc());
                }
                for (name, ty, ..) in &ety.items {
                    builder.register_name(name, ty.clone(), SymbolUsage::Import, self.loc());
                }
            } else {
                return Err(SymbolTableError {
                    error: format!("只有package，struct和enum才能全部导出"),
                    location: self.loc(),
                });
            }
        } else {
            if as_name.is_some() {
                builder.register_name(&as_name.unwrap(), cty.clone(), SymbolUsage::Import, self.loc());
            } else {
                builder.register_name(&item_name, cty.clone(), SymbolUsage::Import, self.loc());
            }
        }
        Ok(())
    }

    fn resolve_type(&self, ty: &CType, builder: &mut SymbolTableBuilder) -> Result<CType, SymbolTableError> {
        let mut cty = ty.clone();
        println!("cccccty:{:?},", cty);
        let prefix = self.prefix();
        let len = prefix.len();
        if len < 1 {
            self.resolve_import_item(&cty, builder)?;
        }

        for (idx, name) in prefix.iter().enumerate() {
            if idx == 0 {
                let r = get_package_layer(&cty, name.clone());
                if r.is_some() {
                    cty = r.unwrap().clone();
                } else {
                    return Err(SymbolTableError {
                        error: format!("{:?}包中找不到{:?}的定义", cty.name(), name),
                        location: self.loc(),
                    });
                }
            } else {
                if let CType::Package(..) = cty.clone() {
                    let r = get_package_layer(&cty, name.clone());
                    if r.is_some() {
                        cty = r.unwrap().clone();
                    } else {
                        return Err(SymbolTableError {
                            error: format!("{:?}包中找不到{:?}的定义", cty.name(), name),
                            location: self.loc(),
                        });
                    }
                } else if let CType::Enum(ety) = cty.clone() {
                    let tmp = cty.attri_name_type(name.clone());
                    if tmp.1 == &CType::Unknown {
                        return Err(SymbolTableError {
                            error: format!("enum{:?}中找不到{:?}的定义", cty.name(), name),
                            location: self.loc(),
                        });
                    } else {
                        cty = tmp.1.clone();
                    }
                } else if let CType::Struct(sty) = cty.clone() {
                    let tmp = cty.attri_name_type(name.clone());
                    if tmp.1 == &CType::Unknown {
                        return Err(SymbolTableError {
                            error: format!("struct{:?}中找不到{:?}的定义", cty.name(), name),
                            location: self.loc(),
                        });
                    } else {
                        cty = tmp.1.clone();
                    }
                } else if let CType::Bound(sty) = cty.clone() {
                    let tmp = cty.attri_name_type(name.clone());
                    if tmp.1 == &CType::Unknown {
                        return Err(SymbolTableError {
                            error: format!("struct{:?}中找不到{:?}的定义", cty.name(), name),
                            location: self.loc(),
                        });
                    } else {
                        cty = tmp.1.clone();
                    }
                } else if idx == len - 1 {
                    //最后一个不要处理子类型，上层已经处理；cty就为最后的类型;
                } else {
                    return Err(SymbolTableError {
                        error: format!("大哥，不要来这里啊，来这里的类型已经不适合import了"),
                        location: self.loc(),
                    });
                }
            }
        }
        Ok(cty)
    }
}
