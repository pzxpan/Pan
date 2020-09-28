use std::any::Any;
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::fmt;
use std::marker::PhantomData;
use std::ops::Deref;
use std::rc::Rc;
use std::error::Error;

use indexmap::IndexMap;
use num_bigint::BigInt;
use num_complex::Complex64;
use num_traits::{One, ToPrimitive, Zero};

use pan_bytecode::bytecode;
use crate::scope::Scope;
use crate::vm::VirtualMachine;
use crate::types::Type;

pub type PanObjectRef = Rc<PanObject<dyn Payload>>;

pub struct PanObject<T> {
    name: String,
    value: T,
}

impl<T> PanObject<T> {
    pub fn new(value: T, name: String) -> PanObject<T> {
        PanObject {
            name,
            value,
        }
    }
}

pub trait Payload {

}