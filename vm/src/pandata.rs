use crate::types::Type;
use crate::panobject::PanObject;

pub struct PanData<T> {
    value: PanObject<T>,
    ty: Type,
}