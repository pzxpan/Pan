use super::BackendTypes;
use pan_middle::ty::Ty;
use pan_target::abi::call::FnAbi;

pub trait AbiBuilderMethods<'tcx>: BackendTypes {
    fn apply_attrs_callsite(&mut self, fn_abi: &FnAbi<'tcx, Ty<'tcx>>, callsite: Self::Value);
    fn get_param(&self, index: usize) -> Self::Value;
}
