/// Aligns with [llvm::coverage::Counter::CounterKind](https://github.com/rust-lang/llvm-project/blob/rustc/11.0-2020-10-12/llvm/include/llvm/ProfileData/Coverage/CoverageMapping.h#L206-L222)
#[derive(Copy, Clone, Debug)]
#[repr(C)]
pub enum CounterKind {
    Zero = 0,
    CounterValueReference = 1,
    Expression = 2,
}

#[derive(Copy, Clone, Debug)]
#[repr(C)]
pub struct Counter {
    // Important: The layout (order and types of fields) must match its C++ counterpart.
    pub kind: CounterKind,
    pub id: u32,
}

impl Counter {
    pub fn zero() -> Self {
        Self { kind: CounterKind::Zero, id: 0 }
    }

    // pub fn counter_value_reference(counter_id: CounterValueReference) -> Self {
    //     Self { kind: CounterKind::CounterValueReference, id: counter_id.into() }
    // }
    //
    // pub fn expression(mapped_expression_index: MappedExpressionIndex) -> Self {
    //     Self { kind: CounterKind::Expression, id: mapped_expression_index.into() }
    // }
}