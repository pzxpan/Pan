/// This declares a list of types which can be allocated by `Arena`.
///
/// The `few` modifier will cause allocation to use the shared arena and recording the destructor.
/// This is faster and more memory efficient if there's only a few allocations of the type.
/// Leaving `few` out will cause the type to get its own dedicated `TypedArena` which is
/// faster and more memory efficient if there is lots of allocations.
///
/// Specifying the `decode` modifier will add decode impls for `&T` and `&[T]`,
/// where `T` is the type listed. These impls will appear in the implement_ty_decoder! macro.
#[macro_export]
macro_rules! arena_types {
    ($macro:path, $args:tt, $tcx:lifetime) => (
        $macro!($args, [
            // HIR types
            [few] hir_krate: pan_hir::Crate<$tcx>,
            [] arm: pan_hir::Arm<$tcx>,
            [] asm_operand: (pan_hir::InlineAsmOperand<$tcx>, Span),
            [] asm_template: pan_ast::InlineAsmTemplatePiece,
            [] attribute: pan_ast::Attribute,
            [] block: pan_hir::Block<$tcx>,
            [] bare_fn_ty: pan_hir::BareFnTy<$tcx>,
            [few] global_asm: pan_hir::GlobalAsm,
            [] generic_arg: pan_hir::GenericArg<$tcx>,
            [] generic_args: pan_hir::GenericArgs<$tcx>,
            [] generic_bound: pan_hir::GenericBound<$tcx>,
            [] generic_param: pan_hir::GenericParam<$tcx>,
            [] expr: pan_hir::Expr<$tcx>,
            [] field: pan_hir::Field<$tcx>,
            [] field_pat: pan_hir::FieldPat<$tcx>,
            [] fn_decl: pan_hir::FnDecl<$tcx>,
            [] foreign_item: pan_hir::ForeignItem<$tcx>,
            [few] foreign_item_ref: pan_hir::ForeignItemRef<$tcx>,
            [] impl_item_ref: pan_hir::ImplItemRef<$tcx>,
            [few] inline_asm: pan_hir::InlineAsm<$tcx>,
            [few] llvm_inline_asm: pan_hir::LlvmInlineAsm<$tcx>,
            [] local: pan_hir::Local<$tcx>,
            [few] macro_def: pan_hir::MacroDef<$tcx>,
            [] param: pan_hir::Param<$tcx>,
            [] pat: pan_hir::Pat<$tcx>,
            [] path: pan_hir::Path<$tcx>,
            [] path_segment: pan_hir::PathSegment<$tcx>,
            [] poly_trait_ref: pan_hir::PolyTraitRef<$tcx>,
            [] qpath: pan_hir::QPath<$tcx>,
            [] stmt: pan_hir::Stmt<$tcx>,
            [] struct_field: pan_hir::StructField<$tcx>,
            [] trait_item_ref: pan_hir::TraitItemRef,
            [] ty: pan_hir::Ty<$tcx>,
            [] type_binding: pan_hir::TypeBinding<$tcx>,
            [] variant: pan_hir::Variant<$tcx>,
            [] where_predicate: pan_hir::WherePredicate<$tcx>,
        ], $tcx);
    )
}
