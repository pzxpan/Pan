/// This declares a list of types which can be allocated by `Arena`.
///
/// The `few` modifier will cause allocation to use the shared arena and recording the destructor.
/// This is faster and more memory efficient if there's only a few allocations of the type.
/// Leaving `few` out will cause the type to get its own dedicated `TypedArena` which is
/// faster and more memory efficient if there is lots of allocations.
///
/// Specifying the `decode` modifier will add decode impls for `&T` and `&[T]` where `T` is the type
/// listed. These impls will appear in the implement_ty_decoder! macro.
#[macro_export]
macro_rules! arena_types {
    ($macro:path, $args:tt, $tcx:lifetime) => (
        $macro!($args, [
            [] layouts: pan_target::abi::Layout,
            // AdtDef are interned and compared by address
            [] adt_def: pan_middle::ty::AdtDef,
            [] steal_mir: pan_data_structures::steal::Steal<pan_middle::mir::Body<$tcx>>,
            [decode] mir: pan_middle::mir::Body<$tcx>,
            [] steal_promoted:
                pan_data_structures::steal::Steal<
                    pan_index::vec::IndexVec<
                        pan_middle::mir::Promoted,
                        pan_middle::mir::Body<$tcx>
                    >
                >,
            [decode] promoted:
                pan_index::vec::IndexVec<
                    pan_middle::mir::Promoted,
                    pan_middle::mir::Body<$tcx>
                >,
            [decode] typeck_results: pan_middle::ty::TypeckResults<$tcx>,
            [decode] borrowck_result:
                pan_middle::mir::BorrowCheckResult<$tcx>,
            [decode] unsafety_check_result: pan_middle::mir::UnsafetyCheckResult,
            [decode] code_region: pan_middle::mir::coverage::CodeRegion,
            [] const_allocs: pan_middle::mir::interpret::Allocation,
            // Required for the incremental on-disk cache
            [few] mir_keys: pan_hir::def_id::DefIdSet,
            [] region_scope_tree: pan_middle::middle::region::ScopeTree,
            [] dropck_outlives:
                pan_middle::infer::canonical::Canonical<'tcx,
                    pan_middle::infer::canonical::QueryResponse<'tcx,
                        pan_middle::traits::query::DropckOutlivesResult<'tcx>
                    >
                >,
            [] normalize_projection_ty:
                pan_middle::infer::canonical::Canonical<'tcx,
                    pan_middle::infer::canonical::QueryResponse<'tcx,
                        pan_middle::traits::query::NormalizationResult<'tcx>
                    >
                >,
            [] implied_outlives_bounds:
                pan_middle::infer::canonical::Canonical<'tcx,
                    pan_middle::infer::canonical::QueryResponse<'tcx,
                        Vec<pan_middle::traits::query::OutlivesBound<'tcx>>
                    >
                >,
            [] type_op_subtype:
                pan_middle::infer::canonical::Canonical<'tcx,
                    pan_middle::infer::canonical::QueryResponse<'tcx, ()>
                >,
            [] type_op_normalize_poly_fn_sig:
                pan_middle::infer::canonical::Canonical<'tcx,
                    pan_middle::infer::canonical::QueryResponse<'tcx, pan_middle::ty::PolyFnSig<'tcx>>
                >,
            [] type_op_normalize_fn_sig:
                pan_middle::infer::canonical::Canonical<'tcx,
                    pan_middle::infer::canonical::QueryResponse<'tcx, pan_middle::ty::FnSig<'tcx>>
                >,
            [] type_op_normalize_predicate:
                pan_middle::infer::canonical::Canonical<'tcx,
                    pan_middle::infer::canonical::QueryResponse<'tcx, pan_middle::ty::Predicate<'tcx>>
                >,
            [] type_op_normalize_ty:
                pan_middle::infer::canonical::Canonical<'tcx,
                    pan_middle::infer::canonical::QueryResponse<'tcx, pan_middle::ty::Ty<'tcx>>
                >,
            [few] all_traits: Vec<pan_hir::def_id::DefId>,
            [few] privacy_access_levels: pan_middle::middle::privacy::AccessLevels,
            [few] foreign_module: pan_middle::middle::cstore::ForeignModule,
            [few] foreign_modules: Vec<pan_middle::middle::cstore::ForeignModule>,
            [] upvars_mentioned: pan_data_structures::fx::FxIndexMap<pan_hir::HirId, pan_hir::Upvar>,
            [] object_safety_violations: pan_middle::traits::ObjectSafetyViolation,
            [] codegen_unit: pan_middle::mir::mono::CodegenUnit<$tcx>,
            [] attribute: pan_ast::Attribute,
            [] name_set: pan_data_structures::fx::FxHashSet<pan_span::symbol::Symbol>,
            [] hir_id_set: pan_hir::HirIdSet,

            // Interned types
            [] tys: pan_middle::ty::TyS<$tcx>,
            [] predicates: pan_middle::ty::PredicateInner<$tcx>,

            // HIR query types
            [few] indexed_hir: pan_middle::hir::map::IndexedHir<$tcx>,
            [few] hir_definitions: pan_hir::definitions::Definitions,
            [] hir_owner: pan_middle::hir::Owner<$tcx>,
            [] hir_owner_nodes: pan_middle::hir::OwnerNodes<$tcx>,

            // Note that this deliberately duplicates items in the `rustc_hir::arena`,
            // since we need to allocate this type on both the `rustc_hir` arena
            // (during lowering) and the `librustc_middle` arena (for decoding MIR)
            [decode] asm_template: pan_ast::InlineAsmTemplatePiece,

            // This is used to decode the &'tcx [Span] for InlineAsm's line_spans.
            [decode] span: pan_span::Span,
            [decode] used_trait_imports: pan_data_structures::fx::FxHashSet<pan_hir::def_id::LocalDefId>,
        ], $tcx);
    )
}

arena_types!(pan_arena::declare_arena, [], 'tcx);
