use pan_hir as hir;
use pan_hir::def_id::{CrateNum, DefId, LOCAL_CRATE};
use pan_hir::itemlikevisit::ItemLikeVisitor;
use pan_middle::ty::query::Providers;
use pan_middle::ty::TyCtxt;
use pan_span::symbol::sym;

pub fn find(tcx: TyCtxt<'_>) -> Option<DefId> {
    tcx.proc_macro_decls_static(LOCAL_CRATE)
}

fn proc_macro_decls_static(tcx: TyCtxt<'_>, cnum: CrateNum) -> Option<DefId> {
    assert_eq!(cnum, LOCAL_CRATE);

    let mut finder = Finder { tcx, decls: None };
    tcx.hir().krate().visit_all_item_likes(&mut finder);

    finder.decls.map(|id| tcx.hir().local_def_id(id).to_def_id())
}

struct Finder<'tcx> {
    tcx: TyCtxt<'tcx>,
    decls: Option<hir::HirId>,
}

impl<'v> ItemLikeVisitor<'v> for Finder<'_> {
    fn visit_item(&mut self, item: &hir::Item<'_>) {
        if self.tcx.sess.contains_name(&item.attrs, sym::pan_proc_macro_decls) {
            self.decls = Some(item.hir_id);
        }
    }

    fn visit_trait_item(&mut self, _trait_item: &hir::TraitItem<'_>) {}

    fn visit_impl_item(&mut self, _impl_item: &hir::ImplItem<'_>) {}

    fn visit_foreign_item(&mut self, _foreign_item: &hir::ForeignItem<'_>) {}
}

pub(crate) fn provide(providers: &mut Providers) {
    *providers = Providers { proc_macro_decls_static, ..*providers };
}
