//! A test for the logic that updates the state in a `ResultsCursor` during seek.

use std::marker::PhantomData;

use pan_index::bit_set::BitSet;
use pan_index::vec::IndexVec;
use pan_middle::mir::{self, BasicBlock, Location};
use pan_middle::ty;
use pan_span::DUMMY_SP;

use super::*;

/// Creates a `mir::Body` with a few disconnected basic blocks.
///
/// This is the `Body` that will be used by the `MockAnalysis` below. The shape of its CFG is not
/// important.
fn mock_body() -> mir::Body<'static> {
    let source_info = mir::SourceInfo::outermost(DUMMY_SP);

    let mut blocks = IndexVec::new();
    let mut block = |n, kind| {
        let nop = mir::Statement { source_info, kind: mir::StatementKind::Nop };

        blocks.push(mir::BasicBlockData {
            statements: std::iter::repeat(&nop).cloned().take(n).collect(),
            terminator: Some(mir::Terminator { source_info, kind }),
            is_cleanup: false,
        })
    };

    let dummy_place = mir::Place { local: mir::RETURN_PLACE, projection: ty::List::empty() };

    block(4, mir::TerminatorKind::Return);
    block(1, mir::TerminatorKind::Return);
    block(
        2,
        mir::TerminatorKind::Call {
            func: mir::Operand::Copy(dummy_place.clone()),
            args: vec![],
            destination: Some((dummy_place.clone(), mir::START_BLOCK)),
            cleanup: None,
            from_hir_call: false,
            fn_span: DUMMY_SP,
        },
    );
    block(3, mir::TerminatorKind::Return);
    block(0, mir::TerminatorKind::Return);
    block(
        4,
        mir::TerminatorKind::Call {
            func: mir::Operand::Copy(dummy_place.clone()),
            args: vec![],
            destination: Some((dummy_place.clone(), mir::START_BLOCK)),
            cleanup: None,
            from_hir_call: false,
            fn_span: DUMMY_SP,
        },
    );

    mir::Body::new_cfg_only(blocks)
}

/// A dataflow analysis whose state is unique at every possible `SeekTarget`.
///
/// Uniqueness is achieved by having a *locally* unique effect before and after each statement and
/// terminator (see `effect_at_target`) while ensuring that the entry set for each block is
/// *globally* unique (see `mock_entry_set`).
///
/// For example, a `BasicBlock` with ID `2` and a `Call` terminator has the following state at each
/// location ("+x" indicates that "x" is added to the state).
///
/// | Location               | Before            | After  |
/// |------------------------|-------------------|--------|
/// | (on_entry)             | {102}                     ||
/// | statement 0            | +0                | +1     |
/// | statement 1            | +2                | +3     |
/// | `Call` terminator      | +4                | +5     |
/// | (on unwind)            | {102,0,1,2,3,4,5}         ||
///
/// The `102` in the block's entry set is derived from the basic block index and ensures that the
/// expected state is unique across all basic blocks. Remember, it is generated by
/// `mock_entry_sets`, not from actually running `MockAnalysis` to fixpoint.
struct MockAnalysis<'tcx, D> {
    body: &'tcx mir::Body<'tcx>,
    dir: PhantomData<D>,
}

impl<D: Direction> MockAnalysis<'tcx, D> {
    const BASIC_BLOCK_OFFSET: usize = 100;

    /// The entry set for each `BasicBlock` is the ID of that block offset by a fixed amount to
    /// avoid colliding with the statement/terminator effects.
    fn mock_entry_set(&self, bb: BasicBlock) -> BitSet<usize> {
        let mut ret = self.bottom_value(self.body);
        ret.insert(Self::BASIC_BLOCK_OFFSET + bb.index());
        ret
    }

    fn mock_entry_sets(&self) -> IndexVec<BasicBlock, BitSet<usize>> {
        let empty = self.bottom_value(self.body);
        let mut ret = IndexVec::from_elem(empty, &self.body.basic_blocks());

        for (bb, _) in self.body.basic_blocks().iter_enumerated() {
            ret[bb] = self.mock_entry_set(bb);
        }

        ret
    }

    /// Returns the index that should be added to the dataflow state at the given target.
    fn effect(&self, loc: EffectIndex) -> usize {
        let idx = match loc.effect {
            Effect::Before => loc.statement_index * 2,
            Effect::Primary => loc.statement_index * 2 + 1,
        };

        assert!(idx < Self::BASIC_BLOCK_OFFSET, "Too many statements in basic block");
        idx
    }

    /// Returns the expected state at the given `SeekTarget`.
    ///
    /// This is the union of index of the target basic block, the index assigned to the
    /// target statement or terminator, and the indices of all preceding statements in the target
    /// basic block.
    ///
    /// For example, the expected state when calling
    /// `seek_before_primary_effect(Location { block: 2, statement_index: 2 })`
    /// would be `[102, 0, 1, 2, 3, 4]`.
    fn expected_state_at_target(&self, target: SeekTarget) -> BitSet<usize> {
        let block = target.block();
        let mut ret = self.bottom_value(self.body);
        ret.insert(Self::BASIC_BLOCK_OFFSET + block.index());

        let target = match target {
            SeekTarget::BlockEntry { .. } => return ret,
            SeekTarget::Before(loc) => Effect::Before.at_index(loc.statement_index),
            SeekTarget::After(loc) => Effect::Primary.at_index(loc.statement_index),
        };

        let mut pos = if D::is_forward() {
            Effect::Before.at_index(0)
        } else {
            Effect::Before.at_index(self.body[block].statements.len())
        };

        loop {
            ret.insert(self.effect(pos));

            if pos == target {
                return ret;
            }

            if D::is_forward() {
                pos = pos.next_in_forward_order();
            } else {
                pos = pos.next_in_backward_order();
            }
        }
    }
}

impl<D: Direction> AnalysisDomain<'tcx> for MockAnalysis<'tcx, D> {
    type Domain = BitSet<usize>;
    type Direction = D;

    const NAME: &'static str = "mock";

    fn bottom_value(&self, body: &mir::Body<'tcx>) -> Self::Domain {
        BitSet::new_empty(Self::BASIC_BLOCK_OFFSET + body.basic_blocks().len())
    }

    fn initialize_start_block(&self, _: &mir::Body<'tcx>, _: &mut Self::Domain) {
        unimplemented!("This is never called since `MockAnalysis` is never iterated to fixpoint");
    }
}

impl<D: Direction> Analysis<'tcx> for MockAnalysis<'tcx, D> {
    fn apply_statement_effect(
        &self,
        state: &mut Self::Domain,
        _statement: &mir::Statement<'tcx>,
        location: Location,
    ) {
        let idx = self.effect(Effect::Primary.at_index(location.statement_index));
        assert!(state.insert(idx));
    }

    fn apply_before_statement_effect(
        &self,
        state: &mut Self::Domain,
        _statement: &mir::Statement<'tcx>,
        location: Location,
    ) {
        let idx = self.effect(Effect::Before.at_index(location.statement_index));
        assert!(state.insert(idx));
    }

    fn apply_terminator_effect(
        &self,
        state: &mut Self::Domain,
        _terminator: &mir::Terminator<'tcx>,
        location: Location,
    ) {
        let idx = self.effect(Effect::Primary.at_index(location.statement_index));
        assert!(state.insert(idx));
    }

    fn apply_before_terminator_effect(
        &self,
        state: &mut Self::Domain,
        _terminator: &mir::Terminator<'tcx>,
        location: Location,
    ) {
        let idx = self.effect(Effect::Before.at_index(location.statement_index));
        assert!(state.insert(idx));
    }

    fn apply_call_return_effect(
        &self,
        _state: &mut Self::Domain,
        _block: BasicBlock,
        _func: &mir::Operand<'tcx>,
        _args: &[mir::Operand<'tcx>],
        _return_place: mir::Place<'tcx>,
    ) {
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum SeekTarget {
    BlockEntry(BasicBlock),
    Before(Location),
    After(Location),
}

impl SeekTarget {
    fn block(&self) -> BasicBlock {
        use SeekTarget::*;

        match *self {
            BlockEntry(block) => block,
            Before(loc) | After(loc) => loc.block,
        }
    }

    /// An iterator over all possible `SeekTarget`s in a given block in order, starting with
    /// `BlockEntry`.
    fn iter_in_block(body: &mir::Body<'_>, block: BasicBlock) -> impl Iterator<Item = Self> {
        let statements_and_terminator = (0..=body[block].statements.len())
            .flat_map(|i| (0..2).map(move |j| (i, j)))
            .map(move |(i, kind)| {
                let loc = Location { block, statement_index: i };
                match kind {
                    0 => SeekTarget::Before(loc),
                    1 => SeekTarget::After(loc),
                    _ => unreachable!(),
                }
            });

        std::iter::once(SeekTarget::BlockEntry(block)).chain(statements_and_terminator)
    }
}

fn test_cursor<D: Direction>(analysis: MockAnalysis<'tcx, D>) {
    let body = analysis.body;

    let mut cursor =
        Results { entry_sets: analysis.mock_entry_sets(), analysis }.into_results_cursor(body);

    let every_target = || {
        body.basic_blocks()
            .iter_enumerated()
            .flat_map(|(bb, _)| SeekTarget::iter_in_block(body, bb))
    };

    let mut seek_to_target = |targ| {
        use SeekTarget::*;

        match targ {
            BlockEntry(block) => cursor.seek_to_block_entry(block),
            Before(loc) => cursor.seek_before_primary_effect(loc),
            After(loc) => cursor.seek_after_primary_effect(loc),
        }

        assert_eq!(cursor.get(), &cursor.analysis().expected_state_at_target(targ));
    };

    // Seek *to* every possible `SeekTarget` *from* every possible `SeekTarget`.
    //
    // By resetting the cursor to `from` each time it changes, we end up checking some edges twice.
    // What we really want is an Eulerian cycle for the complete digraph over all possible
    // `SeekTarget`s, but it's not worth spending the time to compute it.
    for from in every_target() {
        seek_to_target(from);

        for to in every_target() {
            dbg!(from);
            dbg!(to);
            seek_to_target(to);
            seek_to_target(from);
        }
    }
}

#[test]
fn backward_cursor() {
    let body = mock_body();
    let body = &body;
    let analysis = MockAnalysis { body, dir: PhantomData::<Backward> };
    test_cursor(analysis)
}

#[test]
fn forward_cursor() {
    let body = mock_body();
    let body = &body;
    let analysis = MockAnalysis { body, dir: PhantomData::<Forward> };
    test_cursor(analysis)
}
