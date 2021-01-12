impl AA for D {
    fun  add(&self,a:i32) -> i32 {
        return self.bb.b + a;
    }
}
impl D {
    pub fun dddd() -> i32 {
       return 10;
    }
}
{ attrs: [], id: NodeId(45), span: test.pan:12:5: 12:33 (#0),
vis: Visibility { kind: Inherited, span: test.pan:12:5: 12:5 (#0), tokens: None },
ident: add#0, kind: Fn(Final, FnSig { header: FnHeader { unsafety: No, asyncness: No, constness: No, staticness: No, ext: None },
decl: FnDecl { inputs: [Param { attrs: ThinVec(None), ty: Ty { id: NodeId(48), kind: Rptr(None, MutTy { ty: Ty { id: NodeId(49), kind: ImplicitSelf,
 span: test.pan:12:13: 12:18 (#0), tokens: None }, mutbl: Not }), span: test.pan:12:13: 12:18 (#0), tokens: None },
  pat: Pat { id: NodeId(47), kind: Ident(ByValue(Not), self#0, None), span: test.pan:12:13: 12:18 (#0), tokens: None },
  id: NodeId(46), span: test.pan:12:13: 12:18 (#0), is_placeholder: false }, Param { attrs: ThinVec(None),
  ty: Ty { id: NodeId(52), kind: Path(None, Path { span: test.pan:12:21: 12:24 (#0),
  segments: [PathSegment { ident: i32#0, id: NodeId(53), args: None }], tokens: None }), span: test.pan:12:21: 12:24 (#0), tokens: None },
  pat: Pat { id: NodeId(51), kind: Ident(ByValue(Not), a#0, None), span: test.pan:12:19: 12:20 (#0), tokens: None }, id: NodeId(50),
  span: test.pan:12:19: 12:24 (#0), is_placeholder: false }], output: Ty(Ty { id: NodeId(54), kind: Path(None, Path { span: test.pan:12:29: 12:32 (#0),
  segments: [PathSegment { ident: i32#0, id: NodeId(55), args: None }], tokens: None }), span: test.pan:12:29: 12:32 (#0), tokens: None }) },
  span: test.pan:12:5: 12:33 (#0) }, Generics { params: [], where_clause: WhereClause { has_where_token: false, predicates: [],
  span: test.pan:12:32: 12:32 (#0) }, span: test.pan:12:12: 12:12 (#0) }, None), tokens: None }

bound AA {
    fun add(&self,a:i32) -> i32;
}
pub struct D {
    pub d:i32,
    pub bb:BB,
}
pub struct BB {
    pub b:i32,
}

fun main() {
    let dd = D {d:20,bb:BB{b:10}};
    let a = dd.add(30);
    println!("add:{:?}",a);
    println!("helloeeeeeworld");
}