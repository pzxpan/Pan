impl AA for D {
    fun add(a:i32) -> i32 {
        return self.bb.b + a;
    }
}
impl D {
    pub fun dddd() -> i32 {
       return 10;
    }
}

bound AA {
    fun add(a:i32) -> i32;
}
pub struct D {
    pub d:i32,
    pub bb:BB,
}
pub struct BB {
    pub b:i32,
}

fun main() {
    let mut dd = D {d:20,bb:BB{b:10}};
    let a = dd.add(40);
    println!("add:{:?}",a);
    println!("helloeeeeeworld");
}