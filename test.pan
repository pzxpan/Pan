bound AA {
     share fun add(a:i32) -> i32;
}
impl AA for D {
     share fun add(a:i32) -> i32 {
        return self.bb.b + a;
    }
}
impl D {
    pub fun dddd() -> i32 {
       return 10;
    }
    pub fun ::DCC(a:i32)-> i32 {
        return 6660;
    }
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
    let a = D::DCC(20);
    println!("add:{:?}",a);
    println!("helloeeeeeworld");
}