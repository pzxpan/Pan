bound AA {
     share fun add(a:i32) -> i32;
}
impl AA for D {
     share fun add(a:i32) -> i32 {
        return bb.b + a;
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

pub fun dosomething(d:mut D,s: mut i32) -> BB {
    return d.bb.clone();
}

pub struct D {
    pub d:i32,
    pub bb:BB,
}

#[derive(Clone)]
pub struct BB {
    pub b:i32,
}

fun main() {
    let mut dd = D {d:20,bb:BB{b:10}};
    let mut c = 20;
    let b = dosomething(&mut dd,&mut c);
    println!("ddd:{:?},c:{:?}",b.b,c);
}