impl D {
     share fun add(a:i32) -> i32 {
        return self.bb.b + a;
    }
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
}
