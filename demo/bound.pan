pub bound Add<T:Self> {
    pub fun add(rhs: T) : T {
        return rhs;
    }
}

pub struct XPoint {
    pub x: i32,
    pub y: i32,
}

pub struct Point impl Add {
    pub x: i32,
    pub y: i32,
}

fun main() {
    let p1 = Point({x:32,y:45});
    let p2 = Point({x:10000, y: 10000});
    let p3 = p1.add(p2);
    print(p3.x);
    print(p3.y);
}


