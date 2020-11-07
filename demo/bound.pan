pub bound Add<T:Self> {
    fun add(rhs: T) : T {
        self + rhs;
    }
}

pub struct Point impl Add {
    pub x: i32,
    pub y: i32,
    fun add(p: Point): Point {
        x = p.x + x;
        y = p.y + y;
        return self;
    }
}

fun main() {
    let p1 = Point({x:32,y:45});
    let p2 = Point({x:10000, y: 10000});
    p1.add(p2);
    print(p1);
}


