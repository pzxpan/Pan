pub struct Pair<T> {
    pub x: T,
    pub y: T,
    pub fun get_x(cc: T) : T {
        return x + cc;
    }
}

fun main() {
   let a = Pair({x:10000,y:20});
   let b = a.get_x(30);
   print(b);
   return;
}





