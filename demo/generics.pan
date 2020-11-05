pub struct Pair<T> {
    pub x: T,
    pub y: T,
    pub fun get_x() : T {
        return x;
    }
}

fun main() {
   let a = Pair({x:10000,y:20});
   let b = a.get_x();
   print(b);
   return;
}





