//范型T和普通类型没有区别，在语法分析时利用bound进行限定，满足限定的调用都是安全的，不满足的，则范型编译出错，

pub bound Add<T> {
    fun add(rhs: T) : T {
        self + rhs;
    }
}

pub struct Pair<T:Add> {
    pub x: T,
    pub y: T,
    pub fun get_x<T>() : T {
        return x + y;
    }
}

fun main() {
   let a = Pair({x:10000_i128,y:20_i128});
   let b = a.get_x();
   print(b);
   return;
}





