//范型T和普通类型没有区别，在语法分析时利用bound进行限定，满足限定的调用都是安全的，不满足的，则范型编译出错，
package default;
pub bound Add<T> {
    fun add<T>(rhs: T) : T {
        return rhs;
    }
}

pub struct Pair<T:Add> {
    pub x: T,
    pub y: T,
    pub fun x_add_number<T>(n: T) : T {
        return x + n;
    }
}

fun main() {
   let a = Pair!{x:10000,y:20};
   let b = a.x_add_number(10);
   print(b);
   return;
}





