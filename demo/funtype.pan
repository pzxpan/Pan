package default;
pub fun sub(a:i32,b:i32) : i32 {
    return a - b;
}

fun main() {
    let c: fun(i32,i32)->i32 = sub;
    let d = c(30,40);
    print(d);
}









