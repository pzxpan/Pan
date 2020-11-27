package default;
pub fun sub(a:i32,b:i32) : i32 {
    return a - b;
}

fun main() {
    let aa = 30;
    let bb = 40;
    let c:FunType = sub;
    let d = c(aa,bb);
    print(d);
}









