package default;
pub fun add(a:i32,b:i32,varargs..) : i32 {
    let sum = a + b;
    for i in varargs {
        print(i);
    }
    return sum;
}
fun main() {
  let a = add(20,30,40,"pan");
  print(a);
  return;
}




