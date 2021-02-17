package default;
//import core.option.*;
pub fun fib(a:i32) : i32 {
    if a == 0 : 0 elif a == 1 : 1 else :  fib(a-1) + fib(a-2) |;
}
pub fun add(a:i32) :i32 {
     a + 10 |;
}

fun main() {
   let i = 10_i32;
   let u = 1000_i64;
   let ss = 1000_us;
  // let b = Option::Some(10);

  // if b.is_some() {
   // print("ok");
 // }
  let c = add(20);

  let cc = fib(9);
  print(cc);
  print(c);
}




