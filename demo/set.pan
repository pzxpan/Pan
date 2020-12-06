package default;
import core.option.*;
fun main() {
   let i = 10_i32;
   let u = 1000_i64;
   let ss = 1000_us;
   let b = Option::Some(10);
   b!! { print(b); }

  if b.is_some() {
    print("ok");
  }
}




