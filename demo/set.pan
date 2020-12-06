package default;
import core.option.*;
fun main() {
  let b = Option::Some(10);
  b!! { print(b); }

  if b.is_some() {
    print("ok");
  }
}




