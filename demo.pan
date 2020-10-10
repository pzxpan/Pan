fun fib(x:int) : int {
    if x < 3 {
        return 1;
    }
    return fib(x-1) + fib(x-2);
}
fun main() {
 //  let aaa  = other(30,20);
   let fff = 10;
   let abb = fib(5);
   print(abb);
   return;
}
fun print(a:int) {
   return;
}

fun other(a: int, b:int) : int {
   return a - b*100;
}

//fun main() {
  //  let a: int = 20 + 20 * 3;
    //let b: int = 30 + 2 * 6;
    //let c: int = a -b;
    //return c;
//}


