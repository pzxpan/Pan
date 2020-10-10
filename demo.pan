
fun fib(x:int) : int {
    if x == 0 {
        return 0;
    }
    if x < 3 {
        return 1;
    }
    //let x1 :int = x-1;
    //let x2 :int = x-2;
    return fib(x-1) + fib(x-2);
}
fun main() {
 //  let aaa  = other(30,20);
 //  let fff = 10;
   let abb = fib(7);
   print(abb);
   return;
}
fun print(a:int) {
}

//fun other(a: int, b:int) : int {
//   return a - b*100;
//}

//fun main() {
  //  let a: int = 20 + 20 * 3;
    //let b: int = 30 + 2 * 6;
    //let c: int = a -b;
    //return c;
//}


