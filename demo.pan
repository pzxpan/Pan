fun fib(x:int) : int {
    if x < 3 {
        return 1;
    }
    //let x1 = x-1;
    //let x2 = x-2;
   return fib(x-1) + fib(x-2);
}

fun fib1(x:int) : int {
    if x == 0 { return 0; }
    return x + fib1(x-1);
}

fun main() {
  // let a = 20;
  // let aaa = other(a+120,20);
   //print(aaa);
  let abb = fib(9);
   print(abb);
   return;
}
fun print(a:int) {
}

//fun other(a: int, b:int) : int {
  // return a - b*100;
//}

//fun main() {
  //  let a: int = 20 + 20 * 3;
    //let b: int = 30 + 2 * 6;
    //let c: int = a -b;
    //return c;
//}





