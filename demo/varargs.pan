package default;
pub fun add(a:i32,b:i32,var...) : i32 {
    let sum = a + b;
    for i in 0..100 {
        for j in 100..20 {
            sum += i + j;
        }
    }
    for a in var {
       print (a);
    }
    return sum;
}
fun main() {
  let a = add(20,30);
  print(a);
  return;
}




