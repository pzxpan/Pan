pub fun add(b:i32,a:i32 = 10):i32 {
    return a + b;
}
fun main() {
  let sum = add(40,30);
  print(sum);
  let ss = add(1000);
  print(ss);

  let dd = add({b:400,a:4000});
  print(dd);
  return;
}




