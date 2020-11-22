package default;
pub fun add(b:i32,a:i32 = 10):i32 {
    return a + b;
}
pub fun ad(b:i64) :i64 {
    return b + 40_i64;
}
fun main() {
  let sum = add(40,30);
  print(sum);
  let ss = add(1000,ad(20_i64) as i32);
  print(ss);

  let aa = add(3000);
  print(aa);
  let dd = add({b:400,a:4000});
  print(dd);
  return;
}




