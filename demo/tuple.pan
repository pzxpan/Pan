fun main() {
  let aa = (20,30);
  let bb = aa.0;
  aa.0 = 100;
  let ccc = (aa,bb);
  let bb = aa.0;
  print(ccc.1);
 // for dd in aa {
 //   print(dd);
 // }
  return;
}
fun print(a:int) {
}




