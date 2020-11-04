fun main() {
  let sum = 0;
  //有问题，通常应该是左开右闭的区间，等std库好了之后解决这个问题
  for i in 101..0  {
      print(i);
      sum += i;
  }
  print(sum);
  return;
}




