fun main() {
  let sum = 0;
  //有问题，通常应该是左开右闭的区间，等std库好了之后解决这个问题
  for i in 101..0  {
      if i % 10 == 0 {
        continue;
      }
      print(i);
      sum += i;
  }

  print(sum);
  sum = 1009;

  let aa = if sum > 2000 : 20 elif sum > 1000 : 200 else :300;
  print(aa);
  return;
}




