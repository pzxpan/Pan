fun main() {
  let sum = 0;
  for i in 100..0  {
      print(i);
      sum += i;
  }

  print(sum);
  sum = 1009;

  let aa = if sum > 2000 : 20 elif sum > 1000 : 200 else :300;
  print(aa);
  return;
}




