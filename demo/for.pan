package default;
fun main() {
  let sum = 0;
  for i in 0..=100  {
      for j in 0..=10 {
            sum += i + j;
      }
  }

  print(sum);

  return;
}




