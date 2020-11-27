package default;
fun main() {
  let dict = {"Alice": 2341, "Beth": 9102, "Cecil": 3258};

   //
  for a in dict {
    let (aa,bb) = a;
    print(aa);
    print(bb);
    print(a.1);
    print(a);
  }
  dict["Alice"] = 7890;
  print(dict["Alice"]);

  let a = [1,3,4];
  a[1] = 10;
  print(a[1]);
}




