fun main() {
  let dict = {"Alice": 2341, "Beth": 9102, "Cecil": 3258};
  let dict2 = {4: "ddddd",3: "aaa", 2: "eeee"};
  dict["Alice"] = 999;
  dict2[4] = "aaaa";
   //
  for a in dict {
    let (aa,bb) = a;
    print(aa);
    print(bb);
    print(a.1);
    print(a);
  }

  print(dict["Alice"]);
  print(dict2[4]);

  let set = {"Alice","Adasd"};
    for a in set {
      print(a);
    }
  return;
}




