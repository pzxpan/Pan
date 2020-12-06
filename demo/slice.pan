package default;


fun main() {
   let num = 1000;
   for i in -10 .. 20 {
        print(i);
   }

   match num {
        100 < num -> {print("<100000");}
        30..=100 -> {print("ookkk");}
        200 -> {print(2);}
        1000 -> {print(1000);}
        //没做完全匹配检查，通配符_必须要有,起保护作用
        _ -> {print(200);}
   }

   let c = 'c';
   match c {
    'a'..'d' -> {print("ok"); print("ddd");}
    _ -> print("not_ok");
   }
   let ccc = "pan";
   match ccc {
    "pan" -> {print("ok:{}",ccc);}
    _ -> {print("err");}
   }
   let ceshi = "ceshi";
   let cc = ceshi[2..=4];
   print(cc);
   return;
}