package default;


fun main() {
   let num = 1000;
   if num > 30 && num < 100 {
    print("ok");
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
    'a'..'d' -> {print("ok");}
    _ -> {print("not_ok");}
   }
   let ceshi = "ceshi";
   let cc = ceshi[2..=4];
   print(cc);
   return;
}