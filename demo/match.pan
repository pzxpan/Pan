package default;
pub enum Color {
   Red(i32),
   Green(float),
   Blue(i32,i32,i32),
   Black,
   White,
   pub fun is_warm(): bool {
        return self == Color::Black;
   }
}

fun main() {
   let color2 = Color::Black;
   let color1 = Color::Red(10);

   let bb = color1.is_warm();
   print(bb);
   let c = 20;
   let num = 1000;
   match num {
        num < 100 -> {print(1);  bb = false;}
      //  30..100 -> {print(2); bb= true;}
        200 -> {print(2);}
        1000 -> {print(1000);c = 40;}
        //没做完全匹配检查，通配符_必须要有,起保护作用
        _ -> {print(200);}
   }

   let cccc = 'p';
   match cccc {
       // 'a'..'o' -> {print("ohno");}
        'p' -> {print("ok");}
        _ -> {print("no");}
   }

   let dddd = "panpan";
   match dddd {
        "panpan" -> { print("youxiu");}
        "yueyue" -> { print("B"); }
        _ -> {print("C");}
   }
   print(c);
   let sum = 0;
   for a in 0..100 {
      sum += a;
   }
   print(sum);
   let arr = [10,20,30,40];
   let ab = arr[0..=2];
   let ac = arr[1..];
   let ad = arr[..];
   let ae = arr[-2..1000];
   print(ab);
   return;
}