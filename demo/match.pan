import std.color.*;
fun main() {
   let a = 100;
   match a {
        100 -> { print(100); }
        1000 ->{ print(1000); }
        2000 -> {print(2000);}
        3000 -> {print(2000);}
   }

   let color = Color::Red(10);
   let c = color.is_warm();
   print(c);
   return;
}