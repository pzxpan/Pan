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
   
   let num = 1000;
   match num {
        num < 100 -> {print(1);}
        200 -> {print(2);}
        1000 -> {print(1000);}
        //没做完全匹配检查，通配符_必须要有,起保护作用
        _ -> {print(200);}
   }
   return;
}