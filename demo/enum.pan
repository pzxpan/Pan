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
   let bb = color2.is_warm();
   print(bb);

   let num = 1000;
   match num {
        num < 100 -> {print(1);}
        100 -> {print(2);}
        1000 -> {print(1000);}
        _ -> {print(200);}
   }
   return;
}





