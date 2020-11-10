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
   let color2 = Color::Blue(12,13,14);
   match color2 {
     Color::Black -> { print(40); }
     Color::Red(a) -> {print(50);}
     Color::Blue(a,b,cc) -> {print(a);print(b);}
      _ -> { print(3000); }

   }
   let bb = color2.is_warm();
   print(bb);
   return;
}





