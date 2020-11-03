pub enum Color {
   Red(i32),
   Green(float),
   Blue(i32,i32,i32),
   Black,
   White,
   pub fun is_warm() {
        return self == Black;
   }
}

fun main() {
   let color2 = Color::Red(10);
   print(color2);
   //color.is_warm();
   return;
}





