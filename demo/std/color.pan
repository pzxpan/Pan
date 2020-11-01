pub enum Color {
   Red(i32),
   Green(i32),
   Blue(i32,i32,i32),
   Black(),
   White,
   pub fun is_warm() {
        return self == Black();
   }
}







