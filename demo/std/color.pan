pub enum Color {
   Red(int),
   Green(float),
   Blue(int,int,int),
   Black(),
   White,

   pub fun is_warm() {
        return self == Black();
   }
}







