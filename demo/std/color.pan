pub enum Color {
   Red(i32),
   Single(i128),
   Two(i32,string),
   Three(i32,i32,i32),
   Green(i32),
   Blue(i32,i32,i32),
   Black,
   White,
   pub fun is_warm() :bool{
        match self {
            Red(a) -> { print(a); return true;}
            Single(a) -> { print(a); return true;}
            Threed(a,b,c) -> {print(a); print(b);print(c);return false;}
        }
   }
}







