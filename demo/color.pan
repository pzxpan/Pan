pub enum Color {
   Red(i32),
   Single(i128),
   White(i32,string),
   Three(i32,i32,i32),
   pub fun is_warm() {
           match self {
               Red(a) -> { print(a); return true;}
               Single(a) -> { print(a); return 1;}
               Three(a,b,c) -> {print(a); print(b);print(c);return 2;}
           }
      }
}








