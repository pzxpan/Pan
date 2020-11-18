pub enum Color {
   Red(i32),
   Single(i128),
   White(i32,string),
   Three(i32,i32,i32),
   Black,
   pub fun is_warm() {
           match self {
               White(c,b)-> {print(20);}
               Black -> {print(30);}
               Red(a) -> { print(a); return true;}
               Single(a) -> { print(a); return 1;}
               Three(a,b,c) -> {print(a); print(b);print(c);return 2;}
           }
      }
}









