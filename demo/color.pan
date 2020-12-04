package default;
pub enum Color {
   Red(i32),
   Single(i128),
   White(i32,string),
   Three(i32,i32,i32),
   Black,
   pub fun is_warm() {
           match self {
               White(c,bb)-> {print(bb);}
               Black -> {print(30);}
               Red(a) -> { print(a); }
               Single(a) -> { print(a); }
               Three(a,b,c) -> {print(a); print(b);print(c);}
           }
      }
}









