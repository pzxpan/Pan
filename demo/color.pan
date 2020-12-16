package color;
pub enum Color {
   Red(i32),
   Single(i128),
   White(i32,string),
   Black,
   Three(i32,i32,i32),
   pub fun is_warm() {
           match self {
               Black -> {print(30);}
               White(c,bb)-> {print(bb);}
               Red(a) -> { print(a); }

               Color::Single(a) -> { print(a); }
               Color::Three(a,b,c) -> {print(a); print(b);print(c);}
           }
      }
}
pub struct Person {
    pub name: string,
    pub age: i32,
}










