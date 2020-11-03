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

fun main() {
     let color = Color::White(20,"pan");
     match color {
           Color::Red(a) -> { print(a); }
           Color::Single(a) -> { print(a); }
           Color::White(a,b) -> { print(a);print(b);}
       }
    let aa = Color::Three(12,13,14);
    let d = aa.is_warm();
    print(d);
}







