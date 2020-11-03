pub enum Color {
   Red(i32),
   Single(i128),
   White(i32,string),

}

fun main() {
    let color = Color::White(20,"pan");
     match color {
           Color::Red(a) -> { print(a); return true;}
           Color::Single(a) -> { print(a); return true;}
           Color::White(a,b) -> { print(a);print(b); return true;}
       }
    print(a);
}







