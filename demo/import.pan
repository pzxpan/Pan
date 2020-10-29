import std.person.*;
import std.color.*;
fun main() {
  let person = Person({age:50,name:"pan"});
  let bb = person.is_older();
  print(bb);
  let color2 = Color::Red(10);
  print(color2);
}
fun print(a:int) {
}




