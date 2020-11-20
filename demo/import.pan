//import std.person {Person as PPP, House};
//以下import都Ok
//import std.person.Person;
//import std.person.House;
//import std.person.*;
import std.color.*;

fun main() {
  let d = Color::Black;
  d.is_warm();
  let aa = 1000;
  print(aa);
  let c = Color::Red(10);
  c.is_warm();
}

