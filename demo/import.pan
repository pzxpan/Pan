import std.person {Person as PPP, House};
//以下import都Ok
//import std.person.Person;
//import std.person.House;
//import std.person.*;
import std.color.*;

fun main() {
  let person = PPP({age:50,name:"pan"});
  let bb = person.is_older();
  print(bb);
  let house = House({size:1000.0,price:1000000.0});
  let dd = house.idea();
  print(dd);
  let c = Color::Red(10);
  c.is_warm();
}

