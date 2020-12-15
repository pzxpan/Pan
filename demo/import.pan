package default;
import std.person.*;
//以下import都Ok
//import std.person.Person;
//import std.person.House;
//import std.person.*;
import std.color.*;

pub fun add_pub(a:i32) : i32 {
    return a - 100;
}
fun main() {
  let person = PPP!{age:50,name:"pan"};
  let bb = person.is_older();
  print(bb);
  let house = House!{size:1000.0,price:1000000.0};
  let dd = house.idea();
  print(dd);
  let c = color.Color::Red(10);
  c.is_warm();
  let ss = add_pub(10);
  print(ss);
}


