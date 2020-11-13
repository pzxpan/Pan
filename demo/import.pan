import std.person.*;

fun main() {
  let person = Person({age:50,name:"pan"});
  let house = House({size:100.0,price:10000.0});
  let cc = house.idea();
  print(cc);
  let bb = person.is_older();
  print(bb);
}




