package default;

//以下import都Ok
//import std.person.Person;
//import std.person.House;
//import std.person.*;
import color;

fun main() {
    let p = Person!{age:100,name:"pdd"};
    let c = p.older_than(40);
    print(c);
}


