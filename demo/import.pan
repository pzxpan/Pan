package default;

//以下import都Ok
//import std.person.Person;
//import std.person.House;
//import std.person.*;

import std.person.*;
import std.color.*;
fun main() {
    let c = Color::Red(10);
    let p = House!{size:1111.0,price:10.0};
    print(c);
}


