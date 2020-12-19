package default;

//以下import都Ok
//import std.person.Person;
//import std.person.House;
//import std.person.*;

import std.person.*;
import std.color.*;
import std.test.person.*;
fun main() {
    let c = Color::Red(10);
    let d = House!{size:1111.0,price:10.0};
    print(d);
    let ddd = d.idea();
    let name = "ddddd";
    let age = 40000;
    let cc = name_call!{name,age};
    print(cc);
    print(ddd);
}


