package default;

//以下import都Ok
//import std.person.Person;
//import std.person.House;
//import std.person.*;

import std.person.*;
import std.color.*;
import std.test.person.name_call;
fun main() {
    let c = Color::Red(10);
    let d = House!{size:1111.0,price:10.0};
    let d = 20;
    print(d);
    let ddd = d.idea();
    let cc = name_call!{age:40,name:"ddd"};
    print(cc);
    print(ddd);
}


