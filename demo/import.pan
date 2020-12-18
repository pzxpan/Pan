package default;

//以下import都Ok
//import std.person.Person;
//import std.person.House;
//import std.person.*;

import color.Color;

fun main() {
    let c = Color::Red(10);
    let d = c.is_warm();
    print(d);
}


