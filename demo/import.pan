package default;

//以下import都Ok
//import std.person.Person;
//import std.person.House;
//import std.person.*;

import color.AAA;
import color.Color;

fun main() {
    let a = color.AAA;
    print(a);
    let c = Color::Red(10);
    let d = c.is_warm();

    print(d);
}


