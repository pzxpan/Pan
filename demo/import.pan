package default;

//以下import都Ok
//import std.person.Person;
//import std.person.House;
//import std.person.*;
import color;

fun main() {
    let c = color.Color::Red(10);
    let d = c.is_warm();
    let e = color.Person!{name:"pan",age:30};
    print(d);
}


