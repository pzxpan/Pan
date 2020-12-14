package default;
//import std.person.*;
//以下import都Ok
//import std.person.Person;
//import std.person.House;
//import std.person.*;
//import std.color.Color;

pub fun add_pub(cc:i32) : i32 {
    return cc - 100;
}
fun main() {
    let a = add_pub(30);
    print(a);
}


