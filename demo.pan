
fun main() {
    let a: Person = Person.new("pan",1000,28,"Beijing");
    let b: Person = Person.new("wang",2000,38,"Shanghai");
    let d: bool = a.richer_than(b);
    if d {
        print("a 比 b 富有");
    } else {
        print("b 比 a 富有");
    }
    let c: int = other(20,10);
    print(c);
}

fun other(a: int, b:int) int {
    return a - b;
}

fun is_richer(a: Person, b: Person)  bool {
    if a.money < b.money {
        return true;
    } else {
        return false;
    }
}

struct Person {
   pub money: uint,
   pub age: uint,
   pub address: String,
   pub fun is_richman() bool {
        return money > 1000000;
   }
   pub fun new(money: uint, age: uint, address: String) Person {
        return Person(money,age,address);
   }

   pub fun richer_than(other:Person) bool {
        return money > other.money;
   }
}
struct Foo {
    pub fun add(a : int, b: int) uint {
        let a: int  = 20;
        a .+ 20;
        return a + b;
    }
}
