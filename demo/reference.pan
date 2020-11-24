package default;
pub struct House {
    pub size: f64,
    pub price: f64,
    pub fun idea(): bool {
        size = size + 10000;
        return price / size > 10000 as f64;
    }
    fun ::static() :bool {
        return true;
    }
}

pub fun name_call(age: mut i32, name: mut string): bool {
   age = 10 + age;
   return age > 30;
}

pub struct Person {
    pub age: i32,
    pub name: mut string,
    pub house: own House,
    pub const fun is_older(): bool {
        age = 1000 + age;
        return age > 40;
    }
    pub fun older_than(a: i32): bool {
        age = age + 10000;
        return age > a;
    }
}
fun main() {
   let a = 30;
   let b = "aaa";
  // let aa = name_call(a, b);

   a = a + 10;
   print(a);
   let house = House!{size:111.0,price:1_000_000.0};
   let name = "pan";
   let const person = Person!{age:50,name:name,ddd:house.size,house:House!{size:111.0,price:1_000_000.0}};
 //  let c = house.add.clone();
   let bb = person.house.size;
   person.age = 60;
   print(bb);
   let cc = person.house.idea();
}





