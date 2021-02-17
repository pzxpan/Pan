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
    pub ddd: f64,
    pub fun older_than(a: i32): bool {
            age = age + 10000;
            return age > a;
    }
    pub fun older_than2(a: i32): bool {
         age = age + 10000;
         return age > a;
    }

    pub fun older_than3(a: i32): bool {
        older_than(40);
        age = age + 10000;
        return age > a;
    }


    pub const fun is_older(): bool {
        //let aaaaaa = older_than(30);
        //let c = typeof(aaaaaa);
        //print(c);
        let ccc = 100;
        ccc = ccc + age;
        return age > 40;
    }

}
fun main() {
   let a = 30;
   let b = "aaa";
   let aa = name_call(a, b);
   a = a + 10;
   print(a);
   let house = House!{size:111.0,price:1_000_000.0};
   let name = "pan";
   let person = Person!{age:50,name:name,ddd:house.size,house:House!{size:111.0,price:1_000_000.0}};
 //  let c = house.add.clone();
  // person.age = 40;
  // person.older_than3(40);
   person.is_older();
   person.age = 60;
   let bb = person.house.size;
   print(person.age);
   print(bb);
   let h = person.house.idea();
   //let cc = h.idea();
   print("{}",h);
   //person.house.size = 10.0;
   print("{}",person.house.size);

   let ddd = "panddd";
   print(ddd);
}





