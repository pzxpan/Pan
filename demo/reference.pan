pub struct House {
    pub size: f64,
    pub price: f64,
    pub fun idea(): bool {
        return price / size > 10000;
    }
    fun ::static() :bool {
        return true;
    }
}

pub fun name_call(age:i32, name:string): bool {
   return age > 30;
}

pub struct Person {
    pub age: i32,
    pub name: string,
    pub house: House,
    pub fun is_older(): bool {
        return age > 40;
    }
    pub fun older_than(a: i32): bool {
        return age > a;
    }
}
fun main() {
   let person = Person({age:50,name:"pan",house:House({size:111.0,price:1_000_000.0})});
   let bb = person.older_than(40);
   print(bb);
    let aa = name_call(20,"sdd");
   return;
}





