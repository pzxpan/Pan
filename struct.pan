pub struct House {
    pub size :float,
    pub price: float,
    pub fun idea(): bool {
        return price / size > 10000;
    }
}

pub fun name_call(age:int, name:string) : bool {
   return age > 30;
}
pub struct Person {
    pub age: int,
    pub name: string,
    pub address: string,
    money: int,
    //pub house: House,
    pub fun is_older(): bool {
        return age > 40;
    }
    pub fun older_than(a: int): bool {
        money = 100;
        return age > a;
    }
}
fun main() {
   let person = Person({age:50,name:"pan"});
   let bb = person.is_older();
   let house = House({size:111.0,price:1_000_000.0});
   print(house.idea());
   let aaa = name_call(50,"addd");
   print(aaa);
   let bbb = name_call({age:50,name:"addd"});
   print(bbb);
   let cc = person.older_than(40);
   person.name = "ddddd";
   print(cc);
   print(person.name);

   return;
}
fun print(a:int) {
}





