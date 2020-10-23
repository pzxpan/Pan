pub struct House {
    pub size :float,
    pub price: float,
    pub fun is_expensive(): bool {
        return price / size > 10000;
    }
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
   let house = House({size:111.0,price:1_000_000.0});
   print(house.is_expensive());
 //  let bb = person.is_older();
   let cc = person.older_than(40);
   person.name = "ddddd";
   print(cc);
   print(person.name);

   return;
}
fun print(a:int) {
}





