pub struct House {
    pub size: f64,
    pub price: f64,
    fun idea(): bool {
        return price / size > 10000;
    }
    fun ::static() :bool {
        return true;
    }
}

pub fun name_call(age:i32, name:string) : bool {
   return age > 30;
}
pub struct Person {
    pub age: i32,
    pub name: string,
   // pub address: string,
  //  money: i32,
    //pub house: House,
    pub fun is_older(): bool {
        return age > 40;
    }
    pub fun older_than(a: i32): bool {
        //money = 100;
        return age > a;
    }
}
fun main() {
   let person = Person({age:50,name:"pan"});
   let bb = person.is_older();
   let house = House({size:111.0,price:1_000_000.0});
   //私有的
   //print(house.idea());
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





