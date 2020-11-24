package default;
const ababab = 20;
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
    pub house: House,
    fun fff() : bool {
        return true;
    }
    pub fun is_older(): bool {
        let c =  self.fff();
        return c;
    }
    pub fun older_than(a: i32): bool {
        //money = 100;
        let aabb = () => {
            return age + 1000;
        };
        let aa = aabb();
        age += aa;
        let a = self.age;

        print("age:{:d}", age);
        return age > a;
    }
}

fun main() {
   let person = Person!{age:50,name:"pan",house: House!{size:111.0,price:1_000_000.0}};
  // let bb = person.is_older();
   let age = 40;
   let ddd = 40;
   let house = House!{size:111.0,price:1_000_000.0};
   //私有的
   //print(house.idea());
    let aaaaaa = () => {
               age += 100;
           };
           aaaaaa();
   let aaa = name_call(50,"addd");
   print(aaa);
   let bbb = name_call!{age:50,name:"addd"};
   print(bbb);
   let cc = person.older_than(40);
 //  person.name = "ddddd";
   print(cc);
   print(person.name);

   return;
}





