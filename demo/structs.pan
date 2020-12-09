package default;
const ababab = 20;
pub struct House {
    pub size: f64,
    pub price: f64,
    fun idea(): bool {
        return price / size > 10000.0;
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
    pub fun change_age(a:i32) {
        self.age = a;
    }
    pub fun older_than(a: i32): bool {
        //money = 100;
        let aabb = () => {
            age += 10;
        //    let aa = 10000;
            return age + 20;
        };
        let aa = aabb();
        age += aa;
        let a = self.age;
        self.age = 1000000;

        print("age:{:d}", age);
        return age > a;
    }

    pub fun ::ceshi(a:i32) :i32 {
        return a + 1000;
    }
}


fun main() {
   let person_map = {0:"a"};
   for i in 0..10 {
     person_map[i] = "pan"+i;
   }

   print(person_map[2]);
  // let c = sizeof(person_map);
   print("len:{}",10);
   return;
}





