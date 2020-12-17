package std.person;

pub struct House {
    pub size: f64,
    pub price: f64,


    pub fun idea(): bool {
        return price / size > 10000.0;
    }
    pub fun ::static() :bool {
        return true;
    }
}
pub fun add_pub(a:i32) :i32 {
    return a + 100111;
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

    pub fun add_pub(a:i32) :i32 {
        return a - 10000;
    }
    pub fun is_older(): bool {
      let c = add_pub(self.age);
      print(c);
      return age > 20;
    }
}

fun add(a:i32,b:i32) : i32 {
    return a + b;
}







