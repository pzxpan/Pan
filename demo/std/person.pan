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





