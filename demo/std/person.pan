pub struct House {
    pub size: float,
    pub price: float,
    fun idea(): bool {
        return price / size > 10000;
    }
    fun ::static() :bool {
        return true;
    }
}

pub fun name_call(age:int, name:string) : bool {
   return age > 30;
}
pub struct Person {
    pub age: int,
    pub name: string,
   // pub address: string,
  //  money: int,
    //pub house: House,
    pub fun is_older(): bool {

        return age > 40;
    }
    pub fun older_than(a: int): bool {
        //money = 100;
        return age > a;
    }
}





