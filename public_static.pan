pub struct House {
    pub size: int,
    pub price: float,
    inner: int,
    fun close(): bool {
        return size < 1000;
    }
    pub fun open(): bool {
       return close();
    }

    fun ::astatic(): bool {
        return 100 > 0;
    }
    fun ::new() : House {
        return House({size:11,price:1_00_0000.0,inner:20});
    }
}


fun main() {
   let house = House::new();
//   let house2 = House({size:111,price:1_000_000.0,inner:20});
   //私有的，无法通过生成sybmol;
   //house.close();
   //公开的，可生成;
   print(house.open());
  // let ddd = House::astatic();
 //  house.astatic();
 //  print(ddd);
   return;
}
fun print(a:int) {
}





