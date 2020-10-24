pub struct House {
    pub size: int,
    pub price: float,
    fun close(): bool {
        return price / size > 10000;
    }

    pub fun open(): bool {
            return price / size > 10000;
        }

    fun ::astatic(): bool {
        return size > 0;
    }
}


fun main() {
   let house = House({size:111,price:1_000_000.0});
   //私有的，无法通过生成sybmol;
   //house.close();
   //公开的，可生成;
   house.open();
  // let ddd = House::astatic();
   return;
}
fun print(a:int) {
}





