pub struct House {
    pub size: float,
    pub price: float,
    pub address: string,
}

pub struct Person {
    pub age: int,
    pub name: string,
    pub address: string,
    pub fun is_older(): bool {
        return age > 40;
    }
}
fun main() {
  let aaaa = Person({age:20}) ;
 // print(a.is_older());
  return;
}
fun print(a:int) {
}




