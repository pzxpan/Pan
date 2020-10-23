pub struct Person {
    pub age: int,
    pub name: string,
    pub address: string,
    pub fun is_older(): bool {
        return age > 40;
    }
}
fun main() {
   let aaaa = Person({age:50,name:"pan"});
   let bb = aaaa.is_older();
   print(bb);
   return;
}
fun print(a:int) {
}




