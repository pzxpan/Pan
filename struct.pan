pub struct Person {
    pub age: int,
    pub name: string,
    pub address: string,
    pub fun is_older(): bool {
        return age > 40;
    }
}
fun main() {
   let person = Person({age:50,name:"pan"});
   let bb = person.is_older();
   print(bb);
   return;
}
fun print(a:int) {
}




