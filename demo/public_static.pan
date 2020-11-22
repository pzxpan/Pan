package default;
pub struct Person {
    pub name: string,
    pub age: i32,

    pub fun ddd() : bool {
        return age > 0;
    }
}

fun main() {
   let age = 10;
   let namemmm = "addd";
   let person = Person({age,name: namemmm});

   let a = person.ddd();
   print(person.age);
   print(person.name);
   return;
}





