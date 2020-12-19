package color;
pub const AAA = 10000;

pub enum Color {
   Red(i32),
   Single(i128),
   White(i32,string),
   Black,
   Three(i32,i32,i32),
   pub fun is_warm() {
           match self {
               Black -> { print(30);}
               White(c,bb)-> { print(bb);}
               Red(a) -> { print(a); }

               Color::Single(a) -> { print(a); }
               Color::Three(a,b,c) -> {print(a); print(b);print(c);}
           }
      }
}
pub struct Person {
    pub age: i32,
    pub name: mut string,
    pub fun older_than(a: i32): bool {
            age = age + 10000;
            return age > a;
    }
    pub fun older_than2(a: i32): bool {
         age = age + 10000;
         return age > a;
    }

    pub fun older_than3(a: i32): bool {
        older_than(40);
        age = age + 10000;
        return age > a;
    }


    pub const fun is_older(): bool {
        //let aaaaaa = older_than(30);
        //let c = typeof(aaaaaa);
        //print(c);
        let ccc = 100;
        ccc = ccc + age;
        return age > 40;
    }

}

pub fun add(a:i32) : i32 {
    return a+ 1000;
}









