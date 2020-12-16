package color;
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









