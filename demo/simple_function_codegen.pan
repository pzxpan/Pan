package default;

pub struct AAA {
    pub aaa: i32,
    pub bbb: i32,
    pub fun change_aaa() :i32 {
        self.aaa = 1000;
        let cc = self.aaa + 200;
        return cc;
    }
    pub fun change_bbb_return_aaa() :i32 {
        self.bbb = 20000;
        let c = change_aaa();
        return c;
    }
}

pub fun add(a:i32,b:i32) :i32 {
    return a + b;
}

fun main():i32 {
   let aa = AAA!{aaa:30,bbb:10000};
   let cc = aa.change_aaa();
   let a = aa.aaa;
   print("aa%d",a);
   print("%d",cc);
   let bb = 0;
   return 0;
}
 //%tmp = call i32 (i8*, ...) @printf([3 x i8]* @outputs)\n
 //let add= (a:i32,b:i32)=> {
   //             let c =  a + b;
    //            return c;
     //       };




