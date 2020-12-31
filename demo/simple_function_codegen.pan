package default;

pub struct AAA {
    pub aaa: i32,
    pub bbb: i32,
    pub fun change_aaa() :i32 {
        self.aaa = 1000;
        let cc = self.aaa + bbb;
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
   let bbbb = AAA!{aaa:30,bbb:10000};
   let cc = bbbb.change_bbb_return_aaa();
   let a = bbbb.aaa;
   print("我们是 %d",a);
   let bb = 0;
   return 0;
}
 //%tmp = call i32 (i8*, ...) @printf([3 x i8]* @outputs)\n
 //let add= (a:i32,b:i32)=> {
   //             let c =  a + b;
    //            return c;
     //       };




