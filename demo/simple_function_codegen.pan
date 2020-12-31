package default;

pub struct AAA {
    pub aaa: i32,
    pub bbb: i32,
    pub fun get_aaa() :i32 {
        let cc = aaa + 10;
        return cc;
    }
}

pub fun add(a:i32,b:i32) :i32 {
    return a + b;
}

fun main():i32 {
   let aa = AAA!{aaa:30,bbb:10};
   let cc = aa.get_aaa();
   print("cc %d",cc);
   let bb = 0;
   return 0;
}
 //%tmp = call i32 (i8*, ...) @printf([3 x i8]* @outputs)\n
 //let add= (a:i32,b:i32)=> {
   //             let c =  a + b;
    //            return c;
     //       };




