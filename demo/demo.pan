package default;
fun main() {
   let cc = 30;
   let a = 40;
   let b = 20;

  //  a +=b;
  //  a -=b;
   // a *=b;
   // a /=b;
    //a %=b;

    //a <<=b;
    //a >>=b;
    //a ^=b;
    //a |=b;
    //a &=b;

    let add = (aa:i32,bb:i32) => {
        return a  + aa + bb ;
    };
   let ddd = add(20,30);
   let d = typeof(ddd);
   print(d);
   print("lambda(aa=20,b=30),upvalue=40,值为:{:d}",ddd);
   if a > 10 && b < 30 {
        print("a>0,b<30");
   } elif cc > 10 {
        print("cc:");
        print(cc);
   }
   return;
}