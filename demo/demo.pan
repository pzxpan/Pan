fun main() {
   let cc = 30;
   let a = 0;
   let b = 20;

    a +=b;
    a -=b;
    a *=b;
    a /=b;
    a %=b;

    a <<=b;
    a >>=b;
    a ^=b;
    a |=b;
    a &=b;

    let cccc = (aaa:int,bbb:int) => {
        return a + b + aaa;
    };
    a =  if a > 0 : b
         elif a > 20: 20
         else : 20;

   if a > 10 && b < 30 {
        print("a>0,b<30");
   } elif cc > 10 {
        print("cc:");
        print(cc);
   }
   return;
}
fun print(a:int) {
}