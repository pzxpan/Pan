package default;
fun main() {
   let a = 100;
   let b = 20_i32;

   let aa = a << 5;
   let bb = b >> 2;
   print(aa);
   print(bb);

   let c = 0b1100110;

   let ff = 20;
   ff += 10;
   print(ff);

   ff -= 10;
   print(ff);
   ff *= 10;
   print(ff);


    ff = 20;
    ff |= 10;
    print(ff);

    ff ^= 10;
    print(ff);

    ff &= 10;
    print(ff);

    print(20000);
    print(ff);

    print(ff);

   // let aa = 100_i128;
   // let bb = 200_i128;
    //向上转型默认安全，向下转型需要明确指定
   // let cc: i32 = aa as i32 + bb as i32;
    //let cc = aa + bb;

    //print(cc);



   let sss = "我们有";
   let s1 = sss + b + "块";
   print(s1);
   return;
}