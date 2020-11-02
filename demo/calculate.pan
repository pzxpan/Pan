fun main() {
   let a = 100;
   let b = 20_i32;
   let d = a / b;
   print(d);

   let aa = a << 5;
   let bb = b >> 2;
   print(aa);
   print(bb);

   let c = 0b1100110;
   let d = 0b1010101;
   let dd = c | b;
   let ee = c & d;
   let ff = c ^ d;
   print(dd);
   print(ee);
   print(ff);

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
    ff = 0;
    ff %= 10;
    print(ff);



   let sss = "我们有";
   let s1 = sss + b + "块";
   print(s1);
   return;
}