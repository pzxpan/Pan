fun main() {
  let a = 'c';
  let b = 2128333344444212833334444421283333_i128;
  let c = 2231231.6233241;
  print("我们放飞了{:d}个,还有多少个{:#d}气球,总共花了{:.2}块",a as u16,b,c);

  let cc = 55_u8 as char;
  let dd = 128_u8;
  let ee = dd as char;
  print(cc);

  let ff = format("有{:#d}个民族，有{:.2}亿GDP",56,400.0);
  print(ff);

  let gg = typeof(ff);
  print(gg);
  return;
}





