package default;

import std.fs.*;
import std.thread.*;



fun main() {

    // let file_content = read_file("/Users/panzhenxing/Desktop/PanPan/Pan/demo/for.pan");
   // print(file_dir);
   // let file_content = read_file("/Users/panzhenxing/Desktop/PanPan/Pan/demo/for.pan");
    //write_file("/Users/panzhenxing/Desktop/PanPan/Pan/demo/test_input.pan",file_content);

   // let f = file_exists("/Users/panzhenxing/Desktop/PanPan/Pan/demo/for.pan");
   // print("文件是否存在:{}",f);

   // if f {
    //    delete_file("/Users/panzhenxing/Desktop/PanPan/Pan/demo/test_input.pan");
    //}
    //  print(file_content);
   // let ccc = open("http://www.baidu.com");
   // print(ccc);
   let mutex = Mutex! {value:"pan"};
   for i in 0..10 {
      let c = () => {
         let s = 4 - i;
         print("sss is :{:d}",s);
         //print(s);

         mutex.value += s + "aaa";
      };
     run(c);
   }

   sleep(10000);
   let v = mutex.value;
   print(v);
}




