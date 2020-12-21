package default;

import std.env.*
import std.io.*
import std.fs.*

import std.http.*
import std.thread.*;

fun main() {

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

   let c = () => {
        //let sum = 0;
        let sum = Mutex! {value:0};
        for i in 0..=100  {
          sum.value += i;
        }
        print(sum);
   };

   c();
 //  print(sum);
  // run(c);
}




