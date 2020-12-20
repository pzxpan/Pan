package default;

import std.env.*
import std.io.*
import std.fs.*

fun main() {
    print("请输入一个数字:");
    let c = read_int();
    print("您输入的是:0x{:0X}",c);
    let cc = read_float();
    print("您输入的浮点数是:{:.3f}",cc);
    let file_content = read_file("/Users/panzhenxing/Desktop/PanPan/Pan/demo/for.pan");
    write_file("/Users/panzhenxing/Desktop/PanPan/Pan/demo/test_input.pan",file_content);

    let f = file_exists("/Users/panzhenxing/Desktop/PanPan/Pan/demo/for.pan");
    print("文件是否存在:{}",f);

    if f {
        delete_file("/Users/panzhenxing/Desktop/PanPan/Pan/demo/test_input.pan");
    }
    print(file_content);
}




