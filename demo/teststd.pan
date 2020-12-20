package default;

import std.env.*
import std.io.*

fun main() {
    print("请输入一个数字:");
    let c = read_int();
    print("您输入的是:0x{:0X}",c);
    let cc = read_float();
    print("您输入的浮点数是:{:.3f}",cc);
}




