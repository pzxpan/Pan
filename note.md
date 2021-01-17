Starting with this source code:

fn main() {
    println!("Hello, world!");
}
You can create LLVM intermediate representation (IR) or bitcode (BC):

# IR in hello.ll
rustc hello.rs --emit=llvm-ir
# BC in hello.bc
rustc hello.rs --emit=llvm-bc
These files can then be further processed by LLVM to produce assembly or an object file:

# Assembly in hello.s
llc hello.bc
# Object in hello.o
llc hello.bc --filetype=obj
Then you need to link the files to produce an executable. This requires linking to the Rust standard libraries. The path is platform- and version-dependent:

cc -L/Users/panzhenxing/Desktop/Pan/rust/build/x86_64-apple-darwin/stage1-std/x86_64-apple-darwin/release -lstd -o test test.bc
You can then run the program:

DYLD_LIBRARY_PATH=/path/to/stage2/lib/rustlib/x86_64-apple-darwin/lib/ ./hello2
This answer has OS X specific solutions, but the general concepts should be extendable to Linux and Windows. The implementation will differ slightly for Linux and probably greatly for Windows.

llvm-as aa.ll -o aa.bc
llvm-dis aa.bc 
cc aa.bc 生成a.out

rustc --emit=asm,llvm-ir test.rs
ELF(Executable and Linking Format)是一种对象文件的格式

1) .text section 里装载了可执行代码；

2) .data section 里面装载了被初始化的数据；

3) .bss section 里面装载了未被初始化的数据；

4) 以 .rec 打头的 sections 里面装载了重定位条目；

5) .symtab 或者 .dynsym section 里面装载了符号信息；

6) .strtab 或者 .dynstr section 里面装载了字符串信息；

7) 其他还有为满足不同目的所设置的section，比方满足调试的目的、满足动态链接与加载的目的等等。

Identifiers 标识符

    @ 全局
    % 局部

    后接字符串      命名量      @name %name
        无符号数字  未命名量    @42   %42


类型系统

    void                                空类型
    <type> *                            指针类型
    <returntype> (<parameter list>)     函数类型
    < <# elements> x <elementtype> >    向量类型
    [<# elements> x <elementtype>]      数组类型
    { <type list> }                     普通结构体类型
    <{ <type list> }>                   打包结构体类型
    metadata                            元数据类型
    label                               标签类型
    token                               词元类型
    %Color = type { [0 x i32], i32, [11 x i32] }
    %"Color::Red" = type { [0 x i64], double, 
    [0 x i64], %"alloc::string::String",
     [0 x i32], i32, [0 x i32], float, [0 x i32] }

类型系统(例子)

    void                   空
    i32 *                  指针
    i32 (i32)              函数
    <5 x i32>              向量
    [5 x i32]              数组
    { i32, i32, i32 }      普通结构体
    <{ i32, i32, i32 }>    打包结构体

元数据

    ; 未命名元数据节点
    ; 用于被命名元数据引用
    !0 = !{!"zero"}
    !1 = !{!"one"}
    !2 = !{!"two"}

    ; 命名元数据
    !name = !{!0, !1, !2}

    !name --- !0
          |-- !1
          |-- !2

模块层次内联汇编

    module asm "内联汇编代码"

Target Triple

    target triple = "x86_64-amd64-freebsd"

First Class Types 第一类型
    Single Value Types 单值类型
        只在寄存器里头有效

        Integer Type 整数类型

            iN ;N为比特数 (通用描述)

            i1 一个比特整数
            i32 32为整数

        Floating-Point Types 浮点类型

            half      - 16位浮点值
            float     - 32位浮点值
            double    - 64位浮点值
            fp128     - 128位浮点值
            x86_fp80  - 80位浮点值
            ppc_fp128 - 128位浮点值

模块结构
    程序由模块组成，每个模块都是输入程序的翻译单元。
    Hello, world 模块

    ; 定义字符串常量作为全局常量
    @.str = private unnamed_addr constant [13 x i8] c"hello world\0A\00"

    ; 外部声明的 puts 函数
    declare i32 @puts(i8* nocapture) nounwind

    ; main 函数的定义
    define i32 @main() {   ; i32()*
      ; [13 x i8] 转换到 i8...
      %cast210 = getelementptr [13 x i8], [13 x i8]* @.str, i64 0, i64 0

      ; 调用 puts 函数，将字符串写入 stdout
      call i32 @puts(i8* %cast210)
      ret i32 0
    }

    ; 命名元信息
    !0 = !{i32 42, null, !"string"}
    !foo = !{!0}

指令参考
=========

Terminator Instructions
    指示当前块完成后，执行哪个块。
    终结指令典型的产生一个 void 值:他们影响控制流，而不是值。(invoke指令是一个例外)

    ret            返回
    br             二元条件分支/无条件转移
    switch         多条件分支
    indirectbr    
    invoke         普通/带异常调用
    resume         抛出异常?
    catchswitch    捕获异常
    catchret   
    cleanupret   
    unreachable    不可到达(无语义)

Binary Operations

    add    加
    fadd   浮点加
    sub    减
    fsub   浮点减
    mul    乘
    fmul   浮点乘
    udiv   无符号整数除
    sdib   带符号整数除
    fdiv   浮点除
    urem   无符号整数求余
    srem   带符号整数求余
    frem   浮点数求余

    运算\类型    无符号整数   带符号整数   浮点数
        +               add             fadd
        -               sub             fsub
        *               mul             fmul
        /       udiv        sdiv        fdiv
        %       urem        srem        frem

Bitwise Binary Operations

    shl    左移
    lshr   逻辑右移
    ashr   算数右移
    and    与
    or     或
    xor    异或

Vector Operations

    extractelement 取出元素
    insertelement  插入元素
    shufflevector 

Aggregate Operations

    extractvalue  取出值
    insertvalue   插入值

Memory Access and Addressing Operations

    alloca           分配内存
    load             从内存加载
    store            储存到内存
    fence
    cmpxchg
    atomicrmw        自动修改内存
    getelementptr    获取 aggregate(集合) 数据结构的子成员地址

Coversion Operations

    这个类型为转换指令(强制类型转换|铸造casting)
    都取一个单一运算对象和一个类型。
    对运算对象提供一系列位转换。

    trunc .. to            截断转换
    zext .. to             零扩展转换
    sext .. to             符号位扩展转换
    fptrunc .. to          浮点截断转换
    fpext .. to            浮点扩展
    fptoui .. to           浮点转无符号整数
    fptosi .. to           浮点转带符号整数
    uitofp .. to           无符号整数转浮点
    sitofp .. to           带符号整数转浮点
    ptrtoint .. to         指针转整数
    inttoptr .. to         整数转指针
    bitcast .. to          位模式转换(重新解释，不改变任何二进制位)
    addrepacecast .. to    地址空间转换
    
Other Operations

    icmp          整数比较
    fcmp          浮点数比较
    phi           φ 节点
    select        条件值选择
    call          简单函数调用
    va_arg        可变参数
    landingpad
    catchpad
    cleanuppad

