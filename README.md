# Pan语言
##一、概括:
####1. Pan 语言是一种以LLVM为编译后端的编程语言，既能编译执行，又能解析执行；用Rust实现其解释器，用LLVM实现其编译器；
####2. 语言的顶级抽象，“事物的本质只能极限逼近的描述，而无法被绝对精确的定义, 定义都具有历史局限性”；
####3. 万物皆有型,引用除外!!! 引用会发生自指循环，陷入逻辑悖论; 由编译器对引用进行有效的管理，不需要从代码层面加以区分;
##二、特点:
####1.语法简单，除约定俗成的符号外，很多符号都有表意的特点，如：
      1.用'|;'用来表示简化的返回，'|'具有挡板的表意，让其阻止流程并返回；
      2.用'!!'表示从Option、Result中强制取出第一顺位的值，成功，就将值保存给当前变量的名称，在其后的块中使用；如果失败，则跳过后续的块；
       尤其在其wrap较深的时候，能大大简化代码，聚焦核心;
      3.用'::'表示静态函数，因为静态函数函调用的时候大多数情况下都要'::'的符号；
####2.静态类型，enum分型定值，无继承、面向接口编程、融合函数式和面向对象的主要特征,支持函数作为参数和返回值；
####3.对照参考语言Haskell，Python，C；
##三、规划:
#### 1.2021年0.1版本,解释器发布,2021年3月,0.1版本编译器发布; 1.0版本实现自编译;


