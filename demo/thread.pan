package default;
pub struct Thread {
    f: FunType,
    state: i32,
    pub fun ::new(fff: FunType): Thread {
        return Thread! {f:fff,state:0};
    }
    pub fun run() :Any {
        self.state = 1;
        self.f();
    }
    pub fun stop() : Any {
        self.state = 2;
    }
}

pub fun add(a:i32,b:i32) : i32 {
    return a + b;
}
fun main() {
    let aa = 20;
    let bb = 30;
    let c = add(aa,bb);
    let cc = add(1000,200000);
    print(cc);
    print(c);
    let t1 = Thread::new(() => {
            let ccccc = add(aa,bb);
            print("第一个子线程");
            print("第一个子线程..暂停4秒");
            sleep(4000);
            print("第一个子线程..暂停4秒结束");
            print("{}+{}结果为:{}",aa,bb,ccccc);
        });
         let t2 = Thread::new(() => {
                let cc = add(1000,bb);
                print("第二个子线程");
                print("第二个子线程..暂停2秒");
                sleep(2000);
                print("第二个子线程..暂停2秒结束");
                print("1000+{}结果为:{}",bb,cc);
            });
    t2.run();
    t1.run();
}









