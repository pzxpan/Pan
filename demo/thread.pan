pub struct Thread {
    pub f: FunType,
    state: i32,
    pub fun ::new(f:FunType): Thread {
        return Thread ({f,state:0});
    }
    pub fun run() :Any {
        self.state = 1;
        f();
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
    let t = Thread::new(() => {
        let cc = add(aa,bb);
        print(cc);
    });
    t.run();
    t.stop();
    cc = 1000;
    print(cc);
}









