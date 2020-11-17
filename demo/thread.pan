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

pub fun sub(a:i32,b:i32) : i32 {
    print("{}+{}={}",a,b,a-b);
    return a - b;
}
pub fun ddd() {
    let aa = 10;
    let bb = 20;
    let a = sub(aa,bb);
    print(a);
}


fun main() {
    let t = Thread::new(ddd);
    t.run();
    t.stop();
}









