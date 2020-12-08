package default;
pub struct Thread {
    f: fun()->None,
    state: i32,
    pub fun ::new(fff: fun()->None): Thread {
        return Thread! {f:fff,state:0};
    }
    pub fun run() {
        self.state = 1;
        self.f();
    }
    pub fun stop() {
        self.state = 2;
    }
}

pub fun add(a:i32,b:i32) : i32 {
    return a + b;
}
fun main() {
    let arr = [0,1,2,3,4];
    let t1 = Thread::new(() => {

            print("第一个子线程");
            print("第一个子线程..暂停2秒");
            sleep(2000);
            for i in 0..5 {

               arr[i] += 1000;
            }
            print("第一个子线程..暂停2秒结束");
          //  print(arr);
        });
   let t2 = Thread::new(() => {
             print("第二个子线程");
             print("第二个子线程..暂停2秒");
             sleep(3000);
             for ii in 0..5 {

                print(arr[ii]);
             }
             print("第二个子线程..暂停2秒结束");
            // print(arr);
            });

    t2.run();
    t1.run();
    sleep(6000);
    print(arr);
}









