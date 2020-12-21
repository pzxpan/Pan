package thread;

//默认是join

pub struct Thread {
    pub name:string,
    pub id:i32,
}

pub struct JoinHandler {
    t: Thread,
    pub fun join() {}
}
pub fun run(f:fun()->None): JoinHandler {}

pub fun sleep(time: i32) {}

pub fun join() {}












