package thread;

pub struct Thread {
    pub name: string,
    pub id: i32,
}

pub struct Mutex<T> {
    pub value: T,
}

pub fun run(f:fun() -> None,join: bool = true) {}

pub fun sleep(milli_seconds: i32) {}













