package core.bool;

pub struct bool {
    pub fun then_some<T>(t: T) : Option {
        return if self : Some(t)  else:  None;
    }
    pub fun then<T, F: FunType>(f: F) : Option{
        return if self : Some(f())  else:  None;
    }
}