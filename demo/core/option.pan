package core.option;
pub enum Option<T> {
    None,
    Some(T),
    pub fun is_some(): bool {
       self!! {return true;}
       return false;
    }
    pub fun unwrap(): T {
        match self {
            Some(v) -> return v;
            _ -> panic("err");
        }
    }
     pub fun is_none() : bool {
       return !is_some();
     }
     pub fun contains<U:PartialEq<T>>(x: U) : bool
     {
        self!! {return self == x; }
        return false;
     }

}

