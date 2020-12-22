package option;

pub enum Option<T> {
    Some(T),
    None,
    pub fun is_some(): bool {
        self == 0 |;
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

}

