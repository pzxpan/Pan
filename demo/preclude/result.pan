package result;

pub enum Result<T, E> {
    Ok(T),
    Err(E),
    pub const fun is_ok() : bool {
          self == 0 |;
    }
    pub const fun map<U, F: fun(T)->U> (op: F): Result<U, E> {
       match self {
             Ok(t) -> {
                let a = op(t);
                let f = Result::Ok(a);
                return f;
             }
             Err(e) -> return Result::Err(e);
        }
   }

   pub const fun map_or<U, F: fun(T) -> U>(default: U, f: F) : U {
         match self {
               Ok(t) -> return f(t);
               Err(_) -> return default;
         }
   }
    pub const fun is_err() : bool {
        return !self.is_ok();
   }

   pub const fun unwrap(): T {
        match self {
           Ok(t) -> { return t; }
           Err(e) -> { panic("cccc");}
        }
     }

    pub fun and_then<U, F: fun(T) -> Result<U, E>> (op: F) : Result<U, E> {
           match self {
               Ok(t) -> return op(t);
               Err(e) -> return Err(e);
           }
     }
}








