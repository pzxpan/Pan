package default;
pub enum Result<T, E> {
    /// Contains the success value
    Ok(T),
    Err(E),
    pub const fun is_ok() : bool {
        self == 0 |;
    }

    pub const fun is_err() : bool {
        self != 0 |;
   }
   pub const fun unwrap() : T {
        match self {
            Ok(t) -> { return t; }
            _ -> { panic("error"); }
         }

   }
}

fun main() {
   let x: Result<u32,string> = Result::Ok(30_u32);
   print(x.is_ok());
   print(x.unwrap());
}




