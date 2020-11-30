package default;
pub enum Result<T, E> {
    /// Contains the success value
    Ok(T),
    Err(E),
    pub const fun is_ok() : bool {
        match self {
            Ok(_) -> {return true;}
            _ -> {return false;}
        }
        return false;
    }

    pub const fun is_err() : bool {
        match self {
               Ok(_) -> {return false;}
           _ -> {return true;}
         }
        return true;
   }
}

fun main() {
   let x: Result<u32,string> = Result::Ok(2);
   print(x.is_ok());
}




