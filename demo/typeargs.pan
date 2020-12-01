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
   pub const fun unwrap() {
       match self {
          Ok(tttt) -> {return tttt;}
          Err(e) -> {}
       }
    }
}

pub fun type_args(a: Result<i32,string>) : Result<u32,string> {
    let aaa = 0_u32;
    match a {
        Result::Ok(aa) -> { print(aa);}
        Result::Err(_) -> {}
    }
    return Result::Ok(a);
}

fun main() {
    let a = Result::Ok(40000);
    let b = a.unwrap();

    let aa = typeof(a);
    print(aa);
    let c = type_args(a);
    print(b);

}


