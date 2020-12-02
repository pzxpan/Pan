package default;
pub enum Result<T, E> {
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
   pub const fun unwrap(): T {
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
    return Result::Ok(aaa);
}

fun main() {
    let adddd = Result::Ok(400000);
    let b = adddd.unwrap();
    let ccc = b - 2 ;
    print(ccc);
    let aa = typeof(adddd);
    print(aa);
    let c = type_args(adddd);
    print(b);
}


