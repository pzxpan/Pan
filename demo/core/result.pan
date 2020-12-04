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

    pub const fun map<U, F: fun(T)->U>(op: F) : Result<U, E> {
       match self {
             Ok(t) -> {
                let addaaa = op(t);
                let fffff = Result::Ok(addaaa);
                return fffff;
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
           Ok(t) -> {return t;}
           Err(e) -> {panic("cccc");}
        }
     }
}

pub fun to_str(a:i32): string {
    return a + "";
}

fun main() {
   let x: Result<u32,Result<u32,string>> =  Result::Ok(2);
   let xx: [i32:4] = [1,2,3,4];
   let xxx: (i32,string) = (12,"ddd");
   let a_str: fun(i32) -> string = to_str;
   let aaa = Result::Ok(40);
   print(aaa.is_ok());
   let cc = aaa.map(a_str);
   let ccc = cc.is_ok();
   print(cc);
   let i = 40_u64;
   let ab = Result::Err("eeee");
}




