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
                return Result::Ok(op(t));
             }
             Err(e) -> return Result::Err(e);
        }
   }
    pub const fun is_err() : bool {
        return !self.is_ok();
   }
}
pub fun to_str(a:i32):string {
    return a + "";
}
fun main() {
   let x: Result<u32,Result<u32,string>> =  Result::Ok(2);
   let xx: [i32:4] = [1,2,3,4];
   let xxx: (i32,string) = (12,"ddd");
   let a_str:fun(i32) -> string = to_str;
   let aaa = Result::Ok(40);
   let cc = aaa.map(a_str);

}




