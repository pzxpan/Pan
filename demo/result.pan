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
   let x: Result<u32,Result<u32,string>> =  Result::Ok(2);
   let xx: [i32:4] = [1,2,3,4];
   let xxx: (i32,string) = (12,"ddd");

   print(xx[1]);
   print(xxx.1);
   print(x.is_ok());
   let a = 10;
   if a  >= 0 {
       print(a);
   }
   let a = 2000;
   a >>= 4;
   print(a);
}



