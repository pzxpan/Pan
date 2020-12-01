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
pub bound Add<T:Self> {
    pub fun swap(rhs: T) : T {
        return rhs;
    }
}
pub fun type_args(a:Result<i32,string>) : Result<u32,string> {
    let a = 0_u32;
    match a {
        Result::Ok(aa) -> {  print(aa);}
        Result::Err(_) -> {}
    }
    return Result::Ok(a);
}
pub struct XPoint {
    pub x: i32,
    pub y: i32,
}

pub struct Point impl Add {
    pub x: i32,
    pub y: i32,
    pub fun swap(p:Point) : Point {
        x += p.x;
        y += p.y;
        return self;
    }
}

fun main() {
    let a = Ok(40000);
    let c = type_args(a);
    print(c);
}


