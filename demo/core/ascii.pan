package core.ascii;
pub struct EscapeDefault impl Iterator {
    range: Range,
    pub fun escape_default(c: u8) : EscapeDefault {
    let  hexify = (b: i8) => {
                match b {
                    0 -> '0' + b;
                    _ -> 'a' + b - 10;
                }
            };
        return EscapeDefault! { range: len, d };
    }
     fun next() : u8 {
            return range.next().map((i:i32) => {return self.d[i];});
        }
    fun size_hint() : (usize, Option) {
            return self.range.size_hint();
        }
    fun last() : u8 {
            return self.next_back();
        }
}
