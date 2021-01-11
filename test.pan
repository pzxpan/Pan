bound AA {
    fun add(&self,a:i32) -> i32;
}
pub struct D {
pub d:i32
}
impl AA for D {
    fun add(&self,a:i32)->i32 {
        return self.d + a;
    }
}
fun main() {
    let dd = D {d:20};
    let a = dd.add(30);
    println!("add:{:?}",a);
    println!("helloeeeeeworld");
}