struct Foo {
    pub fun add(a : int, b: int) uint {
        let a: int  = 20;
        a .+ 20;
        return a + b;
    }
}
fun main() {
    let a: foo = foo();
    let b: int = a.add(10,20);
    print(b);
}
