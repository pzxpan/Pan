struct Foo {
    pub function add(a : int, b: int) uint {
        let a: int  = 20;
        a .+ 20;
        return a + b;
    }
}
function main() {
    let a: foo = foo();
    let b: int = a.add(10,20);
    print(b);
}
