Starting with this source code:

fn main() {
    println!("Hello, world!");
}
You can create LLVM intermediate representation (IR) or bitcode (BC):

# IR in hello.ll
rustc hello.rs --emit=llvm-ir
# BC in hello.bc
rustc hello.rs --emit=llvm-bc
These files can then be further processed by LLVM to produce assembly or an object file:

# Assembly in hello.s
llc hello.bc
# Object in hello.o
llc hello.bc --filetype=obj
Then you need to link the files to produce an executable. This requires linking to the Rust standard libraries. The path is platform- and version-dependent:

cc -L/Users/cuiqingbo/Desktop/Pan/rust/build/x86_64-apple-darwin/stage1-std/x86_64-apple-darwin/release -lstd -o test test.bc
You can then run the program:

DYLD_LIBRARY_PATH=/path/to/stage2/lib/rustlib/x86_64-apple-darwin/lib/ ./hello2
This answer has OS X specific solutions, but the general concepts should be extendable to Linux and Windows. The implementation will differ slightly for Linux and probably greatly for Windows.


