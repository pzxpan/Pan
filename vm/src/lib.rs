pub mod frame;
pub mod vm;
pub mod scope;
pub mod native_fns;
pub mod util;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
