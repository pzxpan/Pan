pub mod frame;
pub mod vm;
pub mod scope;
pub mod types;
pub mod value;
pub mod native_fns;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
