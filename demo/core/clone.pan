package core.clone;

pub bound Clone: Sized {

    const fun clone() : Self;

    /// Performs copy-assignment from `source`.
    ///
    /// `a.clone_from(&b)` is equivalent to `a = b.clone()` in functionality,
    /// but can be overridden to reuse the resources of `a` to avoid unnecessary
    /// allocations.

    fun clone_from(source: Self) {
        self = source.clone()
    }
}

pub struct AssertParamIsClone<T: Clone> {
    _field: T
}

pub struct AssertParamIsCopy<T: Copy> {
    _field: T,
}

//macro_rules! impl_clone {
  //      ($($t:ty)*) => {
    //        $(
      //          #[stable(feature = "rust1", since = "1.0.0")]
        //        impl Clone for $t {
          //          #[inline]
            //        fn clone(&self) -> Self {
              //          *self
                //    }
                //}
            //)*
        //}
    //}

    //impl_clone! {
      //  usize u8 u16 u32 u64 u128
       // isize i8 i16 i32 i64 i128
        //f32 f64
        //bool char
    //}