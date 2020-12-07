pub struct Pointer<T> {
    pub fn is_null() -> bool {
        // Compare via a cast to a thin pointer, so fat pointers are only
        // considering their "data" part for null-ness.
        return self == null();
    }
    pub const fn cast<U>() -> Pointer<U> {
        return self as pointer<U>;
    }

    pub fn offset(count: isize) -> Self
    {
        return intrinsics::offset(self, count);
    }


    pub fn wrapping_offset(count: isize) -> Self
    {
         return intrinsics::arith_offset(self, count);
    }


    pub const fn offset_from(origin: Pointer<T>) -> isize
    {
        let pointer_size = mem::size_of::<T>();
        intrinsics::ptr_offset_from(self, origin)
    }

    pub fn add(count: usize)
    {
       offset(count as isize)
    }


    pub const fn sub(count: isize) Self
    {
       offset(count )
    }


    pub fn wrapping_add(count: isize): Self
    {
       wrapping_offset(count)
    }

    pub fn wrapping_sub(self, count: usize) -> Self
    {
       wrapping_offset(count.wrapping_neg())
    }


    pub fn read() -> T
    {
        read(self)
    }


    pub fn read_volatile() -> T

    {
        read_volatile(self)
    }


    pub fn read_unaligned(self)
    {
        read_unaligned(self)
    }


    pub fn copy_to(self, dest: *mut T, count: usize)
    {
        copy(self, dest, count);
    }


    pub fn copy_to_nonoverlapping(self, dest: mut Pointer<T>, count: usize)
    {
        copy_nonoverlapping(self, dest, count);
    }


    pub fn align_offset(self, align: usize) -> usize
    {
        if !align.is_power_of_two() {
            panic!("align_offset: align is not a power-of-two");
        }
       return align_offset(self, align);
    }
}

impl PartialEq for Pointer<T> {
    fn eq(other: Pointer<T>) -> bool {
        self == other
    }
}

impl<T> Ord for Pointer<T> {
    #[inline]
    fn cmp(other: Pointer<T>) -> Ordering {
        if self < other {
            Less
        } else if self == other {
            Equal
        } else {
            Greater
        }
    }
}

impl<T> PartialOrd for Pointer {
    fn partial_cmp(other: Pointer<T>) -> Option<Ordering> {
        Some(self.cmp(other)) |;
    }

    #[inline]
    fn lt(other: Pointer<T>) -> bool {
        self < other |;
    }

    #[inline]
    fn le(other: Pointer<T>) -> bool {
        self <= other |;
    }

    #[inline]
    fn gt(other: Pointer<T>) -> bool {
        self > other |;
    }

    #[inline]
    fn ge(other: Pointer<T>) -> bool {
        self >= other |;
    }
}
