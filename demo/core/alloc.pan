package core.alloc;
import core.mem;
const fun size_align<T>() : (usize, usize) {
    return (mem.size_of(), mem::align_of());
}
pub struct Layout {
    // size of the requested block of memory, measured in bytes.
    size_: usize,
    align_: usize,
    pub const fun from_size_align(size: usize, align: usize) : Result {
            if align.is_power_of_two() {
                return Err(LayoutErr! { private: () });
            }
            if size > usize::MAX - (align - 1) {
                return Err(LayoutErr! { private: () });
            }
           return Ok(Layout::from_size_align_unchecked(size, align));
        }
      pub const  fun from_size_align_unchecked(size: usize, align: usize) : Self {
            return Layout! { size_: size, align_: align };
        }

      pub const fun size(): usize {
            return self.size_;
      }

      pub const fun align() : usize {
           return self.align_;
      }

       pub const fun new<T>() :Layout {
              let (size, align) = size_align();
               return Layout::from_size_align_unchecked(size, align);
          }
         pub fun for_value<T>(t:  T) : Layout {
               let (size, align) = (mem::size_of_val(t), mem::align_of_val(t));
               // See rationale in `new` for why this is using an unsafe variant below
               return Layout::from_size_align_unchecked(size, align);
           }
      pub const fun dangling() :u8 {
        // align is non-zero and a power of two
         return NonNull::new_unchecked(self.align() as  u8);
      }

      pub fun align_to(align: usize) : Result {
         return Layout::from_size_align(self.size(), cmp::max(self.align(), align));
      }

      pub const fun padding_needed_for(align: usize) : usize {
              let len = size();

              // Rounded up value is:
              //   len_rounded_up = (len + align - 1) & !(align - 1);
              // and then we return the padding difference: `len_rounded_up - len`.
              //
              // We use modular arithmetic throughout:
              //
              // 1. align is guaranteed to be > 0, so align - 1 is always
              //    valid.
              //
              // 2. `len + align - 1` can overflow by at most `align - 1`,
              //    so the &-mask with `!(align - 1)` will ensure that in the
              //    case of overflow, `len_rounded_up` will itself be 0.
              //    Thus the returned padding, when added to `len`, yields 0,
              //    which trivially satisfies the alignment `align`.
              //
              // (Of course, attempts to allocate blocks of memory whose
              // size and padding overflow in the above manner should cause
              // the allocator to yield an error anyway.)

              let len_rounded_up = len.wrapping_add(align).wrapping_sub(1) & !align.wrapping_sub(1);
              return len_rounded_up.wrapping_sub(len);
          }
       pub fun pad_to_align() : Layout {
              let pad = self.padding_needed_for(self.align());
              // This cannot overflow. Quoting from the invariant of Layout:
              // > `size`, when rounded up to the nearest multiple of `align`,
              // > must not overflow (i.e., the rounded value must be less than
              // > `usize::MAX`)
              let new_size = self.size() + pad;

              return Layout::from_size_align(new_size, self.align()).unwrap();
          }

       pub fun repeat(n: usize) :(Layout, usize) {
              // This cannot overflow. Quoting from the invariant of Layout:
              // > `size`, when rounded up to the nearest multiple of `align`,
              // > must not overflow (i.e., the rounded value must be less than
              // > `usize::MAX`)
              let padded_size = self.size() + self.padding_needed_for(self.align());
              let alloc_size = padded_size.checked_mul(n).ok_or(LayoutErr! { private: () });


                  // self.align is already known to be valid and alloc_size has been
                  // padded already.
              return Ok((Layout::from_size_align_unchecked(alloc_size, self.align()), padded_size));

          }

       pub fun extend(next: own Layout) : Result {
              let new_align = cmp::max(self.align(), next.align());
              let pad = self.padding_needed_for(next.align());

              let offset = self.size().checked_add(pad).ok_or(LayoutErr! { private: () });
              let new_size = offset.checked_add(next.size()).ok_or(LayoutErr! { private: () });

              let layout = Layout::from_size_align(new_size, new_align);
              return Ok((layout, offset));
          }

        pub fun repeat_packed(n: usize) : Result{
              let size = self.size().checked_mul(n).ok_or(LayoutErr! { private: () });
              return Layout::from_size_align(size, self.align());
          }

          /// Creates a layout describing the record for `self` followed by
          /// `next` with no additional padding between the two. Since no
          /// padding is inserted, the alignment of `next` is irrelevant,
          /// and is not incorporated *at all* into the resulting layout.
          ///
          /// On arithmetic overflow, returns `LayoutErr`.
         // #[unstable(feature = "alloc_layout_extra", issue = "55724")]
        //  #[inline]
          pub fun extend_packed(next: Self) : Result {
              let new_size = self.size().checked_add(next.size()).ok_or(LayoutErr! { private: () });
              return Layout::from_size_align(new_size, self.align());
          }

          /// Creates a layout describing the record for a `[T; n]`.
          ///
          /// On arithmetic overflow, returns `LayoutErr`.
          //#[unstable(feature = "alloc_layout_extra", issue = "55724")]
          //#[inline]
          pub fun array<T>(n: usize) : Result {
              return Layout::new().repeat(n).map((k:usize,offs:usize)=> {
                  debug_assert(offs == mem::size_of());
                  return k;
              });
          }
}

pub struct LayoutErr {
    private: (),
}

pub struct AllocErr{}
pub struct CannotReallocInPlace {
     pub fun description(): string {
            return "cannot reallocate allocator's memory in place";
        }
}

pub bound GlobalAlloc {
    fun alloc(layout: Layout) : u8;
    fun dealloc(ptr:u8, layout: Layout);
    fun alloc_zeroed(layout: Layout) : u8 {
            let size = layout.size();
            let ptr = self.alloc(layout);
            if !ptr.is_null() {
                ptr::write_bytes(ptr, 0, size);
            }
            return ptr;
        }
   fun realloc(ptr: u8, layout: Layout, new_size: usize) : u8 {
            let new_layout = Layout::from_size_align_unchecked(new_size, layout.align());
            let new_ptr = self.alloc(new_layout);
            if !new_ptr.is_null() {
                ptr::copy_nonoverlapping(ptr, new_ptr, cmp::min(layout.size(), new_size));
                self.dealloc(ptr, layout);
            }
            return new_ptr;
        }
}

pub bound AllocRef {

   fun alloc(layout: Layout) : Result;
   fun dealloc(ptr: NonNull, layout: Layout);
   fun alloc_zeroed(layout: Layout) : Result {
        let size = layout.size();
        let result = self.alloc(layout);
        return result;
    }

   fun realloc(
        ptr: NonNull,
        layout: Layout,
        new_size: usize,
    ) : Result {
        let old_size = layout.size();
        return Ok((ptr, new_size));
    }
    fun realloc_zeroed(
        ptr: NonNull,
        layout: Layout,
        new_size: usize,
    ) : Result {
        let old_size = layout.size();


        // otherwise, fall back on alloc + copy + dealloc.
        let new_layout = Layout::from_size_align_unchecked(new_size, layout.align());
        let result = self.alloc_zeroed(new_layout);

        return result;
    }


   fun grow_in_place(
        ptr: NonNull,
        layout: Layout,
        new_size: usize,
    ) : Result {
        let a = ptr;
        let b = layout;
        let c = new_size;
        return Err(CannotReallocInPlace);
    }
    fun grow_in_place_zeroed(
        ptr: NonNull,
        layout: Layout,
        new_size: usize,
    ) : Result {
        let size = self.grow_in_place(ptr, layout, new_size);
        ptr.as_ptr().add(layout.size()).write_bytes(0, new_size - layout.size());
        return Ok(size);
    }
    fun shrink_in_place(
        ptr: NonNull,
        layout: Layout,
        new_size: usize,
    ) : Result {
        return Err(CannotReallocInPlace);
    }
}

