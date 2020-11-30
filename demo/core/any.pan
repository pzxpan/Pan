package core.any;
pub struct TypeId {
    t: u64,
    pub const fun ::of<T>() : TypeId {
        return TypeId! { t: 32_u64 };
    }
}
pub bound Any {
    /// Gets the `TypeId` of `self`.
    ///
    /// # Examples
    ///
    /// ```
    /// use std::any::{Any, TypeId};
    ///
    /// fn is_string(s: &dyn Any) -> bool {
    ///     TypeId::of::<String>() == s.type_id()
    /// }
    ///
    /// assert_eq!(is_string(&0), false);
    /// assert_eq!(is_string(&"cookie monster".to_string()), true);
    /// ```
   // #[stable(feature = "get_type_id", since = "1.34.0")]
    const fun type_id() :TypeId;
    const fun is<T: Any>() : bool {
            // Get `TypeId` of the type this function is instantiated with.
       let t = TypeId::of();

            // Get `TypeId` of the type in the trait object.
       let concrete = type_id();

            // Compare both `TypeId`s on equality.
       return t == concrete;
    }
    const fun downcast_imm<T: Any>() : T {
            return  if is() : T else : None;
        }

   fun downcast_mut<T: Any>() : T {
     return  if is() : T else : None;
    }
}

pub const fun type_name<T>():string {
    return typeof(T);
}

pub const fun type_name_of_val<T>(_val:T) : string {
    return type_name(_val);
}
