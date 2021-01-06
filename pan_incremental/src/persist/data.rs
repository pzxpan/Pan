//! The data that we will serialize and deserialize.

use pan_macros::{Decodable, Encodable};
use pan_middle::dep_graph::{WorkProduct, WorkProductId};

#[derive(Debug, Encodable, Decodable)]
pub struct SerializedWorkProduct {
    /// node that produced the work-product
    pub id: WorkProductId,

    /// work-product data itself
    pub work_product: WorkProduct,
}
