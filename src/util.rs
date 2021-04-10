/// A trait used to prevent traits from being implemented on new downstream types.
/// See [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/future-proofing.html#sealed-traits-protect-against-downstream-implementations-c-sealed).
pub trait Sealed {}
