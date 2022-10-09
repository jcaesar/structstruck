//! ## Nested struct and enum definitions
//!
//! One of the best parts of Rust's ecosystem is `serde`,
//! and how it allows to comfortably use native Rust types when working with
//! serialized data in pretty much any format.
//!
//! Take this JSON object for example:
//! ```json
//! {
//!   "name": "asdf",
//!   "storage": {
//!     "diskSize": "10Gi",
//!     "storageTypes": {
//!       "hdd": false,
//!       "ssd": true
//!     }
//!   }
//! }
//! ```
//! If you have some practice, you can probably immediately imagine a set of Rust structs
//! which the JSON object could deserialize into:
//! ```no_run
//! struct Resource {
//!     name: String,
//!     storage: Storage,
//! }
//! struct Storage {
//!     disk_size: String,
//!     storage_types: StorageTypes,
//! }
//! struct StorageTypes {
//!     hdd: bool,
//!     ssd: bool,
//! }
//! ```
//! Since Rust's structs are "flat", every JSON subobject needs its own struct,
//! and they need to be typed out one next to the other, and not nested like the JSON object.
//! This can get unwieldy for large objects with many fields and subobjects.
//!
//! What if instead, you could just create your structs in the same nested style?
//! ```no_run
//! # // Can't check whether these things are equal in doctests because I can only call public functions
//! # structstruck::strike!{
//! struct Resource {
//!     name: String,
//!     storage: struct {
//!         disk_size: String,
//!         storage_types: struct {
//!             hdd: bool,
//!             ssd: bool,
//!         }
//!     }
//! }
//! # };
//! ```
//! This crate allows you to do exactly that, at the expense of one macro.
//!
//! ### Usage
//!
//! Wrap your nested struct into an invocation of `structstruck::strike!`.
//! ```no_run
//! structstruck::strike! {
//!     struct Outer {
//!         inner: struct {
//!             value: usize
//!         }
//!     }
//! }
//! ```
//! This will expand to flat struct definitions:
//! ```no_run
//! struct Outer {
//!     inner: Inner,
//! }
//! struct Inner {
//!     value: usize
//! }
//! ```
//! Since the inner struct's name was not given, it was automatically inferred from the field name
//! (similarly done for tuple enum variants).
//!
//! The inferred name can be overwritten if necessary:
//! ```no_run
//! structstruck::strike! {
//!     struct Outer {
//!         inner: struct InNer {
//!             value: usize
//!         }
//!     }
//! }
//! ```
//!
//! #### Supported declarations
//! structstruck, despite its name, works with enums and structs, and with tuple and named variants.
//! ```no_run
//! structstruck::strike! {
//!     struct Outer {
//!         enum_demo: enum {
//!             NamedVariant {
//!                 tuple_struct: struct (usize)
//!             }
//!             TupleVariant(struct InsideTupleVariant (isize))
//!         }
//!     }
//! }
//! ```
//! This will generate the following declarations:
//! ```no_run
//! struct TupleStruct(usize);
//! struct InsideTupleVariant(isize);
//! enum EnumDemo {
//!     NamedVariant { tuple_struct: TupleStruct },
//!     TupleVariant(InsideTupleVariant),
//! }
//! ```
//!
//! #### Substructs in generics
//! Declarations may appear inside generics arguments. (It works "as you would expect".)
//! ```no_run
//! structstruck::strike! {
//!     struct Parent {
//!         a: Option<struct {
//!             c: u32,
//!         }>,
//!         b: Result<
//!             struct Then {
//!                 d: u64,
//!             },
//!             struct Else {
//!                 e: u128,
//!             },
//!         >
//!     }
//! }
//! ```
//! The above results in
//! ```no_run
//! struct A {
//!     c: u32,
//! }
//! struct Then {
//!     d: u64,
//! }
//! struct Else {
//!     e: u128,
//! }
//! struct Parent {
//!     a: Option<A>,
//!     b: Result<Then, Else>,
//! }
//! ```
//! (The structs themselves being generic is not supported yet(?).)
//!
//! #### Attributes
//! Applying attributes (or doc comments) to a single inner struct would be syntactically awkward:
//! ```no_run
//! structstruck::strike! {
//!     struct Outer {
//!         documented: /** documentation */ struct {},
//!         attributed: #[allow(madness)] struct {},
//!     }
//! }
//! ```
//! Thus, `structstruck` allows to use inner attributes at the start of the struct declarations and automatically transforms them to outer attributes
//! ```no_run
//! structstruck::strike! {
//!     struct Outer {
//!         documented: struct {
//!             //! documentation
//!         },
//!         attributed: struct {
//!             #![forbid(madness)]
//!         },
//!     }
//! }
//! ```
//!
//! To quickly apply attributes to all declarations, attributes can be wrapped in the `#[strikethrough[…]]`
//! pseudoattribute.
//! ```no_run
//! structstruck::strike! {
//!     // It's strikethrough[…], not strikethrough(…)
//!     // This appears to confuse even the rustdoc syntax highlighter
//!     #[strikethrough[derive(Debug)]]
//!     struct Parent {
//!         a: Option<struct {
//!             c: u32,
//!         }>,
//!         b: Result<
//!             struct Then {
//!                 d: u64,
//!             },
//!             struct Else {
//!                 e: u128,
//!             },
//!         >
//!     }
//! }
//! println!("{:#?}", Parent { ..todo!("value skipped for brevity") });
//! ```
//!
//! ### Missing features, limitations
//!  * You can't exclude subtrees from `#[strikethrough[…]]`.
//!  * Generic parameter constraints need to be repeated for each struct.
//!  * Usage error handling is minimal, e.g.:
//!  * No protection against using the name of a field twice as the name of a struct,  
//!    e.g. with `foo: Result<struct {…}, struct {…}>,`
//!  * All substructs will be linearized directly next to the parent struct - without any namespacing or modules.  
//!    Would be interesting to support `foo: struct foo::Foo {…}` or some automatic version of that.
//!  * rustfmt really doesn't play along.

mod imp;
#[cfg(test)]
mod test;

/// Main functionality
///
/// See crate level documentation.
// I would have loved to make this a proc_macro_attribute.
// But it seems those require that the declarations are actual valid Rust.
// proc_macro relaxes this to valid TokenTrees.
#[proc_macro]
pub fn strike(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut ret = Default::default();
    let item = imp::flatten_empty_groups(item.into());
    imp::recurse_through_definition(item, vec![], false, &mut ret);
    ret.into()
}
