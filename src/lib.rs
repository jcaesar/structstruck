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
//! To quickly apply attributes to all declarations, attributes can be wrapped in the `#[structstruck::each[…]]`
//! pseudoattribute.
//! ```no_run
//! structstruck::strike! {
//!     // It's structstruck::each[…], not structstruck::each(…)
//!     // This appears to confuse even the rustdoc syntax highlighter
//!     #[structstruck::each[derive(Debug)]]
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
//! The behavior of `each` can be influenced in two ways:
//!  * `structstruck::exclude_each` will ignore any attributes in `each` for the current struct only.
//!  * `structstruck::clear_each` will ignore any `structstruck::each` from parent structs for the current struct and children.
//!
//! The order of attributes does not matter.
//!
//! For example:
//! ```no_run
//! structstruck::strike! {
//!     struct A {
//!         #![structstruck::each[deny(unused)]]
//!         b: struct {
//!             #![structstruck::each[allow(unused)]]
//!             #![structstruck::skip_each]
//!             #![structstruck::clear_each]
//!             c: struct {}
//!         }
//!     }
//! }
//! # const A: Option<A> = None;
//! # fn foo() { A.unwrap().b; }
//! ```
//! will place no attributes on `B` and only `allow(unused)` on `C`.
//!
//! #### Avoiding name collisions
//! If you want include the parent struct name (or parent enum name and variant name)
//! in the name of the child struct, add `#[structstruck::long_names]` to the struct.
//! ```no_run
//! structstruck::strike! {
//!     #[structstruck::long_names]
//!     struct Outer {
//!         inner: struct { value: usize }
//!     }
//! }
//! ```
//! This will generate the following declarations:
//! ```no_run
//! struct OuterInner {
//!   value: usize
//! }
//! struct Outer {
//!    inner: OuterInner,
//! }
//! ```
//! This can be combined with `structstruck::each` to use the full path of all ancestor struct names.
//! ```no_run
//! structstruck::strike! {
//!     #[structstruck::each[structstruck::long_names]]
//!     struct A {
//!         b: struct {
//!             c: struct { }
//!         }
//!     }
//! }
//! ```
//! will generate three structs, named `A`, `AB`, and `ABC`.
//!
//! This is useful to prevent collisions when using the same field name multiple times or a type with the same name as a field exists.
//!
//! ### Missing features, limitations
//!  * Generic parameter constraints need to be repeated for each struct.
//!  * Usage error handling is minimal, e.g.:
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
