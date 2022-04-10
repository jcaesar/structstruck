use crate::imp::recurse_through_definition;
use proc_macro2::TokenStream;
use quote::quote;

#[test]
fn strikethrough_derive() {
    let from = quote! {
        #[strikethrough[derive(Debug, Default, PartialEq)]]
        struct Parent {
            a: struct {
                b: struct Shared { d: i32 },
                c: Shared,
            },
            e: u32,
        }
    };
    let mut to = TokenStream::new();
    let out = quote! {
        #[derive(Debug, Default, PartialEq)]
        struct Shared {
            d: i32,
        }
        #[derive(Debug, Default, PartialEq)]
        struct A {
            b: Shared,
            c: Shared,
        }
        #[derive(Debug, Default, PartialEq)]
        struct Parent {
            a: A,
            e: u32,
        }
    };
    recurse_through_definition(from, vec![], &mut to);
    // No Eq implementations. :/
    assert_eq!(to.to_string(), out.to_string());
}

#[test]
fn explicit_pub() {
    let from = quote! {
        struct Parent {
            a: pub struct {
                c: u32,
            },
            b: pub(crate) struct {
                d: u64,
            },
        }
    };
    let mut to = TokenStream::new();
    let out = quote! {
        pub struct A {
            c: u32,
        }
        pub(crate) struct B {
            d: u64,
        }
        struct Parent {
            a: A,
            b: B,
        }
    };
    recurse_through_definition(from, vec![], &mut to);
    assert_eq!(to.to_string(), out.to_string());
}

#[test]
fn in_generics() {
    let from = quote! {
        struct Parent {
            a: Option<struct {
                c: u32,
            }>,
            b: Result<
                struct Then {
                    d: u64,
                },
                struct Else {
                    e: u128,
                },
            >
        }
    };
    let mut to = TokenStream::new();
    let out = quote! {
        struct A {
            c: u32,
        }
        struct Then {
            d: u64,
        }
        struct Else {
            e: u128,
        }
        struct Parent {
            a: Option<A>,
            b: Result<Then, Else, >,
        }
    };
    recurse_through_definition(from, vec![], &mut to);
    assert_eq!(to.to_string(), out.to_string());
}
