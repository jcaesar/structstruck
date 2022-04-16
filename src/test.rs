use crate::imp::{recurse_through_definition, type_tree, TypeTree};
use proc_macro2::{Delimiter, Group, TokenStream, TokenTree};
use quote::quote;

fn check(plain: proc_macro2::TokenStream, nested: proc_macro2::TokenStream) {
    let mut to = proc_macro2::TokenStream::new();
    recurse_through_definition(plain, vec![], &mut to);
    // No Eq implementations. :/
    assert_eq!(to.to_string(), nested.to_string());
}

#[test]
fn strikethrough_derive() {
    let from = quote! {
        #[strikethrough[derive(Debug, Default, PartialEq)]]
        #[gubbel]
        struct Parent {
            a: #[gobbel] struct {
                b: struct Shared { d: i32 },
                c: Shared,
            },
            e: u32,
        }
    };
    let out = quote! {
        #[derive(Debug, Default, PartialEq)]
        struct Shared {
            d: i32,
        }
        #[gobbel]
        #[derive(Debug, Default, PartialEq)]
        struct A {
            b: Shared,
            c: Shared,
        }
        #[gubbel]
        #[derive(Debug, Default, PartialEq)]
        struct Parent {
            a: A,
            e: u32,
        }
    };
    check(from, out);
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
    check(from, out);
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
    check(from, out);
}

#[test]
fn unsupported_union() {
    let from = quote! {
        union Foo { }
    };
    let mut to = TokenStream::new();
    recurse_through_definition(from, vec![], &mut to);
    assert!(to.clone().into_iter().any(|tok| match tok {
        TokenTree::Ident(id) => id == "compile_error",
        _ => false,
    }));
    //assert!(to.clone().into_iter().any(|tok| match tok {
    //    TokenTree::Literal(lit) => lit.to_string().contains("unsupported"),
    //    _ => false,
    //}));
}

#[test]
fn enum_named() {
    let from = quote! {
        enum Parent {
            A {
                a: enum { Foo { b: i8 } },
                c: i16
            }
            B {}
        }
    };
    let out = quote! {
        enum A {
            Foo { b: i8, },
        }
        enum Parent {
            A { a: A, c: i16, },
            B {},
        }
    };
    check(from, out);
}

#[test]
fn tupledec() {
    let from = quote! {
        struct Parent {
            a: struct (i16),
            b: struct (struct Bar { bar: i64 }),
            c: enum { Foo(struct(i32))}
        }
    };
    let out = quote! {
        struct A (i16, );
        struct Bar { bar: i64,  }
        struct B (Bar ,);
        struct Foo (i32 ,);
        enum C { Foo (Foo ,) , }
        struct Parent { a : A , b : B , c : C , }
    };
    check(from, out);
}

#[test]
fn tuples_need_semicolon_bug() {
    let from = quote! {
        struct Outer {
            enum_demo: enum {
                NamedVariant {
                    tuple_struct: struct (usize)
                }
                TupleVariant(struct (isize))
            }
        }
    };
    let out = quote! {
        struct TupleStruct (usize ,);
        struct TupleVariant (isize ,);
        enum EnumDemo {
            NamedVariant { tuple_struct : TupleStruct , } ,
            TupleVariant (TupleVariant ,) ,
        }
        struct Outer { enum_demo : EnumDemo , }
    };
    check(from, out);
}

#[test]
fn double_generics_bug() {
    let from = quote! {
        pub struct EventSourceSpec {
            pub kafka: Option<HashMap<String,
                pub struct KafkaSourceSpec {
                    pub url: String,
                }
            > >,
        }
    };
    let out = quote! {
        pub struct KafkaSourceSpec { pub url : String , }
        pub struct EventSourceSpec { pub kafka : Option < HashMap < String , KafkaSourceSpec > > , }
    };
    check(from, out);
}

#[test]
fn triple_generics() {
    let from = quote! {
        struct A (
            Option<Result<struct B(), Option<struct C()> > >
        );
    };
    let out = quote! {
        struct B();
        struct C();
        struct A(Option<Result<B, Option<C> > >, );
    };
    check(from, out);
}

#[test]
fn raw_identifier_panic_bug() {
    let plain = quote! {
        struct A {
            r#type: () // This was actually enough for a segfault
        };
    };
    recurse_through_definition(plain, vec![], &mut proc_macro2::TokenStream::new());
}

#[test]
fn raw_identifier_as_name() {
    let from = quote! {
        struct A { r#type: struct ()  };
    };
    let out = quote! {
        struct Type();
        struct A { r#type: Type, }
    };
    check(from, out);
}

#[test]
fn type_tree_parsing() {
    let inp = quote! {
        Hoge < Huge < Hage, Hegge >, struckt Hacke<'a,U> where P: E<> {} >
    };
    let expect = quote! {
        Hoge ( Huge ( Hage, Hegge ), struckt Hacke('a,U) where P: E() {} )
    };
    let inp = inp.into_iter().collect::<Vec<_>>();
    let mut out = proc_macro2::TokenStream::new();
    let res = type_tree(&inp, &mut out);
    fn tost(inp: Vec<TypeTree>) -> TokenStream {
        inp.into_iter()
            .map(|t| match t {
                TypeTree::Group(_, s, _) => {
                    TokenTree::Group(Group::new(Delimiter::Parenthesis, tost(s)))
                }
                TypeTree::Token(t) => t.clone(),
            })
            .collect()
    }
    println!("{}", out);
    assert_eq!(tost(res).to_string(), expect.to_string());
    assert!(out.is_empty());
}

#[test]
fn generics_on_def() {
    let from = quote! {
        struct Outer {
            unnamed: struct<T>{t: T},
            whatev: struct Named<T>{t: T},
        };
    };
    let out = quote! {
        struct Unnamed < T > { t : T , }
        struct Named < T > { t : T , }
        struct Outer { unnamed : Unnamed , whatev : Named , }
    };
    check(from, out);
}
