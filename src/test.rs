use crate::imp::{recurse_through_definition, type_tree, TypeTree};
use proc_macro2::{Delimiter, Group, TokenStream, TokenTree};
use quote::quote;

fn pretty(plan: proc_macro2::TokenStream) -> String {
    let planstr = plan.to_string();
    let plan = &syn::parse_file(&planstr);
    let plan = match plan {
        Ok(plan) => plan,
        Err(_) => return planstr,
    };
    prettyplease::unparse(plan)
}

fn check(nested: proc_macro2::TokenStream, planexpected: proc_macro2::TokenStream) {
    let mut plan = proc_macro2::TokenStream::new();
    recurse_through_definition(nested, vec![], false, &mut plan);
    // No Eq implementations. :/
    let plan = pretty(plan);
    let planexpected = pretty(planexpected);
    assert!(
        plan == planexpected,
        "\n  left: {}\n right: {}",
        plan,
        planexpected
    );
}

#[test]
fn strikethrough_derive() {
    let from = quote! {
        #[strikethrough[striked_attr]]
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
        #[striked_attr]
        #[derive(Debug, Default, PartialEq)]
        struct Shared {
            d: i32
        }
        #[striked_attr]
        #[derive(Debug, Default, PartialEq)]
        #[gobbel]
        struct A {
            b: Shared,
            c: Shared,
        }
        #[striked_attr]
        #[derive(Debug, Default, PartialEq)]
        #[gubbel]
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

/// If the field is pub, its type must also be
///
/// (But only if it's fully pub. pub(crate) e.g. doesn't require anything.)
#[test]
fn implicit_pub() {
    let from = quote! {
        struct Parent {
            pub pub_to_none: struct {},
            pub(crate) pub_crate_ign: struct {},
            pub no_overwrite: pub(crate) struct {},
        }
    };
    let out = quote! {
        pub struct PubToNone {}
        struct PubCrateIgn {}
        pub(crate) struct NoOverwrite {}
        struct Parent {
            pub pub_to_none: PubToNone,
            pub(crate) pub_crate_ign: PubCrateIgn,
            pub no_overwrite: NoOverwrite,
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
            b: Result<Then, Else, >
        }
    };
    check(from, out);
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
            Foo { b: i8 }
        }
        enum Parent {
            A { a: A, c: i16 },
            B {}
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
        struct A (i16);
        struct Bar { bar: i64 }
        struct B (Bar);
        struct Foo (i32);
        enum C { Foo (Foo) }
        struct Parent { a : A , b : B , c : C }
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
        struct TupleStruct (usize);
        struct TupleVariant (isize);
        enum EnumDemo {
            NamedVariant { tuple_struct : TupleStruct } ,
            TupleVariant (TupleVariant)
        }
        struct Outer { enum_demo : EnumDemo }
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
        struct A(Option<Result<B, Option<C> > > );
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
    recurse_through_definition(plain, vec![], false, &mut proc_macro2::TokenStream::new());
}

#[test]
fn raw_identifier_as_name() {
    let from = quote! {
        struct A { r#type: struct ()  };
    };
    let out = quote! {
        struct Type();
        struct A { r#type: Type }
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
        struct Unnamed < T > { t : T }
        struct Named < T > { t : T }
        struct Outer { unnamed : Unnamed <T> , whatev : Named <T> , }
    };
    check(from, out);
}

#[test]
fn pub_enum() {
    let from = quote! {
        enum Opts {
            Login(pub struct {
                hs: Url,
            }),
            Run(pub struct {
                channel: Option<RoomId>,
            }),
        }
    };
    let out = quote! {
        pub struct Login {
            hs: Url,
        }

        pub struct Run {
            channel: Option<RoomId>,
        }

        enum Opts {
            Login(Login),
            Run(Run),
        }
    };
    check(from, out);
}

#[test]
fn tuple_pub_struct() {
    let from = quote! {
        struct Foo(pub struct Bar(u32));
    };
    let out = quote! {
        pub struct Bar(u32);
        struct Foo(Bar);
    };
    check(from, out);
}

#[test]
fn pub_tuple_pub_struct() {
    let from = quote! {
        struct Foo(pub pub struct Bar(u32));
    };
    let out = quote! {
        pub struct Bar(u32);
        struct Foo(pub Bar);
    };
    check(from, out);
}

#[test]
fn public_enum() {
    let from = quote! {
        enum Outer {
            Struct(pub pub struct { a: Zing }),
            Enum(pub pub enum { A, B, C }),
        };
    };
    let out = quote! {
        pub struct Struct { a : Zing }
        pub enum Enum { A , B , C }
        enum Outer {
            Struct (pub Struct) ,
            Enum (pub Enum) ,
        }
    };
    check(from, out);
}

#[test]
fn inner_comment() {
    // Doc comments just desugar to #[doc = r"…"], but whatev, I'll test both.
    let from = quote! {
        struct Struck {
            //! Foo
            #![bar]
            blubb: i32
        };
    };
    let out = quote! {
        /// Foo
        #[bar]
        struct Struck { blubb: i32 }
    };
    check(from, out);
}

#[test]
fn inner_comment_as_in_doc() {
    let from = quote! {
        struct Outer {
            documented: struct {
                //! documentation
            },
            attributed: struct {
                #![attribute]
            },
        }
    };
    let out = quote! {
        struct Outer {
            documented: /** documentation*/ struct {},
            attributed: #[attribute] struct {},
        }
    };
    let mut rout = Default::default();
    recurse_through_definition(out, vec![], false, &mut rout);
    check(from, rout);
}

#[test]
fn strikethrough_weird() {
    let out = quote! {
        #[strikethrough = foo]
        struct struct { }
    };
    let mut rout = Default::default();
    recurse_through_definition(out, vec![], false, &mut rout);
    assert!(rout
        .into_iter()
        .any(|t| matches!(t, TokenTree::Ident(kw) if kw == "compile_error")));
}

#[test]
fn pub_markers_sane() {
    use crate::imp::*;
    assert!(is_plain_pub(&Some(make_pub_marker())))
}

#[test]
fn issue_2() {
    let from = quote! {
        enum Expr<'src> {
            Binary(struct<'src> {
                 left: Box<Expr<'src>>,
                 operator: BinaryOp,
                 right: Box<Expr<'src>>,
            }),
            Literal(enum<'src> {
                StringLit(&'src str),
                NumLit(&'src str),
            }),
        }
    };
    let out = quote! {
        struct Binary < 'src > { left : Box < Expr < 'src >> , operator : BinaryOp , right : Box < Expr < 'src >> , }
        enum Literal < 'src > { StringLit (& 'src str) , NumLit (& 'src str) , }
        enum Expr < 'src > { Binary (Binary<'src>) , Literal (Literal<'src>) , }
    };
    check(from, out)
}

#[test]
fn pub_enum_autopubs() {
    let from = quote! {
        pub enum Outer {
            An(struct ()),
            Ny(struct {}),
        };
    };
    let out = quote! {
        pub struct An ();
        pub struct Ny {}
        pub enum Outer {
            An(An),
            Ny(Ny),
        }
    };
    check(from, out);
}

#[test]
fn missing_comma_issue4() {
    let from = quote! {
        struct Incorrect {
            eater: struct {
                stomach: (),
            } // notice the missing comma
            eaten: struct {
                apple: bool,
            }
        }
    };
    let mut to = TokenStream::new();
    recurse_through_definition(from, vec![], false, &mut to);
    assert!(to.clone().into_iter().any(|tok| match tok {
        TokenTree::Ident(id) => id == "compile_error",
        _ => false,
    }));
}

#[test]
/// TODO
fn not_quite_fixed_issue4() {
    let from = quote! {
        struct Incorrect {
            eater: struct {
                stomach: (),
            } // notice the missing comma
            you can still put arbitrary junk here and venial will ignore it ;(
            ! ) 42
        }
    };
    let out = quote! {
        struct Eater {
            stomach: (),
        }
        struct Incorrect {
            eater: Eater
        }
    };
    check(from, out);
}

#[test]
fn issue4_variant() {
    let from = quote! {
        struct Incorrect {
            uff: Result<struct {} struct {}>
        }
    };
    let mut to = TokenStream::new();
    recurse_through_definition(from, vec![], false, &mut to);
    assert!(to.clone().into_iter().any(|tok| match tok {
        TokenTree::Ident(id) => id == "compile_error",
        _ => false,
    }));
}

#[test]
fn issue5_unions() {
    let from = quote! {
        struct x_thing {
            a: union {
                value: u32,
                b: struct {
                    thing_a: TypeA,
                    thing_b: TypeB,
                },
            },
            some_data: [char; 123],
        }
    };
    let out = quote! {
        struct B {
            thing_a: TypeA,
            thing_b: TypeB,
        }
        union A {
            value: u32,
            b: B,
        }
        struct x_thing {
            a: A,
            some_data: [char; 123],
        }
    };
    check(from, out);
}

#[test]
fn typedef() {
    let from = quote! {
        struct Thing {
            foo: type = u32,
        }
    };
    let out = quote! {
        type Foo = u32;
        struct Thing {
            foo: Foo,
        }
    };
    check(from, out);
}

#[test]
fn issue6_path() {
    let from = quote! {
        struct ItemDefine {
            semantic_token: keywords::semantic,
            ident: Ident,
            semantic_fields: struct {
                brace_token: token::Brace,
                fields: Vec<SemanticField>,
            }
        }
    };
    let out = quote! {
        struct SemanticFields {
            brace_token: token::Brace,
            fields: Vec<SemanticField>,
        }
        struct ItemDefine {
            semantic_token: keywords::semantic,
            ident: Ident,
            semantic_fields: SemanticFields,
        }
    };
    check(from, out);
}
