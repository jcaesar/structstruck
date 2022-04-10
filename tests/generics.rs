#![allow(dead_code)]

structstruck::strike! {
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
}

#[test]
fn default_eq() {
    // The test here is that this compiles...
    let _ = Parent {
        a: None,
        b: Ok(Then { d: 42 }),
    };
}
