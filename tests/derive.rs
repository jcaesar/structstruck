structstruck::strike! {
    #[strikethrough[derive(Debug, Default, PartialEq)]]
    struct Parent {
        a: struct {
            b: struct Shared { d: i32 },
            c: Shared,
        },
        e: u32,
    }
}

#[test]
fn default_eq() {
    assert_eq!(
        Parent::default(),
        Parent {
            a: A {
                b: Shared { d: 0 },
                c: Shared { d: 0 },
            },
            e: 0,
        }
    );
}
