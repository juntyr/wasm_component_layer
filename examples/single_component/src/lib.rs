wit_bindgen::generate!({
    path: "wit/component.wit",
    exports: {
        "test:guest/foo": Foo
    }
});

struct Foo;

impl exports::test::guest::foo::Guest for Foo {
    fn select_nth(x: Vec<String>, i: u32) -> String {
        x.into_iter().nth(i as usize).expect("Could not get value.")
    }

    fn select_other_flags(f: exports::test::guest::foo::MyFlags) -> exports::test::guest::foo::MyFlags {
        !f
    }

    fn debug_enum_case(e: exports::test::guest::foo::MyEnum) -> String {
        format!("{e:?}")
    }

    fn pack_bytes_into_variant(b: Vec<u8>) -> exports::test::guest::foo::MyVariant {
        exports::test::guest::foo::MyVariant::List(b)
    }

    fn flip_record_fields(r: exports::test::guest::foo::MyRecord) -> exports::test::guest::foo::MyRecord {
        exports::test::guest::foo::MyRecord {
            a: !r.a,
            b: !r.b,
            c: match r.c {
                exports::test::guest::foo::MyEnum::CaseA => exports::test::guest::foo::MyEnum::CaseB,
                exports::test::guest::foo::MyEnum::CaseB => exports::test::guest::foo::MyEnum::CaseC,
                exports::test::guest::foo::MyEnum::CaseC => exports::test::guest::foo::MyEnum::CaseA,
            },
        }
    }
}
