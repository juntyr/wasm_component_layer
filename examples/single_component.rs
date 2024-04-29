use wasm_component_layer::*;

// The bytes of the component.
const WASM: &[u8] = include_bytes!("single_component/component.wasm");

bindgen!({
    path: "examples/single_component/wit",
});

pub fn main() {
    // Create a new engine for instantiating a component.
    let engine = Engine::new(wasmi::Engine::default());

    // Create a store for managing WASM data and any custom user-defined state.
    let mut store = Store::new(&engine, ());

    // Parse the component bytes and load its imports and exports.
    let component = Component::new(&engine, WASM).unwrap();
    // Create a linker that will be used to resolve the component's imports, if any.
    let linker = Linker::default();
    // Create an instance of the component using the linker.
    let (instance, _instance) = Guest::instantiate(&mut store, &component, &linker).unwrap();

    // Get the interface that the interface exports.
    let interface = instance.test_guest_foo();

    // Create an example list to test upon.
    let example = ["a", "b", "c"]
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>();

    println!(
        "Calling select-nth({example:?}, 1) == {:?}",
        interface
            .call_select_nth(&mut store, example.clone(), 1)
            .unwrap()
    );
    // Prints 'Calling select-nth(["a", "b", "c"], 1) == "b"'

    let flags = exports::test::guest::foo::MyFlags::OptB;
    println!(
        "Calling select-other-flags({flags:?}) == {:?}",
        interface
            .call_select_other_flags(&mut store, flags)
            .unwrap()
    );
    // Prints 'Calling select-other-flags((OptB)) == (OptA|OptC)'

    let case = exports::test::guest::foo::MyEnum::CaseC;
    println!(
        "Calling debug-enum-case({case:?}) == {:?}",
        interface.call_debug_enum_case(&mut store, case).unwrap()
    );
    // Prints 'Calling debug-enum-case(MyEnum::CaseC) == "MyEnum::CaseC"'

    let bytes = vec![1_u8, 2, 3];
    println!(
        "Calling pack-bytes-into-variant({bytes:?}) == {:?}",
        interface
            .call_pack_bytes_into_variant(&mut store, bytes.clone())
            .unwrap()
    );
    // Prints 'Calling pack-bytes-into-variant([1, 2, 3]) == MyVariant::List([1, 2, 3])'

    let record = exports::test::guest::foo::MyRecord {
        a: true,
        b: exports::test::guest::foo::MyFlags::OptC,
        c: exports::test::guest::foo::MyEnum::CaseA,
    };
    println!(
        "Calling flip-record-fields({record:?}) == {:?}",
        interface
            .call_flip_record_fields(&mut store, record)
            .unwrap()
    );
    // Prints 'Calling flip-record-fields(MyRecord { a: true, b: (OptC), c: MyEnum::CaseA }) == MyRecord { a: false, b: (OptA|OptB), c: MyEnum::CaseB }'
}
