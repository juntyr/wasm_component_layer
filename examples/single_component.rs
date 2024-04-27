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
    let mut linker = Linker::default();
    // Create an instance of the component using the linker.
    let (instance, _instance) = Guest::instantiate(&mut store, &component, &mut linker).unwrap();

    // Get the interface that the interface exports.
    let interface = instance.test_guest_foo();

    // Create an example list to test upon.
    let example = ["a", "b", "c"]
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>();

    println!(
        "Calling select-nth({example:?}, 1) == {}",
        interface.call_select_nth(&mut store, example.clone(), 1).unwrap()
    );
    // Prints 'Calling select-nth(["a", "b", "c"], 1) == b'
}
