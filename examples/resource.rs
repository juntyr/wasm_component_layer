use anyhow::*;
use wasm_component_layer::*;

// The bytes of the component.
const WASM: &[u8] = include_bytes!("resource/component.wasm");

bindgen!({
    path: "examples/resource/wit",
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

    // Add the host-defined exports to the linker
    Guest::add_to_linker::<_, Host>(&mut store, &mut linker).unwrap();

    // Create an instance of the component using the linker.
    let (instance, _instance) = Guest::instantiate(&mut store, &component, &linker).unwrap();

    // // Get the interface that the interface exports.
    // let interface = instance
    //     .exports()
    //     .instance(&"test:guest/foo".try_into().unwrap())
    //     .unwrap();

    // // Get the function for creating and using a resource.
    // let use_resource = interface
    //     .func("use-resource")
    //     .unwrap()
    //     .typed::<(), ()>()
    //     .unwrap();

    // // Prints 'Called print with value MyResource(42)'
    // use_resource.call(&mut store, ()).unwrap();
}

enum Host {}

impl test::guest::bar::Host for Host {
    type HostResource = MyResource;
}

#[derive(Debug)]
pub struct MyResource(pub i32);

impl test::guest::bar::HostHostResource for MyResource {
    fn new(
        ctx: impl AsContextMut,
        a: i32,
    ) -> anyhow::Result<TypedResourceOwn<test::guest::bar::HostHostResourceResource<Self>>> {
        TypedResourceOwn::new(ctx, Self(a))
    }

    fn print_a(
        ctx: impl AsContextMut,
        self_: TypedResourceBorrow<test::guest::bar::HostHostResourceResource<Self>>,
    ) -> anyhow::Result<()> {
        println!("Host: {}", self_.rep(&ctx.as_context())?.0);
        Ok(())
    }
}
