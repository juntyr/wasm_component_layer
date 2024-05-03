use crate::rust::{to_rust_ident, to_rust_upper_camel_case, RustGenerator, TypeMode};
use crate::types::{TypeInfo, Types};
use anyhow::{bail, Context};
use heck::*;
use indexmap::{IndexMap, IndexSet};
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt::Write as _;
use std::io::{Read, Write};
use std::mem;
use std::process::{Command, Stdio};
use wit_parser::*;

macro_rules! uwrite {
    ($dst:expr, $($arg:tt)*) => {
        write!($dst, $($arg)*).unwrap()
    };
}

macro_rules! uwriteln {
    ($dst:expr, $($arg:tt)*) => {
        writeln!($dst, $($arg)*).unwrap()
    };
}

mod rust;
mod source;
mod types;
use source::Source;

#[derive(Clone)]
enum InterfaceName {
    /// This interface was remapped using `with` to some other Rust code.
    Remapped {
        name_at_root: String,
        local_path: Vec<String>,
    },
    /// This interface is generated in the module hierarchy specified.
    Path(Vec<String>),
}

#[derive(Default)]
struct Bindgen {
    src: Source,
    opts: Opts,
    import_interfaces: Vec<(String, InterfaceName)>,
    import_functions: Vec<ImportFunction>,
    exports: Exports,
    types: Types,
    sizes: SizeAlign,
    interface_names: HashMap<InterfaceId, InterfaceName>,
    interface_last_seen_as_import: HashMap<InterfaceId, bool>,
    trappable_errors: IndexMap<TypeId, String>,
    // Track the with options that were used. Remapped interfaces provided via `with`
    // are required to be used.
    used_with_opts: HashSet<String>,
}

struct ImportFunction {
    add_to_linker: String,
    sig: String,
}

#[derive(Default)]
struct Exports {
    fields: BTreeMap<String, (String, String)>,
    modules: Vec<(String, InterfaceName)>,
    funcs: Vec<String>,
}

#[derive(Default, Debug, Clone, Copy)]
pub enum Ownership {
    /// Generated types will be composed entirely of owning fields, regardless
    /// of whether they are used as parameters to guest exports or not.
    #[default]
    Owning,

    /// Generated types used as parameters to guest exports will be "deeply
    /// borrowing", i.e. contain references rather than owned values when
    /// applicable.
    Borrowing {
        /// Whether or not to generate "duplicate" type definitions for a single
        /// WIT type if necessary, for example if it's used as both an import
        /// and an export, or if it's used both as a parameter to an export and
        /// a return value from an export.
        duplicate_if_necessary: bool,
    },
}

#[derive(Default, Debug, Clone)]
pub struct Opts {
    /// Whether or not `rustfmt` is executed to format generated code.
    pub rustfmt: bool,

    /// Whether or not to emit `tracing` macro calls on function entry/exit.
    pub tracing: bool,

    /// A list of "trappable errors" which are used to replace the `E` in
    /// `result<T, E>` found in WIT.
    pub trappable_error_type: Vec<TrappableError>,

    /// Whether or not to generate code for only the interfaces of this wit file or not.
    pub only_interfaces: bool,

    /// Configuration of which imports are allowed to generate a trap.
    pub trappable_imports: TrappableImports,

    /// Remapping of interface names to rust module names.
    /// TODO: is there a better type to use for the value of this map?
    pub with: HashMap<String, String>,

    /// Additional derive attributes to add to generated types. If using in a CLI, this flag can be
    /// specified multiple times to add multiple attributes.
    ///
    /// These derive attributes will be added to any generated structs or enums
    pub additional_derive_attributes: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct TrappableError {
    /// Full path to the error, such as `wasi:io/streams/error`.
    pub wit_path: String,

    /// The name, in Rust, of the error type to generate.
    pub rust_type_name: String,
}

#[derive(Default, Debug, Clone)]
pub enum TrappableImports {
    /// No imports are allowed to trap.
    #[default]
    None,
    /// All imports may trap.
    All,
    /// Only the specified set of functions may trap.
    Only(HashSet<String>),
}

impl TrappableImports {
    fn can_trap(&self, f: &Function) -> bool {
        match self {
            TrappableImports::None => false,
            TrappableImports::All => true,
            TrappableImports::Only(set) => set.contains(&f.name),
        }
    }
}

impl Opts {
    pub fn generate(&self, resolve: &Resolve, world: WorldId) -> anyhow::Result<String> {
        let mut r = Bindgen::default();
        r.sizes.fill(resolve);
        r.opts = self.clone();
        r.generate(resolve, world)
    }
}

impl Bindgen {
    fn name_interface(
        &mut self,
        resolve: &Resolve,
        id: InterfaceId,
        name: &WorldKey,
        is_export: bool,
    ) -> bool {
        let mut path = Vec::new();
        if is_export {
            path.push("exports".to_string());
        }
        match name {
            WorldKey::Name(name) => {
                path.push(name.to_snake_case());
            }
            WorldKey::Interface(_) => {
                let iface = &resolve.interfaces[id];
                let pkgname = &resolve.packages[iface.package.unwrap()].name;
                path.push(pkgname.namespace.to_snake_case());
                path.push(self.name_package_module(resolve, iface.package.unwrap()));
                path.push(to_rust_ident(iface.name.as_ref().unwrap()));
            }
        }
        let entry = if let Some(name_at_root) = self.lookup_replacement(resolve, name, None) {
            InterfaceName::Remapped {
                name_at_root,
                local_path: path,
            }
        } else {
            InterfaceName::Path(path)
        };

        let remapped = matches!(entry, InterfaceName::Remapped { .. });
        self.interface_names.insert(id, entry);
        remapped
    }

    /// If the package `id` is the only package with its namespace/name combo
    /// then pass through the name unmodified. If, however, there are multiple
    /// versions of this package then the package module is going to get version
    /// information.
    fn name_package_module(&self, resolve: &Resolve, id: PackageId) -> String {
        let pkg = &resolve.packages[id];
        let versions_with_same_name = resolve
            .packages
            .iter()
            .filter_map(|(_, p)| {
                if p.name.namespace == pkg.name.namespace && p.name.name == pkg.name.name {
                    Some(&p.name.version)
                } else {
                    None
                }
            })
            .collect::<Vec<_>>();
        let base = pkg.name.name.to_snake_case();
        if versions_with_same_name.len() == 1 {
            return base;
        }

        let version = match &pkg.name.version {
            Some(version) => version,
            // If this package didn't have a version then don't mangle its name
            // and other packages with the same name but with versions present
            // will have their names mangled.
            None => return base,
        };

        // Here there's multiple packages with the same name that differ only in
        // version, so the version needs to be mangled into the Rust module name
        // that we're generating. This in theory could look at all of
        // `versions_with_same_name` and produce a minimal diff, e.g. for 0.1.0
        // and 0.2.0 this could generate "foo1" and "foo2", but for now
        // a simpler path is chosen to generate "foo0_1_0" and "foo0_2_0".
        let version = version
            .to_string()
            .replace(['.', '-', '+'], "_")
            .to_snake_case();
        format!("{base}{version}")
    }

    fn generate(&mut self, resolve: &Resolve, id: WorldId) -> anyhow::Result<String> {
        self.types.analyze(resolve, id);
        for (i, te) in self.opts.trappable_error_type.iter().enumerate() {
            let id = resolve_type_in_package(resolve, &te.wit_path)
                .context(format!("resolving {:?}", te))
                .unwrap();
            let name = format!("_TrappableError{i}");
            uwriteln!(self.src, "type {name} = {};", te.rust_type_name);
            let prev = self.trappable_errors.insert(id, name);
            assert!(prev.is_none());
        }

        // Convert all entries in `with` as relative to the root of where the
        // macro itself is invoked. This emits a `pub use` to bring the name
        // into scope under an "anonymous name" which then replaces the `with`
        // map entry.
        let mut with = self.opts.with.iter_mut().collect::<Vec<_>>();
        with.sort();
        for (i, (_k, v)) in with.into_iter().enumerate() {
            let name = format!("__with_name{i}");
            uwriteln!(self.src, "#[doc(hidden)]\npub use {v} as {name};");
            *v = name;
        }

        let world = &resolve.worlds[id];
        for (name, import) in world.imports.iter() {
            if !self.opts.only_interfaces || matches!(import, WorldItem::Interface(_)) {
                self.import(resolve, id, name, import);
            }
        }

        for (name, export) in world.exports.iter() {
            if !self.opts.only_interfaces || matches!(export, WorldItem::Interface(_)) {
                self.export(resolve, name, export);
            }
        }
        self.finish(resolve, id)
    }

    fn import(&mut self, resolve: &Resolve, world: WorldId, name: &WorldKey, item: &WorldItem) {
        let mut gen = InterfaceGenerator::new(self, resolve);
        match item {
            WorldItem::Function(func) => {
                // Only generate a trait signature for free functions since
                // resource-related functions get their trait signatures
                // during `type_resource`.
                if let FunctionKind::Freestanding = func.kind {
                    gen.generate_function_trait_sig(func, None);
                }
                let sig = mem::take(&mut gen.src).into();
                gen.generate_add_function_to_linker(TypeOwner::World(world), func, "linker");
                let add_to_linker = gen.src.into();
                self.import_functions
                    .push(ImportFunction { sig, add_to_linker });
            }
            WorldItem::Interface(id) => {
                gen.gen.interface_last_seen_as_import.insert(*id, true);
                if gen.gen.name_interface(resolve, *id, name, false) {
                    return;
                }
                gen.current_interface = Some((*id, name, false));
                gen.types(*id);
                let key_name = resolve.name_world_key(name);

                gen.generate_add_to_linker(*id, &key_name);

                let module = &gen.src[..];

                let snake = match name {
                    WorldKey::Name(s) => s.to_snake_case(),
                    WorldKey::Interface(id) => resolve.interfaces[*id]
                        .name
                        .as_ref()
                        .unwrap()
                        .to_snake_case(),
                };
                let module = format!(
                    "
                        #[allow(clippy::all)]
                        pub mod {snake} {{
                            #[allow(unused_imports)]
                            use wasm_component_layer::__internal::anyhow;

                            {module}
                        }}
                    "
                );
                self.import_interfaces
                    .push((module, self.interface_names[id].clone()));
            }
            WorldItem::Type(ty) => {
                let interface_name = resolve.name_world_key(name);
                let name = match name {
                    WorldKey::Name(name) => name,
                    WorldKey::Interface(_) => unreachable!(),
                };
                gen.define_type(name, &interface_name, *ty);
                let body = mem::take(&mut gen.src);
                self.src.push_str(&body);
            }
        };
    }

    fn export(&mut self, resolve: &Resolve, name: &WorldKey, item: &WorldItem) {
        let mut gen = InterfaceGenerator::new(self, resolve);
        let (field, ty, getter) = match item {
            WorldItem::Function(func) => {
                gen.define_rust_guest_export(resolve, None, func);
                let body = mem::take(&mut gen.src).into();
                let (_name, getter) = gen.extract_typed_function(func);
                assert!(gen.src.is_empty());
                self.exports.funcs.push(body);
                (
                    func_field_name(resolve, func),
                    "wasm_component_layer::Func".to_string(),
                    getter,
                )
            }
            WorldItem::Type(_) => unreachable!(),
            WorldItem::Interface(id) => {
                gen.gen.interface_last_seen_as_import.insert(*id, false);
                gen.gen.name_interface(resolve, *id, name, true);
                gen.current_interface = Some((*id, name, true));
                gen.types(*id);
                let struct_name = "Guest";
                let iface = &resolve.interfaces[*id];
                let iface_name = match name {
                    WorldKey::Name(name) => name,
                    WorldKey::Interface(_) => iface.name.as_ref().unwrap(),
                };
                uwriteln!(gen.src, "pub struct {struct_name} {{");
                for (_, func) in iface.functions.iter() {
                    uwriteln!(
                        gen.src,
                        "{}: wasm_component_layer::TypedFunc<(",
                        func_field_name(resolve, func)
                    );
                    for (_, ty) in func.params.iter() {
                        gen.print_ty(ty, TypeMode::Owned, None);
                        gen.push_str(", ");
                    }
                    gen.src.push_str("), (");
                    for ty in func.results.iter_types() {
                        gen.print_ty(ty, TypeMode::Owned, None);
                        gen.push_str(", ");
                    }
                    gen.src.push_str(")>,");
                }
                uwriteln!(gen.src, "}}");

                uwriteln!(gen.src, "impl {struct_name} {{");
                uwrite!(
                    gen.src,
                    "
                        pub fn new(
                            __exports: &wasm_component_layer::ExportInstance,
                        ) -> anyhow::Result<{struct_name}> {{
                    "
                );
                let mut fields = Vec::new();
                for (_, func) in iface.functions.iter() {
                    let (name, getter) = gen.extract_typed_function(func);
                    uwriteln!(gen.src, "let {name} = {getter};");
                    fields.push(name);
                }
                uwriteln!(gen.src, "Ok({struct_name} {{");
                for name in fields {
                    uwriteln!(gen.src, "{name},");
                }
                uwriteln!(gen.src, "}})");
                uwriteln!(gen.src, "}}");

                let mut resource_methods = IndexMap::new();

                for (_, func) in iface.functions.iter() {
                    match func.kind {
                        FunctionKind::Freestanding => {
                            gen.define_rust_guest_export(resolve, Some(name), func);
                        }
                        FunctionKind::Method(id)
                        | FunctionKind::Constructor(id)
                        | FunctionKind::Static(id) => {
                            resource_methods.entry(id).or_insert(Vec::new()).push(func);
                        }
                    }
                }

                for (id, _) in resource_methods.iter() {
                    let name = resolve.types[*id].name.as_ref().unwrap();
                    let snake = name.to_snake_case();
                    let camel = name.to_upper_camel_case();
                    uwriteln!(
                        gen.src,
                        "pub fn {snake}(&self) -> Guest{camel}<'_> {{
                            Guest{camel} {{ funcs: self }}
                        }}"
                    );
                }

                uwriteln!(gen.src, "}}");

                for (id, methods) in resource_methods {
                    let resource_name = resolve.types[id].name.as_ref().unwrap();
                    let camel = resource_name.to_upper_camel_case();
                    uwriteln!(gen.src, "impl Guest{camel}<'_> {{");
                    for method in methods {
                        gen.define_rust_guest_export(resolve, Some(name), method);
                    }
                    uwriteln!(gen.src, "}}");
                }

                let module = &gen.src[..];
                let snake = to_rust_ident(iface_name);

                let module = format!(
                    "
                        #[allow(clippy::all)]
                        pub mod {snake} {{
                            #[allow(unused_imports)]
                            use wasm_component_layer::__internal::anyhow;

                            {module}
                        }}
                    "
                );
                let pkgname = match name {
                    WorldKey::Name(_) => None,
                    WorldKey::Interface(_) => {
                        Some(resolve.packages[iface.package.unwrap()].name.clone())
                    }
                };
                self.exports
                    .modules
                    .push((module, self.interface_names[id].clone()));

                let name = resolve.name_world_key(name);
                let (path, method_name) = match pkgname {
                    Some(pkgname) => (
                        format!(
                            "exports::{}::{}::{snake}::{struct_name}",
                            pkgname.namespace.to_snake_case(),
                            self.name_package_module(resolve, iface.package.unwrap()),
                        ),
                        format!(
                            "{}_{}_{snake}",
                            pkgname.namespace.to_snake_case(),
                            self.name_package_module(resolve, iface.package.unwrap())
                        ),
                    ),
                    None => (format!("exports::{snake}::{struct_name}"), snake.clone()),
                };
                let getter = format!(
                    "\
                        {path}::new(
                            __exports.instance(&\"{name}\".try_into().unwrap())
                                .ok_or_else(|| anyhow::anyhow!(\"exported instance `{name}` not present\"))?
                        )?\
                    "
                );
                let field = format!("interface{}", self.exports.fields.len());
                self.exports.funcs.push(format!(
                    "
                        pub fn {method_name}(&self) -> &{path} {{
                            &self.{field}
                        }}
                    ",
                ));
                (field, path, getter)
            }
        };
        let prev = self.exports.fields.insert(field, (ty, getter));
        assert!(prev.is_none());
    }

    fn build_struct(&mut self, resolve: &Resolve, world: WorldId) {
        let camel = to_rust_upper_camel_case(&resolve.worlds[world].name);
        uwriteln!(self.src, "pub struct {camel} {{");
        for (name, (ty, _)) in self.exports.fields.iter() {
            uwriteln!(self.src, "{name}: {ty},");
        }
        self.src.push_str("}\n");

        self.toplevel_import_trait(resolve, world);

        uwriteln!(self.src, "const _: () = {{");
        uwriteln!(
            self.src,
            "
                #[allow(unused_imports)]
                use wasm_component_layer::__internal::anyhow;
            "
        );

        uwriteln!(self.src, "impl {camel} {{");
        self.toplevel_add_to_linker(resolve, world);
        uwriteln!(
            self.src,
            "
                /// Instantiates the provided `module` using the specified
                /// parameters, wrapping up the result in a structure that
                /// translates between wasm and the host.
                pub fn instantiate(
                    mut store: impl wasm_component_layer::AsContextMut,
                    component: &wasm_component_layer::Component,
                    linker: &wasm_component_layer::Linker,
                ) -> anyhow::Result<(Self, wasm_component_layer::Instance)> {{
                    let instance = linker.instantiate(&mut store, component)?;
                    Ok((Self::new(store, &instance)?, instance))
                }}

                /// Low-level creation wrapper for wrapping up the exports
                /// of the `instance` provided in this structure of wasm
                /// exports.
                ///
                /// This function will extract exports from the `instance`
                /// defined within `store` and wrap them all up in the
                /// returned structure which can be used to interact with
                /// the wasm module.
                pub fn new(
                    mut store: impl wasm_component_layer::AsContextMut,
                    instance: &wasm_component_layer::Instance,
                ) -> anyhow::Result<Self> {{
                    let mut _store = store.as_context_mut();
                    let __exports = instance.exports();
            ",
        );
        for (name, (_, get)) in self.exports.fields.iter() {
            uwriteln!(self.src, "let {name} = {get};");
        }
        uwriteln!(self.src, "Ok({camel} {{");
        for (name, _) in self.exports.fields.iter() {
            uwriteln!(self.src, "{name},");
        }
        uwriteln!(self.src, "}})");
        uwriteln!(self.src, "}}"); // close `fn new`

        for func in self.exports.funcs.iter() {
            self.src.push_str(func);
        }

        uwriteln!(self.src, "}}"); // close `impl {camel}`

        uwriteln!(self.src, "}};"); // close `const _: () = ...
    }

    fn finish(&mut self, resolve: &Resolve, world: WorldId) -> anyhow::Result<String> {
        let remapping_keys = self.opts.with.keys().cloned().collect::<HashSet<String>>();

        let mut unused_keys = remapping_keys
            .difference(&self.used_with_opts)
            .map(|s| s.as_str())
            .collect::<Vec<&str>>();

        unused_keys.sort();

        if !unused_keys.is_empty() {
            anyhow::bail!("interfaces were specified in the `with` config option but are not referenced in the target world: {unused_keys:?}");
        }

        if !self.opts.only_interfaces {
            self.build_struct(resolve, world)
        }

        let imports = mem::take(&mut self.import_interfaces);
        self.emit_modules(imports);

        let exports = mem::take(&mut self.exports.modules);
        self.emit_modules(exports);

        let mut src = mem::take(&mut self.src);
        if self.opts.rustfmt {
            let mut child = Command::new("rustfmt")
                .arg("--edition=2018")
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .spawn()
                .expect("failed to spawn `rustfmt`");
            child
                .stdin
                .take()
                .unwrap()
                .write_all(src.as_bytes())
                .unwrap();
            src.as_mut_string().truncate(0);
            child
                .stdout
                .take()
                .unwrap()
                .read_to_string(src.as_mut_string())
                .unwrap();
            let status = child.wait().unwrap();
            assert!(status.success());
        }

        Ok(src.into())
    }

    fn emit_modules(&mut self, modules: Vec<(String, InterfaceName)>) {
        #[derive(Default)]
        struct Module {
            submodules: BTreeMap<String, Module>,
            contents: Vec<String>,
        }
        let mut map = Module::default();
        for (module, name) in modules {
            let path = match name {
                InterfaceName::Remapped { local_path, .. } => local_path,
                InterfaceName::Path(path) => path,
            };
            let mut cur = &mut map;
            for name in path[..path.len() - 1].iter() {
                cur = cur
                    .submodules
                    .entry(name.clone())
                    .or_insert(Module::default());
            }
            cur.contents.push(module);
        }

        emit(&mut self.src, map);

        fn emit(me: &mut Source, module: Module) {
            for (name, submodule) in module.submodules {
                uwriteln!(me, "pub mod {name} {{");
                emit(me, submodule);
                uwriteln!(me, "}}");
            }
            for submodule in module.contents {
                uwriteln!(me, "{submodule}");
            }
        }
    }

    /// Attempts to find the `key`, possibly with the resource projection
    /// `item`, within the `with` map provided to bindings configuration.
    fn lookup_replacement(
        &mut self,
        resolve: &Resolve,
        key: &WorldKey,
        item: Option<&str>,
    ) -> Option<String> {
        struct Name<'a> {
            prefix: Prefix,
            item: Option<&'a str>,
        }

        #[derive(Copy, Clone)]
        enum Prefix {
            Namespace(PackageId),
            UnversionedPackage(PackageId),
            VersionedPackage(PackageId),
            UnversionedInterface(InterfaceId),
            VersionedInterface(InterfaceId),
        }

        let prefix = match key {
            WorldKey::Interface(id) => Prefix::VersionedInterface(*id),

            // Non-interface-keyed names don't get the lookup logic below,
            // they're relatively uncommon so only lookup the precise key here.
            WorldKey::Name(key) => {
                let to_lookup = match item {
                    Some(item) => format!("{key}/{item}"),
                    None => key.to_string(),
                };
                let result = self.opts.with.get(&to_lookup).cloned();
                if result.is_some() {
                    self.used_with_opts.insert(to_lookup.clone());
                }
                return result;
            }
        };

        // Here names are iteratively attempted as `key` + `item` is "walked to
        // its root" and each attempt is consulted in `self.opts.with`. This
        // loop will start at the leaf, the most specific path, and then walk to
        // the root, popping items, trying to find a result.
        //
        // Each time a name is "popped" the projection from the next path is
        // pushed onto `projection`. This means that if we actually find a match
        // then `projection` is a collection of namespaces that results in the
        // final replacement name.
        let mut name = Name { prefix, item };
        let mut projection = Vec::new();
        loop {
            let lookup = name.lookup_key(resolve);
            if let Some(renamed) = self.opts.with.get(&lookup) {
                projection.push(renamed.clone());
                projection.reverse();
                self.used_with_opts.insert(lookup);
                return Some(projection.join("::"));
            }
            if !name.pop(resolve, &mut projection) {
                return None;
            }
        }

        impl<'a> Name<'a> {
            fn lookup_key(&self, resolve: &Resolve) -> String {
                let mut s = self.prefix.lookup_key(resolve);
                if let Some(item) = self.item {
                    s.push('/');
                    s.push_str(item);
                }
                s
            }

            fn pop(&mut self, resolve: &'a Resolve, projection: &mut Vec<String>) -> bool {
                match (self.item, self.prefix) {
                    // If this is a versioned resource name, try the unversioned
                    // resource name next.
                    (Some(_), Prefix::VersionedInterface(id)) => {
                        self.prefix = Prefix::UnversionedInterface(id);
                        true
                    }
                    // If this is an unversioned resource name then time to
                    // ignore the resource itself and move on to the next most
                    // specific item, versioned interface names.
                    (Some(item), Prefix::UnversionedInterface(id)) => {
                        self.prefix = Prefix::VersionedInterface(id);
                        self.item = None;
                        projection.push(item.to_upper_camel_case());
                        true
                    }
                    (Some(_), _) => unreachable!(),
                    (None, _) => self.prefix.pop(resolve, projection),
                }
            }
        }

        impl Prefix {
            fn lookup_key(&self, resolve: &Resolve) -> String {
                match *self {
                    Prefix::Namespace(id) => resolve.packages[id].name.namespace.clone(),
                    Prefix::UnversionedPackage(id) => {
                        let mut name = resolve.packages[id].name.clone();
                        name.version = None;
                        name.to_string()
                    }
                    Prefix::VersionedPackage(id) => resolve.packages[id].name.to_string(),
                    Prefix::UnversionedInterface(id) => {
                        let id = resolve.id_of(id).unwrap();
                        match id.find('@') {
                            Some(i) => id[..i].to_string(),
                            None => id,
                        }
                    }
                    Prefix::VersionedInterface(id) => resolve.id_of(id).unwrap(),
                }
            }

            fn pop(&mut self, resolve: &Resolve, projection: &mut Vec<String>) -> bool {
                *self = match *self {
                    // try the unversioned interface next
                    Prefix::VersionedInterface(id) => Prefix::UnversionedInterface(id),
                    // try this interface's versioned package next
                    Prefix::UnversionedInterface(id) => {
                        let iface = &resolve.interfaces[id];
                        let name = iface.name.as_ref().unwrap();
                        projection.push(to_rust_ident(name));
                        Prefix::VersionedPackage(iface.package.unwrap())
                    }
                    // try the unversioned package next
                    Prefix::VersionedPackage(id) => Prefix::UnversionedPackage(id),
                    // try this package's namespace next
                    Prefix::UnversionedPackage(id) => {
                        let name = &resolve.packages[id].name;
                        projection.push(to_rust_ident(&name.name));
                        Prefix::Namespace(id)
                    }
                    // nothing left to try any more
                    Prefix::Namespace(_) => return false,
                };
                true
            }
        }
    }
}

impl Bindgen {
    fn has_world_trait(&self, resolve: &Resolve, world: WorldId) -> bool {
        !self.import_functions.is_empty() || get_world_resources(resolve, world).count() > 0
    }

    fn toplevel_import_trait(&mut self, resolve: &Resolve, world: WorldId) {
        if !self.has_world_trait(resolve, world) {
            return;
        }

        let world_camel = to_rust_upper_camel_case(&resolve.worlds[world].name);
        uwrite!(self.src, "pub trait {world_camel}Imports");
        for (i, resource) in get_world_resources(resolve, world).enumerate() {
            if i == 0 {
                uwrite!(self.src, ": ");
            } else {
                uwrite!(self.src, " + ");
            }
            uwrite!(self.src, "Host{}", resource.to_upper_camel_case());
        }
        uwriteln!(self.src, " {{");
        for f in self.import_functions.iter() {
            self.src.push_str(&f.sig);
            self.src.push_str("\n");
        }
        uwriteln!(self.src, "}}");
    }

    fn toplevel_add_to_linker(&mut self, resolve: &Resolve, world: WorldId) {
        let has_world_trait = self.has_world_trait(resolve, world);
        if self.import_interfaces.is_empty() && !has_world_trait {
            return;
        }
        let mut interfaces = Vec::new();
        for (_, name) in self.import_interfaces.iter() {
            let path = match name {
                InterfaceName::Remapped { .. } => unreachable!("imported a remapped module"),
                InterfaceName::Path(path) => path,
            };
            interfaces.push(path.join("::"));
        }

        uwrite!(
            self.src,
            "
                pub fn add_to_linker<T, U>(
                    mut ctx: impl wasm_component_layer::AsContextMut<UserState = T>,
                    linker: &mut wasm_component_layer::Linker,
                ) -> anyhow::Result<()>
                    where U: \
            "
        );
        let world_camel = to_rust_upper_camel_case(&resolve.worlds[world].name);
        let world_trait = format!("{world_camel}Imports");
        for (i, name) in interfaces
            .iter()
            .map(|n| format!("{n}::Host"))
            .chain(if has_world_trait {
                Some(world_trait.clone())
            } else {
                None
            })
            .enumerate()
        {
            if i > 0 {
                self.src.push_str(" + ");
            }
            self.src.push_str(&name);
        }

        self.src.push_str(",\n{\n");
        self.src.push_str("let mut ctx = ctx.as_context_mut();\n");
        for name in interfaces.iter() {
            uwriteln!(self.src, "{name}::add_to_linker::<T, U>(&mut ctx, linker)?;");
        }
        if has_world_trait {
            uwriteln!(self.src, "Self::add_root_to_linker::<T>(linker)?;");
        }
        uwriteln!(self.src, "Ok(())\n}}");
        if !has_world_trait {
            return;
        }

        uwrite!(
            self.src,
            "
                pub fn add_root_to_linker<T, U>(
                    linker: &mut wasm_component_layer::Linker,
                    get: impl Fn(&mut T) -> &mut U + Send + Sync + Copy + 'static,
                ) -> anyhow::Result<()>
                    where U: {world_trait}
                {{
                    let mut linker = linker.root();
            ",
        );
        // for name in get_world_resources(resolve, world) {
        //     let camel = name.to_upper_camel_case();
        //     uwriteln!(
        //         self.src,
        //         "linker.resource(
        //             \"{name}\",
        //             wasmtime::component::ResourceType::host::<{camel}>(),
        //             move |mut store, rep| -> anyhow::Result<()> {{
        //                 Host{camel}::drop(get(store.data_mut()), wasmtime::component::Resource::new_own(rep))
        //             }},
        //         )?;"
        //     )
        // }

        for f in self.import_functions.iter() {
            self.src.push_str(&f.add_to_linker);
            self.src.push_str("\n");
        }
        uwriteln!(self.src, "Ok(())\n}}");
    }
}

fn resolve_type_in_package(resolve: &Resolve, wit_path: &str) -> anyhow::Result<TypeId> {
    // Build a map, `packages_to_omit_version`, where that package can be
    // uniquely identified by its name/namespace combo and as such version
    // information is not required.
    let mut packages_with_same_name = HashMap::new();
    for (id, pkg) in resolve.packages.iter() {
        packages_with_same_name
            .entry(PackageName {
                version: None,
                ..pkg.name.clone()
            })
            .or_insert(Vec::new())
            .push(id)
    }
    let packages_to_omit_version = packages_with_same_name
        .iter()
        .filter_map(
            |(_name, list)| {
                if list.len() == 1 {
                    Some(list)
                } else {
                    None
                }
            },
        )
        .flatten()
        .collect::<HashSet<_>>();

    let mut found_interface = false;

    // Look for an interface whose assigned prefix starts `wit_path`. Not
    // exactly the most efficient thing ever but is sufficient for now.
    for (id, interface) in resolve.interfaces.iter() {
        found_interface = true;

        let iface_name = match &interface.name {
            Some(name) => name,
            None => continue,
        };
        let pkgid = interface.package.unwrap();
        let pkgname = &resolve.packages[pkgid].name;
        let prefix = if packages_to_omit_version.contains(&pkgid) {
            let mut name = pkgname.clone();
            name.version = None;
            format!("{name}/{iface_name}")
        } else {
            resolve.id_of(id).unwrap()
        };
        let wit_path = match wit_path.strip_prefix(&prefix) {
            Some(rest) => rest,
            None => continue,
        };

        let wit_path = match wit_path.strip_prefix('/') {
            Some(rest) => rest,
            None => continue,
        };

        match interface.types.get(wit_path).copied() {
            Some(type_id) => return Ok(type_id),
            None => continue,
        }
    }

    if found_interface {
        bail!("no types found to match `{wit_path}` in interface");
    }

    bail!("no package/interface found to match `{wit_path}`")
}

struct InterfaceGenerator<'a> {
    src: Source,
    gen: &'a mut Bindgen,
    resolve: &'a Resolve,
    current_interface: Option<(InterfaceId, &'a WorldKey, bool)>,
}

impl<'a> InterfaceGenerator<'a> {
    fn new(gen: &'a mut Bindgen, resolve: &'a Resolve) -> InterfaceGenerator<'a> {
        InterfaceGenerator {
            src: Source::default(),
            gen,
            resolve,
            current_interface: None,
        }
    }

    fn types_imported(&self) -> bool {
        match self.current_interface {
            Some((_, _, is_export)) => !is_export,
            None => true,
        }
    }

    fn types(&mut self, id: InterfaceId) {
        let interface_name = self
            .resolve
            .id_of(id)
            .expect("unexpected anonymous interface");
        for (name, id) in self.resolve.interfaces[id].types.iter() {
            self.define_type(name, &interface_name, *id);
        }
    }

    fn define_type(&mut self, name: &str, interface_name: &str, id: TypeId) {
        let ty = &self.resolve.types[id];
        match &ty.kind {
            TypeDefKind::Record(record) => {
                self.type_record(id, name, interface_name, record, &ty.docs)
            }
            TypeDefKind::Flags(flags) => self.type_flags(id, name, interface_name, flags, &ty.docs),
            TypeDefKind::Tuple(tuple) => self.type_tuple(id, name, tuple, &ty.docs),
            TypeDefKind::Enum(enum_) => self.type_enum(id, name, interface_name, enum_, &ty.docs),
            TypeDefKind::Variant(variant) => {
                self.type_variant(id, name, interface_name, variant, &ty.docs)
            }
            TypeDefKind::Option(t) => self.type_option(id, name, t, &ty.docs),
            TypeDefKind::Result(r) => self.type_result(id, name, r, &ty.docs),
            TypeDefKind::List(t) => self.type_list(id, name, t, &ty.docs),
            TypeDefKind::Type(t) => self.type_alias(id, name, t, &ty.docs),
            TypeDefKind::Future(_) => todo!("generate for future"),
            TypeDefKind::Stream(_) => todo!("generate for stream"),
            TypeDefKind::Handle(handle) => self.type_handle(id, name, handle, &ty.docs),
            TypeDefKind::Resource => self.type_resource(id, name, interface_name, ty, &ty.docs),
            TypeDefKind::Unknown => unreachable!(),
        }
    }

    fn type_handle(&mut self, _id: TypeId, name: &str, handle: &Handle, docs: &Docs) {
        self.rustdoc(docs);
        let name = name.to_upper_camel_case();
        uwriteln!(self.src, "pub type {name} = ");
        self.print_handle(handle, None);
        self.push_str(";\n");
    }

    fn type_resource(&mut self, id: TypeId, name: &str, interface_name: &str, resource: &TypeDef, docs: &Docs) {
        let camel = name.to_upper_camel_case();

        if self.types_imported() {
            // self.rustdoc(docs);

            // let replacement = match self.current_interface {
            //     Some((_, key, _)) => self.gen.lookup_replacement(self.resolve, key, Some(name)),
            //     None => {
            //         self.gen.used_with_opts.insert(name.into());
            //         self.gen.opts.with.get(name).cloned()
            //     }
            // };
            // match replacement {
            //     Some(path) => {
            //         uwriteln!(
            //             self.src,
            //             "pub use {}{path} as {camel};",
            //             self.path_to_root()
            //         );
            //     }
            //     None => {
            //         uwriteln!(self.src, "pub enum {camel} {{}}");
            //     }
            // }

            self.rustdoc(docs);
            uwriteln!(self.src, "pub trait Host{camel}: 'static + Send + Sync + Sized {{");

            let functions = match resource.owner {
                TypeOwner::World(id) => self.resolve.worlds[id]
                    .imports
                    .values()
                    .filter_map(|item| match item {
                        WorldItem::Function(f) => Some(f),
                        _ => None,
                    })
                    .collect(),
                TypeOwner::Interface(id) => self.resolve.interfaces[id]
                    .functions
                    .values()
                    .collect::<Vec<_>>(),
                TypeOwner::None => {
                    panic!("A resource must be owned by a world or interface");
                }
            };

            for func in functions {
                match func.kind {
                    FunctionKind::Method(resource)
                    | FunctionKind::Static(resource)
                    | FunctionKind::Constructor(resource)
                        if id == resource => {}
                    _ => continue,
                }

                self.generate_function_trait_sig(func, Some(id));
            }

            uwriteln!(self.src, "}}");

            // TODO: this impl is ot allowed
            uwriteln!(self.src, "impl<T: Host{camel}> wasm_component_layer::Resource for T {{
                fn ty() -> wasm_component_layer::ResourceType {{
                    static RESOURCE_TY: std::sync::OnceLock<wasm_component_layer::ResourceType> = std::sync::OnceLock::new();
                    RESOURCE_TY.get_or_init(|| {{
                        wasm_component_layer::ResourceType::new::<Self>(Some(
                            wasm_component_layer::TypeIdentifier::new(\"{name}\", Some(
                                \"{interface_name}\".try_into().unwrap()
                            ))
                        ))
                    }}).clone()
                }}
            }}");
        } else {
            // self.rustdoc(docs);
            // uwriteln!(
            //     self.src,
            //     "
            //         pub type {camel} = wasmtime::component::ResourceAny;

            //         pub struct Guest{camel}<'a> {{
            //             funcs: &'a Guest,
            //         }}
            //     "
            // );
        }
    }

    fn type_record(
        &mut self,
        id: TypeId,
        name: &str,
        interface_name: &str,
        record: &Record,
        docs: &Docs,
    ) {
        let info = self.info(id);

        // We use a BTree set to make sure we don't have any duplicates and we have a stable order
        let mut additional_derives: BTreeSet<String> = self
            .gen
            .opts
            .additional_derive_attributes
            .iter()
            .cloned()
            .collect();

        self.rustdoc(docs);

        if info.is_copy() {
            additional_derives.extend(["Copy", "Clone"].into_iter().map(|s| s.to_string()));
        } else if info.is_clone() {
            additional_derives.insert("Clone".to_string());
        }

        if !additional_derives.is_empty() {
            self.push_str("#[derive(");
            self.push_str(
                &additional_derives
                    .into_iter()
                    .collect::<Vec<_>>()
                    .join(", "),
            );
            self.push_str(")]\n")
        }

        let rust_name = to_rust_upper_camel_case(name);
        self.push_str("pub struct ");
        self.push_str(&rust_name);
        self.push_str(" {\n");
        for field in record.fields.iter() {
            self.rustdoc(&field.docs);
            self.push_str("pub ");
            self.push_str(&to_rust_ident(&field.name));
            self.push_str(": ");
            self.print_ty(&field.ty, TypeMode::Owned, None);
            self.push_str(",\n");
        }
        self.push_str("}\n");

        self.src.push_str("impl ");
        self.src.push_str(&rust_name);
        self.src.push_str("{\n");
        for field in record.fields.iter() {
            self.src.push_str("fn get_");
            self.src.push_str(&field.name.to_snake_case());
            self.src
                .push_str("_field_name() -> &'static std::sync::Arc<str> {\n");
            self.src.push_str("static FIELD_NAME: std::sync::OnceLock<std::sync::Arc<str>> = std::sync::OnceLock::new();\n");
            self.src
                .push_str("FIELD_NAME.get_or_init(|| std::sync::Arc::from(\"");
            self.src.push_str(&field.name);
            self.src.push_str("\"))\n");
            self.src.push_str("}\n\n");
        }
        self.src
            .push_str("fn get_record_ty() -> &'static wasm_component_layer::RecordType {\n");
        self.src.push_str("static RECORD_TY: std::sync::OnceLock<wasm_component_layer::RecordType> = std::sync::OnceLock::new();\n");
        self.src.push_str("RECORD_TY.get_or_init(|| {\n");
        self.src
            .push_str("wasm_component_layer::RecordType::new(\n");
        self.src
            .push_str("Some(wasm_component_layer::TypeIdentifier::new(\n");
        uwrite!(
            self.src,
            "\"{name}\", Some(\"{interface_name}\".try_into().unwrap())\n",
        );
        self.src.push_str(")),\n");
        self.src.push_str("[");
        for field in record.fields.iter() {
            self.src.push_str("(std::sync::Arc::clone(Self::get_");
            self.src.push_str(&field.name.to_snake_case());
            self.src.push_str("_field_name()), <");
            self.print_ty(&field.ty, TypeMode::Owned, None);
            self.src
                .push_str(" as wasm_component_layer::ComponentType>::ty()),\n");
        }
        self.src.push_str("],\n");
        self.src.push_str(").unwrap()\n");
        self.src.push_str("})\n");
        self.src.push_str("}\n");
        self.src.push_str("}\n\n");
        self.src
            .push_str("impl wasm_component_layer::ComponentType for ");
        self.src.push_str(&rust_name);
        self.src.push_str("{\n");
        self.src
            .push_str("fn ty() -> wasm_component_layer::ValueType {\n");
        self.src
            .push_str("wasm_component_layer::ValueType::Record(Self::get_record_ty().clone())\n");
        self.src.push_str("}\n\n");
        self.src.push_str(
            "fn from_value(value: &wasm_component_layer::Value) -> anyhow::Result<Self> {\n",
        );
        self.src.push_str("let record = match value {\n");
        self.src
            .push_str("wasm_component_layer::Value::Record(record) => record,\n");
        self.src
            .push_str("_ => anyhow::bail!(\"incorrect type, expected record\"),\n");
        self.src.push_str("};\n");
        self.src.push_str(
            "anyhow::ensure!(&record.ty() == Self::get_record_ty(), \"incorrect record type\");\n",
        );
        self.src.push_str("Ok(Self {\n");
        for field in record.fields.iter() {
            self.src.push_str(&to_rust_ident(&field.name));
            self.src.push_str(": <");
            self.print_ty(&field.ty, TypeMode::Owned, None);
            self.src
                .push_str(" as wasm_component_layer::ComponentType>::from_value(&record.field(\"");
            self.src.push_str(&field.name);
            self.src
                .push_str("\").ok_or_else(|| anyhow::anyhow!(\"record missing field\"))?)?,");
        }
        self.src.push_str("})\n");
        self.src.push_str("}\n\n");
        self.src
            .push_str("fn into_value(self) -> anyhow::Result<wasm_component_layer::Value> {\n");
        self.src.push_str(
            "let record = wasm_component_layer::Record::new(Self::get_record_ty().clone(), [\n",
        );
        for field in record.fields.iter() {
            self.src.push_str("(std::sync::Arc::clone(Self::get_");
            self.src.push_str(&field.name.to_snake_case());
            self.src.push_str("_field_name()), <");
            self.print_ty(&field.ty, TypeMode::Owned, None);
            self.src
                .push_str(" as wasm_component_layer::ComponentType>::into_value(self.");
            self.src.push_str(&to_rust_ident(&field.name));
            self.src.push_str(")?),\n");
        }
        self.src.push_str("])?;\n");
        self.src
            .push_str("Ok(wasm_component_layer::Value::Record(record))\n");
        self.src.push_str("}\n");
        self.src.push_str("}\n\n");

        self.push_str("impl core::fmt::Debug for ");
        self.push_str(&rust_name);
        self.push_str(" {\n");
        self.push_str("fn fmt(&self, fmt: &mut core::fmt::Formatter) -> core::fmt::Result {\n");
        self.push_str(&format!("fmt.debug_struct(\"{}\")", rust_name));
        for field in record.fields.iter() {
            self.push_str(&format!(
                ".field(\"{}\", &self.{})",
                to_rust_ident(&field.name),
                to_rust_ident(&field.name)
            ));
        }
        self.push_str(".finish()\n");
        self.push_str("}\n");
        self.push_str("}\n\n");

        if info.error {
            self.push_str("impl core::fmt::Display for ");
            self.push_str(&rust_name);
            self.push_str(" {\n");
            self.push_str("fn fmt(&self, fmt: &mut core::fmt::Formatter) -> core::fmt::Result {\n");
            self.push_str("write!(f, \"{:?}\", self)\n");
            self.push_str("}\n");
            self.push_str("}\n\n");
            self.push_str("impl std::error::Error for ");
            self.push_str(&rust_name);
            self.push_str("{}\n\n");
        }
    }

    fn type_tuple(&mut self, id: TypeId, _name: &str, tuple: &Tuple, docs: &Docs) {
        let info = self.info(id);
        for (name, mode) in self.modes_of(id) {
            let lt = self.lifetime_for(&info, mode);
            self.rustdoc(docs);
            self.push_str(&format!("pub type {}", name));
            self.print_generics(lt);
            self.push_str(" = (");
            for ty in tuple.types.iter() {
                self.print_ty(ty, mode, None);
                self.push_str(",");
            }
            self.push_str(");\n");
        }
    }

    fn type_flags(
        &mut self,
        _id: TypeId,
        name: &str,
        interface_name: &str,
        flags: &Flags,
        docs: &Docs,
    ) {
        self.rustdoc(docs);
        let rust_name = to_rust_upper_camel_case(name);
        self.src
            .push_str("wasm_component_layer::__internal::flags!(\n");
        self.src.push_str(&format!("{rust_name} {{\n"));
        for flag in flags.flags.iter() {
            uwrite!(self.src, "{},\n", flag.name.to_upper_camel_case());
        }
        self.src.push_str("}\n");
        self.src.push_str(");\n\n");
        self.src.push_str("impl ");
        self.src.push_str(&rust_name);
        self.src.push_str("{\n");
        self.src
            .push_str("fn get_flags_ty() -> &'static wasm_component_layer::FlagsType {\n");
        self.src.push_str("static FLAGS_TY: std::sync::OnceLock<wasm_component_layer::FlagsType> = std::sync::OnceLock::new();\n");
        self.src.push_str("FLAGS_TY.get_or_init(|| {\n");
        self.src.push_str("wasm_component_layer::FlagsType::new(\n");
        self.src
            .push_str("Some(wasm_component_layer::TypeIdentifier::new(\n");
        uwrite!(
            self.src,
            "\"{name}\", Some(\"{interface_name}\".try_into().unwrap())\n",
        );
        self.src.push_str(")),\n");
        self.src.push_str("[");
        for flag in flags.flags.iter() {
            uwrite!(self.src, "\"{}\",", flag.name);
        }
        self.src.push_str("],\n");
        self.src.push_str(").unwrap()\n");
        self.src.push_str("})\n");
        self.src.push_str("}\n");
        self.src.push_str("}\n\n");
        self.src
            .push_str("impl wasm_component_layer::ComponentType for ");
        self.src.push_str(&rust_name);
        self.src.push_str("{\n");
        self.src
            .push_str("fn ty() -> wasm_component_layer::ValueType {\n");
        self.src
            .push_str("wasm_component_layer::ValueType::Flags(Self::get_flags_ty().clone())\n");
        self.src.push_str("}\n\n");
        self.src.push_str(
            "fn from_value(value: &wasm_component_layer::Value) -> anyhow::Result<Self> {\n",
        );
        self.src.push_str("let flags = match value {\n");
        self.src
            .push_str("wasm_component_layer::Value::Flags(flags) => flags,\n");
        self.src
            .push_str("_ => anyhow::bail!(\"incorrect type, expected flags\"),\n");
        self.src.push_str("};\n");
        self.src.push_str(
            "anyhow::ensure!(&flags.ty() == Self::get_flags_ty(), \"incorrect flags type\");\n",
        );
        self.src.push_str("let mut this = Self::default();\n");
        for flag in flags.flags.iter() {
            uwrite!(
                self.src,
                "if flags.get(\"{}\") {{\nthis |= Self::{};\n}}\n",
                flag.name,
                flag.name.to_upper_camel_case()
            );
        }
        self.src.push_str("Ok(this)\n");
        self.src.push_str("}\n\n");
        self.src
            .push_str("fn into_value(self) -> anyhow::Result<wasm_component_layer::Value> {\n");
        self.src.push_str(
            "let mut flags = wasm_component_layer::Flags::new(Self::get_flags_ty().clone());\n",
        );
        for flag in flags.flags.iter() {
            uwrite!(
                self.src,
                "if (self & Self::{}) == Self::{} {{\nflags.set(\"{}\", true);\n}}\n",
                flag.name.to_upper_camel_case(),
                flag.name.to_upper_camel_case(),
                flag.name
            );
        }
        self.src
            .push_str("Ok(wasm_component_layer::Value::Flags(flags))\n");
        self.src.push_str("}\n");
        self.src.push_str("}\n\n");
    }

    fn type_variant(
        &mut self,
        id: TypeId,
        name: &str,
        interface_name: &str,
        variant: &Variant,
        docs: &Docs,
    ) {
        let info = self.info(id);

        // We use a BTree set to make sure we don't have any duplicates and have a stable order
        let mut derives: BTreeSet<String> = self
            .gen
            .opts
            .additional_derive_attributes
            .iter()
            .cloned()
            .collect();

        if info.is_copy() {
            derives.extend(["Copy", "Clone"].into_iter().map(|s| s.to_string()));
        } else if info.is_clone() {
            derives.insert("Clone".to_string());
        }

        let rust_name = to_rust_upper_camel_case(name);
        self.rustdoc(docs);

        if !derives.is_empty() {
            self.push_str("#[derive(");
            self.push_str(&derives.into_iter().collect::<Vec<_>>().join(", "));
            self.push_str(")]\n")
        }

        self.push_str(&format!("pub enum {} {{\n", rust_name));
        for case in variant.cases.iter() {
            self.rustdoc(&case.docs);
            self.push_str(&case.name.to_upper_camel_case());
            if let Some(ty) = &case.ty {
                self.push_str("(");
                self.print_ty(ty, TypeMode::Owned, None);
                self.push_str(")")
            }
            self.push_str(",\n");
        }
        self.push_str("}\n");

        self.src.push_str("impl ");
        self.src.push_str(&rust_name);
        self.src.push_str("{\n");
        self.src
            .push_str("fn get_variant_ty() -> &'static wasm_component_layer::VariantType {\n");
        self.src.push_str("static VARIANT_TY: std::sync::OnceLock<wasm_component_layer::VariantType> = std::sync::OnceLock::new();\n");
        self.src.push_str("VARIANT_TY.get_or_init(|| {\n");
        self.src
            .push_str("wasm_component_layer::VariantType::new(\n");
        self.src
            .push_str("Some(wasm_component_layer::TypeIdentifier::new(\n");
        uwrite!(
            self.src,
            "\"{name}\", Some(\"{interface_name}\".try_into().unwrap())\n",
        );
        self.src.push_str(")),\n");
        self.src.push_str("[\n");
        for case in variant.cases.iter() {
            self.src
                .push_str("wasm_component_layer::VariantCase::new(\"");
            self.src.push_str(&case.name);
            self.src.push_str("\", ");
            match &case.ty {
                None => self.src.push_str("None"),
                Some(ty) => {
                    self.src.push_str("Some(<");
                    self.print_ty(ty, TypeMode::Owned, None);
                    self.src
                        .push_str(" as wasm_component_layer::ComponentType>::ty())");
                }
            }
            self.src.push_str("),\n");
        }
        self.src.push_str("],\n");
        self.src.push_str(").unwrap()\n");
        self.src.push_str("})\n");
        self.src.push_str("}\n");
        self.src.push_str("}\n\n");
        self.src
            .push_str("impl wasm_component_layer::ComponentType for ");
        self.src.push_str(&rust_name);
        self.src.push_str("{\n");
        self.src
            .push_str("fn ty() -> wasm_component_layer::ValueType {\n");
        self.src
            .push_str("wasm_component_layer::ValueType::Variant(Self::get_variant_ty().clone())\n");
        self.src.push_str("}\n\n");
        self.src.push_str(
            "fn from_value(value: &wasm_component_layer::Value) -> anyhow::Result<Self> {\n",
        );
        self.src.push_str("let variant = match value {\n");
        self.src
            .push_str("wasm_component_layer::Value::Variant(variant) => variant,\n");
        self.src
            .push_str("_ => anyhow::bail!(\"incorrect type, expected variant\"),\n");
        self.src.push_str("};\n");
        self.src.push_str("anyhow::ensure!(&variant.ty() == Self::get_variant_ty(), \"incorrect variant type\");\n");
        self.src
            .push_str("let this = match (variant.discriminant(), variant.value()) {\n");
        for (i, case) in variant.cases.iter().enumerate() {
            match &case.ty {
                None => uwrite!(
                    self.src,
                    "({i}, None) => Self::{},",
                    case.name.to_upper_camel_case(),
                ),
                Some(_) => uwrite!(
                    self.src,
                    "({i}, Some(v)) => Self::{}(wasm_component_layer::ComponentType::from_value(&v)?),",
                    case.name.to_upper_camel_case(),
                ),
            }
        }
        self.src
            .push_str("_ => anyhow::bail!(\"unexpected variant discriminant\"),\n");
        self.src.push_str("};\n");
        self.src.push_str("Ok(this)\n");
        self.src.push_str("}\n\n");
        self.src
            .push_str("fn into_value(self) -> anyhow::Result<wasm_component_layer::Value> {\n");
        self.src
            .push_str("let (discriminant, value) = match self {\n");
        for (i, case) in variant.cases.iter().enumerate() {
            match &case.ty {
                None => uwrite!(
                    self.src,
                    "Self::{} => ({i}, None),",
                    case.name.to_upper_camel_case(),
                ),
                Some(_) => uwrite!(
                    self.src,
                    "Self::{}(v) => ({i}, Some(wasm_component_layer::ComponentType::into_value(v)?)),",
                    case.name.to_upper_camel_case(),
                ),
            }
        }
        self.src.push_str("};\n");
        self.src.push_str("let variant = wasm_component_layer::Variant::new(Self::get_variant_ty().clone(), discriminant, value)?;\n");
        self.src
            .push_str("Ok(wasm_component_layer::Value::Variant(variant))\n");
        self.src.push_str("}\n");
        self.src.push_str("}\n\n");

        self.push_str("impl core::fmt::Debug for ");
        self.push_str(&rust_name);
        self.push_str("{\nfn fmt(&self, fmt: &mut core::fmt::Formatter) -> core::fmt::Result {\n");
        self.push_str("match self {\n");
        for case in variant.cases.iter() {
            self.push_str("Self::");
            self.push_str(&case.name.to_upper_camel_case());
            if case.ty.is_some() {
                self.push_str("(v)");
            }
            self.push_str(" => {\n");
            self.push_str(&format!(
                "fmt.debug_tuple(\"{}::{}\")",
                rust_name,
                case.name.to_upper_camel_case()
            ));
            if case.ty.is_some() {
                self.push_str(".field(v)");
            }
            self.push_str(".finish()\n");
            self.push_str("},\n");
        }
        self.push_str("}\n");
        self.push_str("}\n");
        self.push_str("}\n\n");

        if info.error {
            self.push_str("impl core::fmt::Display for ");
            self.push_str(&rust_name);
            self.push_str(" {\n");
            self.push_str("fn fmt(&self, fmt: &mut core::fmt::Formatter) -> core::fmt::Result {\n");
            self.push_str("write!(fmt, \"{:?}\", self)");
            self.push_str("}\n");
            self.push_str("}\n\n");

            self.push_str("impl std::error::Error for ");
            self.push_str(&rust_name);
            self.push_str(" {}\n\n");
        }
    }

    fn type_option(&mut self, id: TypeId, _name: &str, payload: &Type, docs: &Docs) {
        let info = self.info(id);

        for (name, mode) in self.modes_of(id) {
            self.rustdoc(docs);
            let lt = self.lifetime_for(&info, mode);
            self.push_str(&format!("pub type {}", name));
            self.print_generics(lt);
            self.push_str("= Option<");
            self.print_ty(payload, mode, None);
            self.push_str(">;\n");
        }
    }

    fn type_result(&mut self, id: TypeId, _name: &str, result: &Result_, docs: &Docs) {
        let info = self.info(id);

        for (name, mode) in self.modes_of(id) {
            self.rustdoc(docs);
            let lt = self.lifetime_for(&info, mode);
            self.push_str(&format!("pub type {}", name));
            self.print_generics(lt);
            self.push_str("= Result<");
            self.print_optional_ty(result.ok.as_ref(), mode, None);
            self.push_str(",");
            self.print_optional_ty(result.err.as_ref(), mode, None);
            self.push_str(">;\n");
        }
    }

    fn type_enum(
        &mut self,
        id: TypeId,
        name: &str,
        interface_name: &str,
        enum_: &Enum,
        docs: &Docs,
    ) {
        let info = self.info(id);

        // We use a BTree set to make sure we don't have any duplicates and have a stable order
        let mut derives: BTreeSet<String> = self
            .gen
            .opts
            .additional_derive_attributes
            .iter()
            .cloned()
            .collect();

        derives.extend(
            ["Clone", "Copy", "PartialEq", "Eq"]
                .into_iter()
                .map(|s| s.to_string()),
        );

        let rust_name = to_rust_upper_camel_case(name);
        self.rustdoc(docs);
        self.push_str("#[derive(");
        self.push_str(&derives.into_iter().collect::<Vec<_>>().join(", "));
        self.push_str(")]\n");

        self.push_str(&format!("pub enum {} {{\n", rust_name));
        for case in enum_.cases.iter() {
            self.rustdoc(&case.docs);
            self.push_str(&case.name.to_upper_camel_case());
            self.push_str(",\n");
        }
        self.push_str("}\n");

        self.src.push_str("impl ");
        self.src.push_str(&rust_name);
        self.src.push_str("{\n");
        self.src
            .push_str("fn get_enum_ty() -> &'static wasm_component_layer::EnumType {\n");
        self.src.push_str("static ENUM_TY: std::sync::OnceLock<wasm_component_layer::EnumType> = std::sync::OnceLock::new();\n");
        self.src.push_str("ENUM_TY.get_or_init(|| {\n");
        self.src.push_str("wasm_component_layer::EnumType::new(\n");
        self.src
            .push_str("Some(wasm_component_layer::TypeIdentifier::new(\n");
        uwrite!(
            self.src,
            "\"{name}\", Some(\"{interface_name}\".try_into().unwrap())\n",
        );
        self.src.push_str(")),\n");
        self.src.push_str("[");
        for case in enum_.cases.iter() {
            uwrite!(self.src, "\"{}\",", case.name);
        }
        self.src.push_str("],\n");
        self.src.push_str(").unwrap()\n");
        self.src.push_str("})\n");
        self.src.push_str("}\n");
        self.src.push_str("}\n\n");
        self.src
            .push_str("impl wasm_component_layer::ComponentType for ");
        self.src.push_str(&rust_name);
        self.src.push_str("{\n");
        self.src
            .push_str("fn ty() -> wasm_component_layer::ValueType {\n");
        self.src
            .push_str("wasm_component_layer::ValueType::Enum(Self::get_enum_ty().clone())\n");
        self.src.push_str("}\n\n");
        self.src.push_str(
            "fn from_value(value: &wasm_component_layer::Value) -> anyhow::Result<Self> {\n",
        );
        self.src.push_str("let enum_ = match value {\n");
        self.src
            .push_str("wasm_component_layer::Value::Enum(enum_) => enum_,\n");
        self.src
            .push_str("_ => anyhow::bail!(\"incorrect type, expected enum\"),\n");
        self.src.push_str("};\n");
        self.src.push_str(
            "anyhow::ensure!(&enum_.ty() == Self::get_enum_ty(), \"incorrect enum type\");\n",
        );
        self.src
            .push_str("let this = match enum_.discriminant() {\n");
        for (i, case) in enum_.cases.iter().enumerate() {
            uwrite!(
                self.src,
                "{i} => Self::{},\n",
                case.name.to_upper_camel_case(),
            );
        }
        self.src
            .push_str("_ => anyhow::bail!(\"unexpected enum discriminant\"),\n");
        self.src.push_str("};\n");
        self.src.push_str("Ok(this)\n");
        self.src.push_str("}\n\n");
        self.src
            .push_str("fn into_value(self) -> anyhow::Result<wasm_component_layer::Value> {\n");
        self.src.push_str("let discriminant = match self {\n");
        for (i, case) in enum_.cases.iter().enumerate() {
            uwrite!(
                self.src,
                "Self::{} => {i},\n",
                case.name.to_upper_camel_case(),
            );
        }
        self.src.push_str("};\n");
        self.src.push_str("let enum_ = wasm_component_layer::Enum::new(Self::get_enum_ty().clone(), discriminant)?;\n");
        self.src
            .push_str("Ok(wasm_component_layer::Value::Enum(enum_))\n");
        self.src.push_str("}\n");
        self.src.push_str("}\n\n");

        // Auto-synthesize an implementation of the standard `Error` trait for
        // error-looking types based on their name.
        if info.error {
            self.push_str("impl ");
            self.push_str(&rust_name);
            self.push_str("{\n");

            self.push_str("pub fn name(&self) -> &'static str {\n");
            self.push_str("match self {\n");
            for case in enum_.cases.iter() {
                self.push_str("Self::");
                self.push_str(&case.name.to_upper_camel_case());
                self.push_str(" => \"");
                self.push_str(case.name.as_str());
                self.push_str("\",\n");
            }
            self.push_str("}\n");
            self.push_str("}\n");

            self.push_str("pub fn message(&self) -> &'static str {\n");
            self.push_str("match self {\n");
            for case in enum_.cases.iter() {
                self.push_str("Self::");
                self.push_str(&case.name.to_upper_camel_case());
                self.push_str(" => \"");
                if let Some(contents) = &case.docs.contents {
                    self.push_str(contents.trim());
                }
                self.push_str("\",\n");
            }
            self.push_str("}\n");
            self.push_str("}\n");

            self.push_str("}\n");

            self.push_str("impl core::fmt::Debug for ");
            self.push_str(&rust_name);
            self.push_str(
                "{\nfn fmt(&self, fmt: &mut core::fmt::Formatter) -> core::fmt::Result {\n",
            );
            self.push_str("fmt.debug_struct(\"");
            self.push_str(name);
            self.push_str("\")\n");
            self.push_str(".field(\"code\", &(*self as i32))\n");
            self.push_str(".field(\"name\", &self.name())\n");
            self.push_str(".field(\"message\", &self.message())\n");
            self.push_str(".finish()\n");
            self.push_str("}\n");
            self.push_str("}\n");

            self.push_str("impl core::fmt::Display for ");
            self.push_str(&rust_name);
            self.push_str(
                "{\nfn fmt(&self, fmt: &mut core::fmt::Formatter) -> core::fmt::Result {\n",
            );
            self.push_str("write!(fmt, \"{} (error {})\", self.name(), *self as i32)");
            self.push_str("}\n");
            self.push_str("}\n");
            self.push_str("\n");
            self.push_str("impl std::error::Error for ");
            self.push_str(&rust_name);
            self.push_str("{}\n\n");
        } else {
            self.push_str("impl core::fmt::Debug for ");
            self.push_str(&rust_name);
            self.push_str(
                "{\nfn fmt(&self, fmt: &mut core::fmt::Formatter) -> core::fmt::Result {\n",
            );
            self.push_str("match self {\n");
            for case in enum_.cases.iter() {
                self.push_str("Self::");
                self.push_str(&case.name.to_upper_camel_case());
                self.push_str(" => {\n");
                self.push_str(&format!(
                    "fmt.debug_tuple(\"{}::{}\")",
                    rust_name,
                    case.name.to_upper_camel_case()
                ));
                self.push_str(".finish()\n");
                self.push_str("},\n");
            }
            self.push_str("}\n");
            self.push_str("}\n");
            self.push_str("}\n\n");
        }
    }

    fn type_alias(&mut self, id: TypeId, _name: &str, ty: &Type, docs: &Docs) {
        let info = self.info(id);
        for (name, mode) in self.modes_of(id) {
            self.rustdoc(docs);
            self.push_str(&format!("pub type {}", name));
            let lt = self.lifetime_for(&info, mode);
            self.print_generics(lt);
            self.push_str(" = ");
            self.print_ty(ty, mode, None);
            self.push_str(";\n");
        }
    }

    fn type_list(&mut self, id: TypeId, _name: &str, ty: &Type, docs: &Docs) {
        let info = self.info(id);
        for (name, mode) in self.modes_of(id) {
            let lt = self.lifetime_for(&info, mode);
            self.rustdoc(docs);
            self.push_str(&format!("pub type {}", name));
            self.print_generics(lt);
            self.push_str(" = ");
            self.print_list(ty, mode, None);
            self.push_str(";\n");
        }
    }

    fn print_result_ty(&mut self, results: &Results, mode: TypeMode, resource: Option<TypeId>) {
        match results {
            Results::Named(rs) => match rs.len() {
                0 => self.push_str("()"),
                1 => self.print_ty(&rs[0].1, mode, resource),
                _ => {
                    self.push_str("(");
                    for (i, (_, ty)) in rs.iter().enumerate() {
                        if i > 0 {
                            self.push_str(", ")
                        }
                        self.print_ty(ty, mode, resource)
                    }
                    self.push_str(")");
                }
            },
            Results::Anon(ty) => self.print_ty(ty, mode, resource),
        }
    }

    fn special_case_trappable_error(
        &self,
        results: &Results,
    ) -> Option<(&'a Result_, TypeId, String)> {
        // We fillin a special trappable error type in the case when a function has just one
        // result, which is itself a `result<a, e>`, and the `e` is *not* a primitive
        // (i.e. defined in std) type, and matches the typename given by the user.
        let mut i = results.iter_types();
        let id = match i.next()? {
            Type::Id(id) => id,
            _ => return None,
        };
        if i.next().is_some() {
            return None;
        }
        let result = match &self.resolve.types[*id].kind {
            TypeDefKind::Result(r) => r,
            _ => return None,
        };
        let error_typeid = match result.err? {
            Type::Id(id) => resolve_type_definition_id(self.resolve, id),
            _ => return None,
        };

        let name = self.gen.trappable_errors.get(&error_typeid)?;

        let mut path = self.path_to_root();
        uwrite!(path, "{name}");
        Some((result, error_typeid, path))
    }

    fn generate_add_to_linker(&mut self, id: InterfaceId, name: &str) {
        let iface = &self.resolve.interfaces[id];
        let owner = TypeOwner::Interface(id);

        // Generate the `pub trait` which represents the host functionality for
        // this import which additionally inherits from all resource traits
        // for this interface defined by `type_resource`.
        uwrite!(self.src, "pub trait Host {{");
        for resource in get_resources(self.resolve, id) {
            uwrite!(self.src, "type {}: Host{};\n", resource.to_upper_camel_case(), resource.to_upper_camel_case());
        }
        for (_, func) in iface.functions.iter() {
            match func.kind {
                FunctionKind::Freestanding => {}
                _ => continue,
            }
            self.generate_function_trait_sig(func, None);
        }

        // Generate `convert_*` functions to convert custom trappable errors
        // into the representation required by Wasmtime's component API.
        let mut required_conversion_traits = IndexSet::new();
        let mut errors_converted = IndexSet::new();
        let my_error_types = iface
            .types
            .iter()
            .filter(|(_, id)| self.gen.trappable_errors.contains_key(*id))
            .map(|(_, id)| *id);
        let used_error_types = iface
            .functions
            .iter()
            .filter_map(|(_, func)| self.special_case_trappable_error(&func.results))
            .map(|(_, id, _)| id);
        for err in my_error_types.chain(used_error_types).collect::<Vec<_>>() {
            let custom_name = &self.gen.trappable_errors[&err];
            let err = &self.resolve.types[resolve_type_definition_id(self.resolve, err)];
            let err_name = err.name.as_ref().unwrap();
            let err_snake = err_name.to_snake_case();
            let err_camel = err_name.to_upper_camel_case();
            let owner = match err.owner {
                TypeOwner::Interface(i) => i,
                _ => unimplemented!(),
            };
            match self.path_to_interface(owner) {
                Some(path) => {
                    required_conversion_traits.insert(format!("{path}::Host"));
                }
                None => {
                    if errors_converted.insert(err_name) {
                        let root = self.path_to_root();
                        uwriteln!(
                            self.src,
                            "fn convert_{err_snake}(&mut self, err: {root}{custom_name}) -> anyhow::Result<{err_camel}>;"
                        );
                    }
                }
            }
        }
        uwriteln!(self.src, "}}");

        let mut where_clause = String::from("U: Host");

        for t in required_conversion_traits {
            where_clause.push_str(" + ");
            where_clause.push_str(&t);
        }

        uwriteln!(
            self.src,
            "
                pub fn add_to_linker<T, U>(
                    mut ctx: impl wasm_component_layer::AsContextMut<UserState = T>,
                    linker: &mut wasm_component_layer::Linker,
                ) -> anyhow::Result<()>
                    where {where_clause}
                {{
            "
        );
        self.src.push_str("let mut ctx = ctx.as_context_mut();\n");
        uwriteln!(self.src, "let mut inst = linker.define_instance(\"{name}\".try_into().unwrap())?;");

        for name in get_resources(self.resolve, id) {
            let camel = name.to_upper_camel_case();
            
            uwriteln!(
                self.src,
                "inst.define_resource(
                    \"{name}\",
                    <U::{camel}\nas wasm_component_layer::Resource>::ty(),
                )?;"
            )
        }

        for (_, func) in iface.functions.iter() {
            self.generate_add_function_to_linker(owner, func, "inst");
        }
        uwriteln!(self.src, "Ok(())");
        uwriteln!(self.src, "}}");
    }

    fn generate_add_function_to_linker(&mut self, owner: TypeOwner, func: &Function, linker: &str) {
        uwrite!(self.src, "{linker}.define_func(\"{}\", wasm_component_layer::Func::new(&mut ctx, todo!(), ", func.name);
        self.generate_guest_import_closure(owner, func);
        uwriteln!(self.src, "))?;")
    }

    fn generate_guest_import_closure(&mut self, owner: TypeOwner, func: &Function) {
        // Generate the closure that's passed to a `Linker`, the final piece of
        // codegen here.

        self.src.push_str("|\n");
        self.src.push_str("mut ctx: wasm_component_layer::StoreContextMut<'_, T, _>,\n");
        self.src.push_str("arguments: &[wasm_component_layer::Value],\n");
        self.src.push_str("results: &mut [wasm_component_layer::Value],\n");
        self.src.push_str("| -> anyhow::Result<()> {\n");
        // for (i, _param) in func.params.iter().enumerate() {
        //     uwrite!(self.src, "arg{},", i);
        // }
        // self.src.push_str(") : (");

        // for (_, ty) in func.params.iter() {
        //     // Lift is required to be impled for this type, so we can't use
        //     // a borrowed type:
        //     self.print_ty(ty, TypeMode::Owned);
        //     self.src.push_str(", ");
        // }

        // if self.gen.opts.tracing {
        //     uwrite!(
        //         self.src,
        //         "
        //            let span = tracing::span!(
        //                tracing::Level::TRACE,
        //                \"wit-bindgen import\",
        //                module = \"{}\",
        //                function = \"{}\",
        //            );
        //            let _enter = span.enter();
        //        ",
        //         match owner {
        //             TypeOwner::Interface(id) => self.resolve.interfaces[id]
        //                 .name
        //                 .as_deref()
        //                 .unwrap_or("<no module>"),
        //             TypeOwner::World(id) => &self.resolve.worlds[id].name,
        //             TypeOwner::None => "<no owner>",
        //         },
        //         func.name,
        //     );
        //     let mut event_fields = func
        //         .params
        //         .iter()
        //         .enumerate()
        //         .map(|(i, (name, _ty))| {
        //             let name = to_rust_ident(name);
        //             format!("{name} = tracing::field::debug(&arg{i})")
        //         })
        //         .collect::<Vec<String>>();
        //     event_fields.push(String::from("\"call\""));
        //     uwrite!(
        //         self.src,
        //         "tracing::event!(tracing::Level::TRACE, {});\n",
        //         event_fields.join(", ")
        //     );
        // }

        // self.src.push_str("let host = get(caller.data_mut());\n");
        // let func_name = rust_function_name(func);
        // let host_trait = match func.kind {
        //     FunctionKind::Freestanding => match owner {
        //         TypeOwner::World(id) => format!(
        //             "{}Imports",
        //             self.resolve.worlds[id].name.to_upper_camel_case()
        //         ),
        //         _ => "Host".to_string(),
        //     },
        //     FunctionKind::Method(id) | FunctionKind::Static(id) | FunctionKind::Constructor(id) => {
        //         let resource = self.resolve.types[id]
        //             .name
        //             .as_ref()
        //             .unwrap()
        //             .to_upper_camel_case();
        //         format!("Host{resource}")
        //     }
        // };
        // uwrite!(self.src, "let r = {host_trait}::{func_name}(host, ");

        // for (i, _) in func.params.iter().enumerate() {
        //     uwrite!(self.src, "arg{},", i);
        // }
        // uwrite!(self.src, ");\n");

        // if self.gen.opts.tracing {
        //     uwrite!(
        //         self.src,
        //         "tracing::event!(tracing::Level::TRACE, result = tracing::field::debug(&r), \"return\");"
        //     );
        // }

        // if !self.gen.opts.trappable_imports.can_trap(func) {
        //     if func.results.iter_types().len() == 1 {
        //         uwrite!(self.src, "Ok((r,))\n");
        //     } else {
        //         uwrite!(self.src, "Ok(r)\n");
        //     }
        // } else if let Some((_, err, _)) = self.special_case_trappable_error(&func.results) {
        //     let err = &self.resolve.types[resolve_type_definition_id(self.resolve, err)];
        //     let err_name = err.name.as_ref().unwrap();
        //     let owner = match err.owner {
        //         TypeOwner::Interface(i) => i,
        //         _ => unimplemented!(),
        //     };
        //     let convert_trait = match self.path_to_interface(owner) {
        //         Some(path) => format!("{path}::Host"),
        //         None => String::from("Host"),
        //     };
        //     let convert = format!("{}::convert_{}", convert_trait, err_name.to_snake_case());
        //     uwrite!(
        //         self.src,
        //         "Ok((match r {{
        //             Ok(a) => Ok(a),
        //             Err(e) => Err({convert}(host, e)?),
        //         }},))"
        //     );
        // } else if func.results.iter_types().len() == 1 {
        //     uwrite!(self.src, "Ok((r?,))\n");
        // } else {
        //     uwrite!(self.src, "r\n");
        // }

        self.src.push_str("todo!()\n");
        self.src.push_str("}");
    }

    fn generate_function_trait_sig(&mut self, func: &Function, resource: Option<TypeId>) {
        self.rustdoc(&func.docs);

        self.push_str("fn ");
        self.push_str(&rust_function_name(func));
        self.push_str("(");
        if resource.is_none() {
            self.push_str("&mut self, ");
        }
        for (name, param) in func.params.iter() {
            let name = to_rust_ident(name);
            self.push_str(&name);
            self.push_str(": ");
            self.print_ty(param, TypeMode::Owned, resource);
            self.push_str(",");
        }
        self.push_str(")");
        self.push_str(" -> ");

        if !self.gen.opts.trappable_imports.can_trap(func) {
            self.print_result_ty(&func.results, TypeMode::Owned, resource);
        } else if let Some((r, _id, error_typename)) =
            self.special_case_trappable_error(&func.results)
        {
            // Functions which have a single result `result<ok,err>` get special
            // cased to use the host_wasmtime_rust::Error<err>, making it possible
            // for them to trap or use `?` to propogate their errors
            self.push_str("Result<");
            if let Some(ok) = r.ok {
                self.print_ty(&ok, TypeMode::Owned, resource);
            } else {
                self.push_str("()");
            }
            self.push_str(",");
            self.push_str(&error_typename);
            self.push_str(">");
        } else {
            // All other functions get their return values wrapped in an anyhow::Result.
            // Returning the anyhow::Error case can be used to trap.
            self.push_str("anyhow::Result<");
            self.print_result_ty(&func.results, TypeMode::Owned, resource);
            self.push_str(">");
        }

        self.push_str(";\n");
    }

    fn extract_typed_function(&mut self, func: &Function) -> (String, String) {
        let prev = mem::take(&mut self.src);
        let snake = func_field_name(self.resolve, func);
        uwrite!(self.src, "__exports.func(\"");
        self.src.push_str(&func.name);
        self.src
            .push_str("\").ok_or_else(|| anyhow::anyhow!(\"exported function `");
        self.src.push_str(&func.name);
        self.src.push_str("` not present\"))?.typed()?");

        // uwrite!(self.src, "*__exports.typed_func::<(");
        // for (_, ty) in func.params.iter() {
        //     self.print_ty(ty, TypeMode::AllBorrowed("'_"));
        //     self.push_str(", ");
        // }
        // self.src.push_str("), (");
        // for ty in func.results.iter_types() {
        //     self.print_ty(ty, TypeMode::Owned);
        //     self.push_str(", ");
        // }
        // self.src.push_str(")>(\"");
        // self.src.push_str(&func.name);
        // self.src.push_str("\")?.func()");

        let ret = (snake, mem::take(&mut self.src).to_string());
        self.src = prev;
        ret
    }

    fn define_rust_guest_export(
        &mut self,
        resolve: &Resolve,
        ns: Option<&WorldKey>,
        func: &Function,
    ) {
        self.rustdoc(&func.docs);

        uwrite!(
            self.src,
            "pub fn call_{}<S: wasm_component_layer::AsContextMut>(&self, mut store: S, ",
            func.item_name().to_snake_case(),
        );

        for (i, param) in func.params.iter().enumerate() {
            uwrite!(self.src, "arg{}: ", i);
            self.print_ty(&param.1, TypeMode::Owned, None);
            self.push_str(",");
        }

        self.src.push_str(") -> anyhow::Result<");
        self.print_result_ty(&func.results, TypeMode::Owned, None);

        self.src.push_str("> {\n");

        if self.gen.opts.tracing {
            let ns = match ns {
                Some(key) => resolve.name_world_key(key),
                None => "default".to_string(),
            };
            self.src.push_str(&format!(
                "
                   let span = tracing::span!(
                       tracing::Level::TRACE,
                       \"wit-bindgen export\",
                       module = \"{ns}\",
                       function = \"{}\",
                   );
                   let _enter = span.enter();
               ",
                func.name,
            ));
        }

        let projection_to_func = match &func.kind {
            FunctionKind::Freestanding => "",
            _ => ".funcs",
        };
        self.src.push_str("let (");
        for (i, _) in func.results.iter_types().enumerate() {
            uwrite!(self.src, "ret{},", i);
        }
        uwriteln!(
            self.src,
            ") = self{projection_to_func}.{}.call(store.as_context_mut(), (",
            func_field_name(self.resolve, func),
        );
        for (i, _) in func.params.iter().enumerate() {
            uwrite!(self.src, "arg{}, ", i);
        }
        uwriteln!(self.src, "))?;");

        self.src.push_str("Ok(");
        if func.results.iter_types().len() == 1 {
            self.src.push_str("ret0");
        } else {
            self.src.push_str("(");
            for (i, _) in func.results.iter_types().enumerate() {
                uwrite!(self.src, "ret{},", i);
            }
            self.src.push_str(")");
        }
        self.src.push_str(")\n");

        // End function body
        self.src.push_str("}\n");
    }

    fn rustdoc(&mut self, docs: &Docs) {
        let docs = match &docs.contents {
            Some(docs) => docs,
            None => return,
        };
        for line in docs.trim().lines() {
            self.push_str("/// ");
            self.push_str(line);
            self.push_str("\n");
        }
    }

    fn path_to_root(&self) -> String {
        let mut path_to_root = String::new();
        if let Some((_, key, is_export)) = self.current_interface {
            match key {
                WorldKey::Name(_) => {
                    path_to_root.push_str("super::");
                }
                WorldKey::Interface(_) => {
                    path_to_root.push_str("super::super::super::");
                }
            }
            if is_export {
                path_to_root.push_str("super::");
            }
        }
        path_to_root
    }
}

impl<'a> RustGenerator<'a> for InterfaceGenerator<'a> {
    fn resolve(&self) -> &'a Resolve {
        self.resolve
    }

    fn ownership(&self) -> Ownership {
        Ownership::Owning
    }

    fn path_to_interface(&self, interface: InterfaceId) -> Option<String> {
        if let Some((cur, _, _)) = self.current_interface {
            if cur == interface {
                return None;
            }
        }
        let mut path_to_root = self.path_to_root();
        match &self.gen.interface_names[&interface] {
            InterfaceName::Remapped { name_at_root, .. } => path_to_root.push_str(name_at_root),
            InterfaceName::Path(path) => {
                for (i, name) in path.iter().enumerate() {
                    if i > 0 {
                        path_to_root.push_str("::");
                    }
                    path_to_root.push_str(name);
                }
            }
        }
        Some(path_to_root)
    }

    fn push_str(&mut self, s: &str) {
        self.src.push_str(s);
    }

    fn info(&self, ty: TypeId) -> TypeInfo {
        self.gen.types.get(ty)
    }

    fn is_imported_interface(&self, interface: InterfaceId) -> bool {
        self.gen.interface_last_seen_as_import[&interface]
    }
}

/// When an interface `use`s a type from another interface, it creates a new TypeId
/// referring to the definition TypeId. Chase this chain of references down to
/// a TypeId for type's definition.
fn resolve_type_definition_id(resolve: &Resolve, mut id: TypeId) -> TypeId {
    loop {
        match resolve.types[id].kind {
            TypeDefKind::Type(Type::Id(def_id)) => id = def_id,
            _ => return id,
        }
    }
}

fn rust_function_name(func: &Function) -> String {
    match func.kind {
        FunctionKind::Method(_) | FunctionKind::Static(_) => to_rust_ident(func.item_name()),
        FunctionKind::Constructor(_) => "new".to_string(),
        FunctionKind::Freestanding => to_rust_ident(&func.name),
    }
}

fn func_field_name(resolve: &Resolve, func: &Function) -> String {
    let mut name = String::new();
    match func.kind {
        FunctionKind::Method(id) => {
            name.push_str("method-");
            name.push_str(resolve.types[id].name.as_ref().unwrap());
            name.push('-');
        }
        FunctionKind::Static(id) => {
            name.push_str("static-");
            name.push_str(resolve.types[id].name.as_ref().unwrap());
            name.push('-');
        }
        FunctionKind::Constructor(id) => {
            name.push_str("constructor-");
            name.push_str(resolve.types[id].name.as_ref().unwrap());
            name.push('-');
        }
        FunctionKind::Freestanding => {}
    }
    name.push_str(func.item_name());
    name.to_snake_case()
}

fn get_resources(resolve: &Resolve, id: InterfaceId) -> impl Iterator<Item = &'_ str> + '_ {
    resolve.interfaces[id]
        .types
        .iter()
        .filter_map(move |(name, ty)| match resolve.types[*ty].kind {
            TypeDefKind::Resource => Some(name.as_str()),
            _ => None,
        })
}

fn get_world_resources(resolve: &Resolve, id: WorldId) -> impl Iterator<Item = &'_ str> + '_ {
    resolve.worlds[id]
        .imports
        .iter()
        .filter_map(move |(name, item)| match item {
            WorldItem::Type(id) => match resolve.types[*id].kind {
                TypeDefKind::Resource => Some(match name {
                    WorldKey::Name(s) => s.as_str(),
                    WorldKey::Interface(_) => unreachable!(),
                }),
                _ => None,
            },
            _ => None,
        })
}
