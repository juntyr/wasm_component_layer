use syn::{parse_macro_input, Error};

mod bindgen;
mod flags;

#[proc_macro]
pub fn bindgen(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    bindgen::expand(&parse_macro_input!(input as bindgen::Config))
        .unwrap_or_else(Error::into_compile_error)
        .into()
}

#[proc_macro]
pub fn flags(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    flags::expand_flags(&parse_macro_input!(input as flags::Flags))
        .unwrap_or_else(Error::into_compile_error)
        .into()
}
