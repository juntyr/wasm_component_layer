use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::parse::{Parse, ParseStream};
use syn::{braced, Error, Result, Token};

mod kw {
    syn::custom_keyword!(flags);
    syn::custom_keyword!(name);
}

fn find_rename(attributes: &[syn::Attribute]) -> Result<Option<syn::LitStr>> {
    let mut name = None;

    for attribute in attributes {
        if !attribute.path().is_ident("component") {
            continue;
        }
        let name_literal = attribute.parse_args_with(|parser: ParseStream<'_>| {
            parser.parse::<kw::name>()?;
            parser.parse::<Token![=]>()?;
            parser.parse::<syn::LitStr>()
        })?;

        if name.is_some() {
            return Err(Error::new_spanned(
                attribute,
                "duplicate field rename attribute",
            ));
        }

        name = Some(name_literal);
    }

    Ok(name)
}

#[derive(Debug)]
struct Flag {
    rename: Option<String>,
    name: String,
}

impl Parse for Flag {
    fn parse(input: ParseStream) -> Result<Self> {
        let attributes = syn::Attribute::parse_outer(input)?;

        let rename = find_rename(&attributes)?.map(|literal| literal.value());

        input.parse::<Token![const]>()?;
        let name = input.parse::<syn::Ident>()?.to_string();

        Ok(Self { rename, name })
    }
}

#[derive(Debug)]
pub struct Flags {
    name: String,
    flags: Vec<Flag>,
}

impl Parse for Flags {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse::<syn::Ident>()?.to_string();

        let content;
        braced!(content in input);

        let flags = content
            .parse_terminated(Flag::parse, Token![;])?
            .into_iter()
            .collect();

        Ok(Self { name, flags })
    }
}

pub fn expand_flags(flags: &Flags) -> Result<TokenStream> {
    let size = FlagsSize::from_count(flags.flags.len());

    let ty;
    let eq;

    let count = flags.flags.len();

    match size {
        FlagsSize::Size0 => {
            ty = quote!(());
            eq = quote!(true);
        }
        FlagsSize::Size1 => {
            ty = quote!(u8);

            eq = if count == 8 {
                quote!(self.__inner0.eq(&rhs.__inner0))
            } else {
                let mask = !(0xFF_u8 << count);

                quote!((self.__inner0 & #mask).eq(&(rhs.__inner0 & #mask)))
            };
        }
        FlagsSize::Size2 => {
            ty = quote!(u16);

            eq = if count == 16 {
                quote!(self.__inner0.eq(&rhs.__inner0))
            } else {
                let mask = !(0xFFFF_u16 << count);

                quote!((self.__inner0 & #mask).eq(&(rhs.__inner0 & #mask)))
            };
        }
        FlagsSize::Size4Plus(n) => {
            ty = quote!(u32);

            let comparisons = (0..(n - 1))
                .map(|index| {
                    let field = format_ident!("__inner{}", index);

                    quote!(self.#field.eq(&rhs.#field) &&)
                })
                .collect::<TokenStream>();

            let field = format_ident!("__inner{}", n - 1);

            eq = if count % 32 == 0 {
                quote!(#comparisons self.#field.eq(&rhs.#field))
            } else {
                let mask = !(0xFFFF_FFFF_u32 << (count % 32));

                quote!(#comparisons (self.#field & #mask).eq(&(rhs.#field & #mask)))
            }
        }
    }

    let count;
    let mut as_array;
    let mut bitor;
    let mut bitor_assign;
    let mut bitand;
    let mut bitand_assign;
    let mut bitxor;
    let mut bitxor_assign;
    let mut not;

    match size {
        FlagsSize::Size0 => {
            count = 0;
            as_array = quote!([]);
            bitor = quote!(Self {});
            bitor_assign = quote!();
            bitand = quote!(Self {});
            bitand_assign = quote!();
            bitxor = quote!(Self {});
            bitxor_assign = quote!();
            not = quote!(Self {});
        }
        FlagsSize::Size1 | FlagsSize::Size2 => {
            count = 1;
            as_array = quote!([self.__inner0 as u32]);
            bitor = quote!(Self {
                __inner0: self.__inner0.bitor(rhs.__inner0)
            });
            bitor_assign = quote!(self.__inner0.bitor_assign(rhs.__inner0));
            bitand = quote!(Self {
                __inner0: self.__inner0.bitand(rhs.__inner0)
            });
            bitand_assign = quote!(self.__inner0.bitand_assign(rhs.__inner0));
            bitxor = quote!(Self {
                __inner0: self.__inner0.bitxor(rhs.__inner0)
            });
            bitxor_assign = quote!(self.__inner0.bitxor_assign(rhs.__inner0));
            not = quote!(Self {
                __inner0: self.__inner0.not()
            });
        }
        FlagsSize::Size4Plus(n) => {
            count = usize::from(n);
            as_array = TokenStream::new();
            bitor = TokenStream::new();
            bitor_assign = TokenStream::new();
            bitand = TokenStream::new();
            bitand_assign = TokenStream::new();
            bitxor = TokenStream::new();
            bitxor_assign = TokenStream::new();
            not = TokenStream::new();

            for index in 0..n {
                let field = format_ident!("__inner{}", index);

                as_array.extend(quote!(self.#field,));
                bitor.extend(quote!(#field: self.#field.bitor(rhs.#field),));
                bitor_assign.extend(quote!(self.#field.bitor_assign(rhs.#field);));
                bitand.extend(quote!(#field: self.#field.bitand(rhs.#field),));
                bitand_assign.extend(quote!(self.#field.bitand_assign(rhs.#field);));
                bitxor.extend(quote!(#field: self.#field.bitxor(rhs.#field),));
                bitxor_assign.extend(quote!(self.#field.bitxor_assign(rhs.#field);));
                not.extend(quote!(#field: self.#field.not(),));
            }

            as_array = quote!([#as_array]);
            bitor = quote!(Self { #bitor });
            bitand = quote!(Self { #bitand });
            bitxor = quote!(Self { #bitxor });
            not = quote!(Self { #not });
        }
    };

    let name = format_ident!("{}", flags.name);

    let mut constants = TokenStream::new();
    let mut rust_names = TokenStream::new();
    let mut component_names = TokenStream::new();

    for (index, Flag { name, rename }) in flags.flags.iter().enumerate() {
        rust_names.extend(quote!(#name,));

        let component_name = rename.as_ref().unwrap_or(name);
        component_names.extend(quote!(#component_name,));

        let fields = match size {
            FlagsSize::Size0 => quote!(),
            FlagsSize::Size1 => {
                let init = 1_u8 << index;
                quote!(__inner0: #init)
            }
            FlagsSize::Size2 => {
                let init = 1_u16 << index;
                quote!(__inner0: #init)
            }
            FlagsSize::Size4Plus(n) => (0..n)
                .map(|i| {
                    let field = format_ident!("__inner{}", i);

                    let init = if index / 32 == usize::from(i) {
                        1_u32 << (index % 32)
                    } else {
                        0
                    };

                    quote!(#field: #init,)
                })
                .collect::<TokenStream>(),
        };

        let name = format_ident!("{}", name);

        constants.extend(quote!(pub const #name: Self = Self { #fields };));
    }

    let fields = {
        let ty = syn::parse2::<syn::Type>(ty.clone())?;

        (0..count)
            .map(|index| syn::Field {
                attrs: Vec::new(),
                vis: syn::Visibility::Inherited,
                ident: Some(format_ident!("__inner{}", index)),
                colon_token: None,
                ty: ty.clone(),
                mutability: syn::FieldMutability::None,
            })
            .collect::<Vec<_>>()
    };

    let fields = fields.iter().collect::<Vec<_>>();

    let fields = fields
        .iter()
        .map(|syn::Field { ident, .. }| quote!(#[doc(hidden)] #ident: #ty,))
        .collect::<TokenStream>();

    let expanded = quote! {
        #[derive(Copy, Clone, Default)]
        pub struct #name { #fields }

        impl #name {
            #constants

            pub fn as_array(&self) -> [u32; #count] {
                #as_array
            }

            pub fn empty() -> Self {
                Self::default()
            }

            pub fn all() -> Self {
                use std::ops::Not;
                Self::default().not()
            }

            pub fn contains(&self, other: Self) -> bool {
                *self & other == other
            }

            pub fn intersects(&self, other: Self) -> bool {
                *self & other != Self::empty()
            }
        }

        impl std::cmp::PartialEq for #name {
            fn eq(&self, rhs: &#name) -> bool {
                #eq
            }
        }

        impl std::cmp::Eq for #name { }

        impl std::fmt::Debug for #name {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                wasm_component_layer::__internal::format_flags(&self.as_array(), &[#rust_names], f)
            }
        }

        impl std::ops::BitOr for #name {
            type Output = #name;

            fn bitor(self, rhs: #name) -> #name {
                #bitor
            }
        }

        impl std::ops::BitOrAssign for #name {
            fn bitor_assign(&mut self, rhs: #name) {
                #bitor_assign
            }
        }

        impl std::ops::BitAnd for #name {
            type Output = #name;

            fn bitand(self, rhs: #name) -> #name {
                #bitand
            }
        }

        impl std::ops::BitAndAssign for #name {
            fn bitand_assign(&mut self, rhs: #name) {
                #bitand_assign
            }
        }

        impl std::ops::BitXor for #name {
            type Output = #name;

            fn bitxor(self, rhs: #name) -> #name {
                #bitxor
            }
        }

        impl std::ops::BitXorAssign for #name {
            fn bitxor_assign(&mut self, rhs: #name) {
                #bitxor_assign
            }
        }

        impl std::ops::Not for #name {
            type Output = #name;

            fn not(self) -> #name {
                #not
            }
        }
    };

    Ok(expanded)
}

/// Represents the number of bytes required to store a flags value in the component model
pub enum FlagsSize {
    /// There are no flags
    Size0,
    /// Flags can fit in a u8
    Size1,
    /// Flags can fit in a u16
    Size2,
    /// Flags can fit in a specified number of u32 fields
    Size4Plus(u8),
}

impl FlagsSize {
    /// Calculate the size needed to represent a value with the specified number of flags.
    pub const fn from_count(count: usize) -> FlagsSize {
        if count == 0 {
            FlagsSize::Size0
        } else if count <= 8 {
            FlagsSize::Size1
        } else if count <= 16 {
            FlagsSize::Size2
        } else {
            let amt = ceiling_divide(count, 32);
            if amt > (u8::MAX as usize) {
                panic!("too many flags");
            }
            FlagsSize::Size4Plus(amt as u8)
        }
    }
}

/// Divide `n` by `d`, rounding up in the case of a non-zero remainder.
const fn ceiling_divide(n: usize, d: usize) -> usize {
    (n + d - 1) / d
}
