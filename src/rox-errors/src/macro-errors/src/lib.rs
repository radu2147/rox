extern crate proc_macro;
extern crate syn;
#[macro_use]
extern crate quote;

use syn::{parse_macro_input, DeriveInput};

use proc_macro::TokenStream;

#[proc_macro_derive(InterpreterError)]
pub fn error(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let name = &ast.ident;
    let gen = quote! {
        impl InterpreterError for #name {
            fn error(&self) -> String {
                stringify!(#name).to_string()
            }
        }
    };

    // Return the generated impl
    gen.into()
}
