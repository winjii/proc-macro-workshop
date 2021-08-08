use proc_macro::TokenStream;
use syn;
use syn::{parse_macro_input};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);
    eprintln!("INPUT: {:#?}", input);
    TokenStream::new()
}
