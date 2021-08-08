use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);
    eprintln!("INPUT: {:#?}", input);
    let struct_name = input.ident;
    let builder_name = format!("{}Builder", struct_name);
    let builder_name = syn::Ident::new(&builder_name, struct_name.span());
    let (field_names, field_types) = match input.data {
        syn::Data::Struct(ref ds) => {
            let fields = ds
                .fields
                .iter()
                .filter_map(|field| field.ident.as_ref().map(|ident| (ident, &field.ty)));
            (
                fields.clone().map(|(x, _)| x).collect::<Vec<&syn::Ident>>(),
                fields.map(|(_, x)| x).collect::<Vec<&syn::Type>>(),
            )
        }
        _ => {
            panic!("{} has data which is not struct.", struct_name);
        }
    };
    let field_names_str: Vec<String> = field_names.iter().map(|x| x.to_string()).collect();

    let output = quote! {
        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name { #(#field_names: None),* }
            }
        }
        pub struct #builder_name {
            #(
                #field_names: Option<#field_types>
            ),*
        }
        impl #builder_name {
            #(
                fn #field_names(&mut self, #field_names: #field_types) -> &mut Self {
                    self.#field_names = Some(#field_names);
                    self
                }
            )*
            pub fn build(&mut self) -> Result<#struct_name, Box<dyn std::error::Error>> {
                #(
                    let #field_names = match self.#field_names {
                        Some(ref x) => x.clone(),
                        None => return Err(concat!("field", #field_names_str, "is None.").into()),
                    };
                )*
                Ok(#struct_name { #(#field_names),* })
            }
            pub fn build_once(self) -> Result<#struct_name, Box<dyn std::error::Error>> {
                #(
                    let #field_names = match self.#field_names {
                        Some(x) => x,
                        None => return Err(concat!("field", #field_names_str, "is None.").into()),
                    };
                )*
                Ok(#struct_name { #(#field_names),* })
            }
        }
    };
    let output = output.into();
    eprintln!("OUTPUT: {:#}", output);
    output
}
