use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::parse_macro_input;

struct FieldData<'a> {
    pub ident: &'a syn::Ident,
    pub ty: &'a syn::Type,
    pub is_optional: bool,
}

impl<'a> FieldData<'a> {
    pub fn original_type(&self) -> TokenStream2 {
        let ty = self.ty;
        if self.is_optional {
            quote!(Option<#ty>)
        } else {
            quote!(ty)
        }
    }
}

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);
    eprintln!("INPUT: {:#?}", input);
    let struct_name = input.ident;
    let builder_name = format!("{}Builder", struct_name);
    let builder_name = syn::Ident::new(&builder_name, struct_name.span());
    let data = match input.data {
        syn::Data::Struct(ref ds) => ds,
        _ => {
            panic!("{} has data which is not struct.", struct_name);
        }
    };
    // let fields = data.fields.iter().map(|f| match option_field_type(f) {
    //     Some(t) => {
    //         let mut f = f.clone();
    //         f.ty = t.clone();
    //         (f, true)
    //     }
    //     None => (f.clone(), false),
    // });
    let fields = data.fields.iter().filter_map(|f| {
        f.ident.as_ref().map(|ref ident| {
            let ty = option_field_type(f);
            let is_optional = ty.is_some();
            let ty = ty.unwrap_or(&f.ty);
            FieldData {
                ident,
                ty,
                is_optional,
            }
        })
    });

    let builder_function = {
        let builder_inits = fields.clone().map(|f| {
            let ident = f.ident;
            quote!(
                #ident: None,
            )
        });
        quote! {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#builder_inits)*
                }
            }
        }
    };

    let fields_declaration = fields.clone().map(|f| {
        let ident = f.ident;
        let ty = f.ty;
        quote!(
            #ident: Option<#ty>,
        )
    });
    let fields_declaration = quote!(#(#fields_declaration)*);

    let setters = fields.clone().map(|f| {
        let ident = f.ident;
        let ty = f.ty;
        quote!(
            fn #ident(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = Some(#ident);
                self
            }
        )
    });
    let setters = quote!(#(#setters)*);

    let build_function = {
        let lets_required = |chain| {
            fields.clone().filter(|f| !f.is_optional).map(move |f| {
                let ident = f.ident;
                let ident_str = ident.to_string();
                quote!(
                    let #ident = match self.#ident {
                        Some(ref x) => x #chain,
                        None => {
                            return Err(concat!("field ", #ident_str, " is not set.", ).into())
                        }
                    };
                )
            })
        };
        let lets_optional = |chain| {
            fields.clone().filter(|f| f.is_optional).map(move |f| {
                let ident = f.ident;
                quote!(let #ident = self.#ident #chain;)
            })
        };
        let lets_required_cloned = lets_required(quote!(.clone()));
        let lets_optional_cloned = lets_optional(quote!(.clone()));
        let lets_required_moved = lets_required(quote!(.to_owned()));
        let lets_optional_moved = lets_optional(quote!(.to_owned()));
        let sets = fields
            .clone()
            .map(|f| {
                let ident = f.ident;
                quote!(#ident,)
            })
            .collect::<Vec<TokenStream2>>();
        quote!(
            pub fn build(&mut self) -> Result<#struct_name, Box<dyn std::error::Error>> {
                #(#lets_required_cloned)*
                #(#lets_optional_cloned)*
                Ok( #struct_name { #(#sets)* } )
            }
            pub fn build_once(self) -> Result<#struct_name, Box<dyn std::error::Error>> {
                #(#lets_required_moved)*
                #(#lets_optional_moved)*
                Ok( #struct_name { #(#sets)* } )
            }
        )
    };

    let output = quote! {
        impl #struct_name {
            #builder_function
        }
        pub struct #builder_name {
            #fields_declaration
        }
        impl #builder_name {
            #setters
            #build_function
        }
    };
    let output = output.into();
    eprintln!("OUTPUT: {:#}", output);
    output
}

fn option_field_type(f: &syn::Field) -> Option<&syn::Type> {
    if let syn::Type::Path(ref x) = f.ty {
        if x.path.segments.len() > 0 && x.path.segments[0].ident == "Option" {
            let ref x = x.path.segments[0].arguments;
            if let syn::PathArguments::AngleBracketed(ref x) = x {
                if x.args.len() > 0 {
                    if let syn::GenericArgument::Type(ref x) = x.args[0] {
                        return Some(x);
                    }
                }
            }
        }
    }
    return None;
}
