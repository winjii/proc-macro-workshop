use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::parse_macro_input;

#[derive(Debug)]
enum FieldKind {
    Plain,
    Option,
    Vec { each: syn::Ident },
}
impl FieldKind {
    fn is_optional(&self) -> bool {
        match self {
            FieldKind::Option => true,
            FieldKind::Vec { .. } => true,
            FieldKind::Plain => false,
        }
    }
}

#[derive(Debug)]
struct FieldData<'a> {
    pub ident: &'a syn::Ident,
    pub ty: &'a syn::Type,
    pub kind: FieldKind,
}

impl<'a> FieldData<'a> {
    fn type_in_builder(&self) -> TokenStream2 {
        let ty = self.ty;
        match &self.kind {
            FieldKind::Plain => quote!(Option<#ty>),
            FieldKind::Option => quote!(Option<#ty>),
            FieldKind::Vec { .. } => quote!(Vec<#ty>),
        }
    }
    fn type_set(&self) -> TokenStream2 {
        let ty = self.ty;
        match &self.kind {
            FieldKind::Plain => quote!(#ty),
            FieldKind::Option => quote!(#ty),
            FieldKind::Vec { .. } => quote!(Vec<#ty>),
        }
    }
    fn default_value(&self) -> TokenStream2 {
        let ty = self.ty;
        match &self.kind {
            FieldKind::Plain => quote!(None),
            FieldKind::Option => quote!(None),
            FieldKind::Vec { .. } => quote!(Vec::<#ty>::new()),
        }
    }
    fn convert_setter_value<T: quote::ToTokens>(&self, value: &T) -> TokenStream2 {
        match &self.kind {
            FieldKind::Plain => quote!(Some(#value)),
            FieldKind::Option => quote!(Some(#value)),
            FieldKind::Vec { .. } => quote!(#value),
        }
    }
    fn from(f: &'a syn::Field) -> Option<FieldData> {
        let ident = match f.ident {
            Some(ref x) => x,
            None => return None,
        };

        let meta = f
            .attrs
            .iter()
            .filter_map(|a| a.parse_meta().ok())
            .collect::<Vec<syn::Meta>>();
        let get_each = |m: &syn::Meta| {
            match m {
                syn::Meta::List(ref p) => Some(p),
                _ => None,
            }
            .filter(|m| {
                m.path
                    .segments
                    .first()
                    .filter(|seg| seg.ident.to_string() == "builder")
                    .is_some()
            })
            .and_then(|m| m.nested.first())
            .and_then(|m| match m {
                syn::NestedMeta::Meta(ref m) => Some(m),
                _ => None,
            })
            .and_then(|ref m| match m {
                syn::Meta::NameValue(ref x) => Some(x),
                _ => None,
            })
            .filter(|nv| {
                nv.path
                    .segments
                    .first()
                    .filter(|s| s.ident.to_string() == "each")
                    .is_some()
            })
            .and_then(|nv: &syn::MetaNameValue| match nv.lit {
                syn::Lit::Str(ref s) => Some(syn::Ident::new(&s.value(), s.span())),
                _ => None,
            })
        };
        let each: Option<syn::Ident> = meta.iter().filter_map(|m| get_each(m)).next();

        let kind = match f.ty {
            syn::Type::Path(ref x) => x.path.segments.first(),
            _ => None,
        }
        .and_then(|s| match &s.ident.to_string()[..] {
            "Option" => Some(FieldKind::Option),
            "Vec" => each.map(|each| FieldKind::Vec { each }),
            _ => None,
        })
        .unwrap_or(FieldKind::Plain);

        let get_generic_arg = || {
            match f.ty {
                syn::Type::Path(ref x) => x.path.segments.first(),
                _ => None,
            }
            .and_then(|ps| match ps.arguments {
                syn::PathArguments::AngleBracketed(ref x) => Some(x),
                _ => None,
            })
            .and_then(|x| {
                x.args.first().and_then(|x| match x {
                    syn::GenericArgument::Type(ref x) => Some(x),
                    _ => None,
                })
            })
        };

        let ty = match kind {
            FieldKind::Plain => Some(&f.ty),
            FieldKind::Option => get_generic_arg(),
            FieldKind::Vec { .. } => get_generic_arg(),
        };

        return ty.map(|ty| FieldData { ident, ty, kind });
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as syn::DeriveInput);
    // eprintln!("INPUT: {:#?}", input);
    let struct_name = input.ident;
    let builder_name = format!("{}Builder", struct_name);
    let builder_name = syn::Ident::new(&builder_name, struct_name.span());
    let data = match input.data {
        syn::Data::Struct(ref ds) => ds,
        _ => {
            panic!("{} has data which is not struct.", struct_name);
        }
    };
    let fields: Vec<FieldData> = data
        .fields
        .iter()
        .filter_map(|f| FieldData::from(f))
        .collect();

    // eprintln!("{:#?}", fields);

    let builder_function = {
        let builder_inits = fields.iter().map(|f| {
            let ident = f.ident;
            let default = f.default_value();
            quote!(
                #ident: #default,
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

    let fields_declaration = fields.iter().map(|f| {
        let ident = f.ident;
        let ty = f.type_in_builder();
        quote!(
            #ident: #ty,
        )
    });
    let fields_declaration = quote!(#(#fields_declaration)*);

    let setters = fields
        .iter()
        .filter(|f| match &f.kind {
            FieldKind::Plain => true,
            FieldKind::Option => true,
            FieldKind::Vec { each } => return f.ident.to_string() != each.to_string(),
        })
        .map(|f| {
            let ident = f.ident;
            let ty = f.type_set();
            let value = f.convert_setter_value(ident);
            quote!(
                fn #ident(&mut self, #ident: #ty) -> &mut Self {
                    self.#ident = #value;
                    self
                }
            )
        });
    let vec_item_settters = fields.iter().filter_map(|f| match &f.kind {
        FieldKind::Vec { each } => {
            let ident = f.ident;
            let ty = f.ty;
            Some(quote!(
                fn #each(&mut self, item: #ty) -> &mut Self {
                    self.#ident.push(item);
                    self
                }
            ))
        }
        _ => None,
    });
    let setters = quote!(#(#setters)* #(#vec_item_settters)*);

    let build_function = {
        let lets_required =
            |ref_op, chain| {
                fields.iter().filter(|f| !f.kind.is_optional()).map(move |f| {
                let ident = f.ident;
                let ident_str = ident.to_string();
                quote!(
                    let #ident = match #ref_op self.#ident {
                        Some(x) => x #chain,
                        None => {
                            return Err(concat!("field ", #ident_str, " is not set.", ).into())
                        }
                    };
                )
            })
            };
        let lets_optional = |chain| {
            fields
                .iter()
                .filter(|f| f.kind.is_optional())
                .map(move |f| {
                    let ident = f.ident;
                    quote!(let #ident = self.#ident #chain;)
                })
        };
        let lets_required_cloned = lets_required(quote!(&), quote!(.clone()));
        let lets_optional_cloned = lets_optional(quote!(.clone()));
        let lets_required_moved = lets_required(quote!(), quote!());
        let lets_optional_moved = lets_optional(quote!());
        let sets = fields
            .iter()
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
