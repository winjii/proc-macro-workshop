use insideout::InsideOut;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned};

#[allow(dead_code)]
type IDependsOnPathOfOption<T> = std::option::Option<T>;
#[allow(dead_code)]
type IDependsOnPathOfVec<T> = std::vec::Vec<T>;
#[allow(dead_code)]
type IDependsOnPathOfResult<T, E> = std::result::Result<T, E>;
#[allow(dead_code)]
type IDependsOnPathOfBox<T> = std::boxed::Box<T>;

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
            FieldKind::Plain => quote!(std::option::Option<#ty>),
            FieldKind::Option => quote!(std::option::Option<#ty>),
            FieldKind::Vec { .. } => quote!(std::vec::Vec<#ty>),
        }
    }
    fn type_set(&self) -> TokenStream2 {
        let ty = self.ty;
        match &self.kind {
            FieldKind::Plain => quote!(#ty),
            FieldKind::Option => quote!(#ty),
            FieldKind::Vec { .. } => quote!(std::vec::Vec<#ty>),
        }
    }
    fn default_value(&self) -> TokenStream2 {
        let ty = self.ty;
        match &self.kind {
            FieldKind::Plain => quote!(std::option::Option::None),
            FieldKind::Option => quote!(std::option::Option::None),
            FieldKind::Vec { .. } => quote!(std::vec::Vec::<#ty>::new()),
        }
    }
    fn convert_setter_value<T: quote::ToTokens>(&self, value: &T) -> TokenStream2 {
        match &self.kind {
            FieldKind::Plain => quote!(std::option::Option::Some(#value)),
            FieldKind::Option => quote!(std::option::Option::Some(#value)),
            FieldKind::Vec { .. } => quote!(#value),
        }
    }
    fn from(f: &'a syn::Field) -> Result<FieldData, syn::Error> {
        let ident = f
            .ident
            .as_ref()
            .ok_or(syn::Error::new(f.ident.span(), "invalid identifier"))?;

        let meta = f.attrs.iter().map(|a| a.parse_meta());
        let get_each = |m: &syn::Meta| -> Result<syn::Ident, syn::Error> {
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
            .ok_or(syn::Error::new_spanned(
                m,
                "expected `builder(each = \"...\")`",
            ))
        };
        let each: Option<syn::Ident> = meta
            .map(|m| m.and_then(|m| get_each(&m)))
            .next()
            .inside_out()?;

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
            FieldKind::Plain => Ok(&f.ty),
            FieldKind::Option => get_generic_arg().ok_or(syn::Error::new(
                f.ty.span(),
                "cannot parse generic arguments of Option<>",
            )),
            FieldKind::Vec { .. } => get_generic_arg().ok_or(syn::Error::new(
                f.ty.span(),
                "cannot parse generic arguments of Vec<>",
            )),
        }?;

        Ok(FieldData {
            ident: &ident,
            ty,
            kind,
        })
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
    let fields = {
        let mut fields: Vec<FieldData> = Vec::new();
        data.fields
            .iter()
            .map(|f| FieldData::from(f))
            .try_for_each(|field| {
                field.map(|f| {
                    fields.push(f);
                })
            })
            .map(|_| fields)
    };
    let fields = match fields {
        Ok(fields) => fields,
        Err(e) => {
            return e.to_compile_error().into();
        }
    };

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
        let lets_required = |ref_op, chain| {
            fields.iter().filter(|f| !f.kind.is_optional()).map(move |f| {
                let ident = f.ident;
                let ident_str = ident.to_string();
                quote!(
                    let #ident = match #ref_op self.#ident {
                        std::option::Option::Some(x) => x #chain,
                        std::option::Option::None => {
                            return std::result::Result::Err(concat!("field ", #ident_str, " is not set.", ).into())
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
            pub fn build(&mut self) -> std::result::Result<#struct_name, std::boxed::Box<dyn std::error::Error>> {
                #(#lets_required_cloned)*
                #(#lets_optional_cloned)*
                std::result::Result::Ok( #struct_name { #(#sets)* } )
            }
            pub fn build_once(self) -> std::result::Result<#struct_name, std::boxed::Box<dyn std::error::Error>> {
                #(#lets_required_moved)*
                #(#lets_optional_moved)*
                std::result::Result::Ok( #struct_name { #(#sets)* } )
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
