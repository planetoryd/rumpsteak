use crate::parse;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{
    parse::Parse, parse2, spanned::Spanned, Data, DeriveInput, Error, Index, Result, Token, Type,
};

pub fn role(input: TokenStream) -> Result<TokenStream> {
    let input = parse2::<DeriveInput>(input)?;

    // let message = parse::attribute::<Type>(&input.attrs, "message", input.span())?;

    let ident = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let mut output = quote! {
        impl ::rumpsteak::Role for #ident {
            // type Message = #message;
        }
    };

    let fields = match &input.data {
        Data::Struct(input) => Ok(&input.fields),
        _ => Err(Error::new_spanned(&input, "expected a struct")),
    }?;

    for (i, field) in fields.iter().enumerate() {
        let route = parse::attribute::<RouteType>(&field.attrs, "route", field.span())?;
        let node = route.target;
        let msg = route.msg;

        let field_ty = &field.ty;
        let field_ident = match &field.ident {
            Some(ident) => ident.to_token_stream(),
            None => Index::from(i).to_token_stream(),
        };

        output.extend(quote! {
            impl #impl_generics ::rumpsteak::Route<#node, #field_ty> for #ident #ty_generics #where_clause {
                type Onwire = #msg;

                fn route(&mut self) -> &mut #field_ty {
                    &mut self.#field_ident
                }
            }
        });
    }

    Ok(output)
}

#[derive(Debug, Clone)]
struct RouteType {
    target: Type,
    sep: Token!(,),
    msg: Type,
}

impl Parse for RouteType {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        Ok(Self {
            target: input.parse()?,
            sep: input.parse()?,
            msg: input.parse()?,
        })
    }
}
