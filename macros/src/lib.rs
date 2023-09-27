use std::collections::HashSet;

use proc_macro::TokenStream;
use proc_macro2::Ident;
use syn::{
    parse::Parse, parse_macro_input, parse_quote, punctuated::Punctuated, token::Comma, Lifetime,
    Type,
};

mod choices;
mod message;
mod parse;
mod role;
mod roles;
mod session;

#[proc_macro_derive(Message)]
pub fn message(input: TokenStream) -> TokenStream {
    message::message(input.into())
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

#[proc_macro_derive(Role, attributes(message, route))]
pub fn role(input: TokenStream) -> TokenStream {
    role::role(input.into())
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

#[proc_macro_derive(Roles)]
pub fn roles(input: TokenStream) -> TokenStream {
    roles::roles(input.into())
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

#[proc_macro_attribute]
pub fn session(attr: TokenStream, input: TokenStream) -> TokenStream {
    let sattr = parse_macro_input!(attr as SessionAttr);
    let k = init_global();
    session::session(sattr, input.into(), &k)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

#[proc_macro_attribute]
pub fn choices(attr: TokenStream, input: TokenStream) -> TokenStream {
    choices::choices(attr.into(), input.into())
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

#[derive(Debug, Default, Clone)]
struct SessionAttr {
    lt: Option<Lifetime>,
    ro: Option<Type>,
}

impl Parse for SessionAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let lt = input.parse().ok();
        let ro = input.parse().ok();
        Ok(Self { lt, ro })
    }
}

fn types() -> HashSet<Ident> {
    let idents: Punctuated<Ident, Comma> =
        parse_quote!(Select, End, Receive, Branch, Send, FullDual, PartialDual);
    idents.into_iter().collect()
}

struct Global {
    idents: HashSet<Ident>,
}

fn init_global() -> Global {
    Global { idents: types() }
}
