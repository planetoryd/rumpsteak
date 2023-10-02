use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use std::{collections::HashSet, mem};
use syn::{
    parse::{Nothing, Parse},
    parse2, parse_macro_input, parse_quote,
    punctuated::Punctuated,
    token::Comma,
    Error, Expr, Fields, GenericArgument, GenericParam, Ident, Index, Item, ItemEnum, ItemStruct,
    ItemType, PathArguments, Result, TraitItem, TraitItemType, Type,
};

use crate::{init_global, Global, SessionAttr};

fn idents_set<P>(params: &Punctuated<GenericParam, P>) -> HashSet<Ident> {
    let idents = params.iter().filter_map(|param| match param {
        GenericParam::Type(ty) => Some(ty.ident.clone()),
        _ => None,
    });
    idents.collect::<HashSet<_>>()
}

pub fn punctuated_prepend<T, P: Default>(left: &mut Punctuated<T, P>, mut right: Punctuated<T, P>) {
    right.extend(mem::take(left));
    *left = right;
}

fn unroll_type(mut ty: &mut Type) -> &mut Type {
    loop {
        ty = match ty {
            Type::Group(ty) => ty.elem.as_mut(),
            Type::Paren(ty) => ty.elem.as_mut(),
            _ => break,
        }
    }

    ty
}

#[test]
fn aug_test() {
    let k = init_global();

    let mut t = parse_quote!(Send<Server, FDStream, End>);
    augment_type(
        &mut t,
        &Default::default(),
        parse_quote!('k Self::Selector),
        &k,
    );
    println!("{}", t.to_token_stream());
}

fn augment_type(mut ty: &mut Type, exclude: &HashSet<Ident>, sattr: SessionAttr, global: &Global) {
    while let Type::Path(path) = unroll_type(ty) {
        if *path == parse_quote!(Self) {
            break;
        }

        let segment = match path.path.segments.last_mut() {
            Some(segment) => segment,
            _ => break,
        };

        if let PathArguments::None = segment.arguments {
            if exclude.contains(&segment.ident) {
                break;
            }

            segment.arguments = PathArguments::AngleBracketed(parse_quote!(<>));
        }

        let args = match &mut segment.arguments {
            PathArguments::AngleBracketed(args) => &mut args.args,
            _ => break,
        };

        if !global.idents.contains(&segment.ident) {
            break;
        }

        cond_gener_use(args, &sattr);

        ty = match args.last_mut() {
            Some(GenericArgument::Type(ty)) => ty,
            _ => break,
        };
    }
}

fn cond_gener(punc: &mut Punctuated<GenericParam, Comma>, sattr: &SessionAttr, omit: bool) {
    let SessionAttr { lt, ro } = sattr;
    if let Some(lt) = lt {
        if !omit {
            punctuated_prepend(punc, parse_quote!(#lt));
        }
    } else {
        punctuated_prepend(punc, parse_quote!('r));
    }
    if let Some(ro) = ro {
        if !omit {
            punctuated_prepend(punc, parse_quote!(#ro));
        }
    } else {
        punctuated_prepend(punc, parse_quote!(Ro: ::rumpsteak::Role));
    }
}

fn cond_gener_use(punc: &mut Punctuated<GenericArgument, Comma>, sattr: &SessionAttr) {
    let SessionAttr { lt, ro } = sattr;
    if let Some(lt) = lt {
        punctuated_prepend(punc, parse_quote!(#lt));
    } else {
        punctuated_prepend(punc, parse_quote!('r));
    }
    if let Some(ro) = ro {
        punctuated_prepend(punc, parse_quote!(#ro));
    } else {
        punctuated_prepend(punc, parse_quote!(Ro));
    }
}

fn session_type(mut input: ItemType, sattr: SessionAttr, global: &Global) -> TokenStream {
    let exclude = idents_set(&input.generics.params);
    cond_gener(&mut input.generics.params, &sattr, true);
    augment_type(&mut input.ty, &exclude, sattr, global);
    input.into_token_stream()
}

fn session_type_in_trait(
    mut input: TraitItemType,
    sattr: SessionAttr,
    global: &Global,
) -> TokenStream {
    let exclude = idents_set(&input.generics.params);
    cond_gener(&mut input.generics.params, &sattr, true);
    if let Some((eq, b)) = &mut input.default {
        augment_type(b, &exclude, sattr, global);
    }
    input.into_token_stream()
}

fn session_struct(
    mut input: ItemStruct,
    sattr: SessionAttr,
    global: &Global,
) -> Result<TokenStream> {
    let ident = &input.ident;
    let exclude = idents_set(&input.generics.params);

    punctuated_prepend(
        &mut input.generics.params,
        parse_quote!('r, Ro: ::rumpsteak::Role),
    );
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    if input.fields.len() != 1 {
        let message = "expected exactly one field";
        return Err(Error::new_spanned(&input.fields, message));
    }

    let field = input.fields.iter_mut().next().unwrap();
    augment_type(&mut field.ty, &exclude, sattr, global);

    let field_ty = &field.ty;
    let field_ident = match &field.ident {
        Some(ident) => ident.to_token_stream(),
        None => Index::from(0).to_token_stream(),
    };

    let mut output = TokenStream::new();
    output.extend(quote! {
        impl #impl_generics ::rumpsteak::FromState<'r> for #ident #ty_generics #where_clause {
            type Role = Ro;

            fn from_state(state: ::rumpsteak::State<'r, Self::Role>) -> Self {
                Self { #field_ident: ::rumpsteak::FromState::from_state(state) }
            }
        }

        impl #impl_generics ::rumpsteak::IntoSession<'r> for #ident #ty_generics #where_clause {
            type Session = #field_ty;

            fn into_session(self) -> Self::Session {
                self.#field_ident
            }
        }

        impl<'k, Ro: Role, B> FullDual<'k, Ro, B> for #ident<'k, Ro>
        where
            <Self as ::rumpsteak::IntoSession<'k>>::Session: FullDual<'k, Ro, B>,
        {
            type Dual = <<Self as ::rumpsteak::IntoSession<'k>>::Session as FullDual<'k, Ro, B>>::Dual;
        }
    });

    #[cfg(feature = "serialize")]
    {
        let mut where_clause = where_clause.cloned().unwrap_or_else(|| parse_quote!(where));
        where_clause.predicates.push(parse_quote!(Self: 'static));

        output.extend(quote! {
            impl #impl_generics ::rumpsteak::serialize::Serialize for #ident #ty_generics #where_clause {
                fn serialize(s: &mut ::rumpsteak::serialize::Serializer) {
                    <#field_ty as ::rumpsteak::serialize::Serialize>::serialize(s);
                }
            }
        });
    }

    Ok(quote!(#input #output))
}

fn session_enum(mut input: ItemEnum, sattr: SessionAttr, global: &Global) -> Result<TokenStream> {
    if input.variants.is_empty() {
        let message = "expected at least one variant";
        return Err(Error::new_spanned(&input.variants, message));
    }

    let ident = &input.ident;
    let exclude = idents_set(&input.generics.params);

    let mut generics = input.generics.clone();
    punctuated_prepend(
        &mut generics.params,
        parse_quote!('__q, 'r, Ro: ::rumpsteak::Role + 'r),
    );
    let (impl_generics, _, _) = generics.split_for_impl();

    let mut generics = input.generics.clone();
    punctuated_prepend(
        &mut generics.params,
        parse_quote!('__q, Ro: ::rumpsteak::Role),
    );
    let (_, ty_generics, where_clause) = generics.split_for_impl();

    let mut idents = Vec::with_capacity(input.variants.len());
    let mut labels = Vec::with_capacity(input.variants.len());
    let mut tys = Vec::with_capacity(input.variants.len());

    for variant in &mut input.variants {
        idents.push(&variant.ident);
        let fields = match &mut variant.fields {
            Fields::Unnamed(fields) => Ok(&mut fields.unnamed),
            fields => Err(Error::new_spanned(fields, "expected tuple variants")),
        }?;

        if fields.len() != 2 {
            let message = "expected exactly two fields per variant";
            return Err(Error::new_spanned(fields, message));
        }

        let mut fields = fields.iter_mut();

        let label = &fields.next().unwrap().ty;
        labels.push(label);

        let ty = &mut fields.next().unwrap().ty;
        augment_type(ty, &exclude, sattr.clone(), global);
        tys.push(&*ty);
    }

    let mut output = TokenStream::new();
    for (label, ty) in labels.iter().zip(&tys) {
        output.extend(quote! {
            impl #impl_generics ::rumpsteak::Choice<'r, #label> for #ident #ty_generics #where_clause {
                type Session = #ty;
            }
        });
    }

    punctuated_prepend(
        &mut input.generics.params,
        parse_quote!('r, Ro: ::rumpsteak::Role),
    );
    let (impl_generics, ty_generics, _) = input.generics.split_for_impl();

    #[cfg(feature = "serialize")]
    {
        let mut where_clause = where_clause.cloned().unwrap_or_else(|| parse_quote!(where));
        where_clause.predicates.push(parse_quote!(Self: 'static));

        output.extend(quote! {
            impl #impl_generics ::rumpsteak::serialize::SerializeChoices for #ident #ty_generics #where_clause {
                fn serialize_choices(mut s: ::rumpsteak::serialize::ChoicesSerializer<'_>) {
                    #(s.serialize_choice::<#labels, #tys>();)*
                }
            }
        });
    }

    let mut generics = input.generics.clone();
    generics.make_where_clause().predicates.push(parse_quote! {
        Ro::Message: #(::rumpsteak::Message<#labels> +)*
    });

    let (_, _, where_clause) = generics.split_for_impl();
    output.extend(quote! {
        impl #impl_generics ::rumpsteak::Choices<'r> for #ident #ty_generics #where_clause {
            type Role = Ro;

            fn downcast(
                state: ::rumpsteak::State<'r, Self::Role>,
                message: <Self::Role as Role>::Message,
            ) -> ::core::result::Result<Self, <Self::Role as Role>::Message> {
                #(let message = match ::rumpsteak::Message::downcast(message) {
                    Ok(label) => {
                        return Ok(Self::#idents(
                            label,
                            ::rumpsteak::FromState::from_state(state)
                        ));
                    }
                    Err(message) => message
                };)*

                Err(message)
            }
        }
    });

    Ok(quote!(#input #output))
}

pub(crate) fn session(attr: SessionAttr, input: TokenStream, k: &Global) -> Result<TokenStream> {
    match parse2::<Item>(input.clone()) {
        Ok(item) => match item {
            Item::Type(input) => Ok(session_type(input, attr, k)),
            Item::Struct(input) => session_struct(input, attr, k),
            // Item::Enum(input) => session_enum(input),
            item => Err(Error::new_spanned(
                item,
                "expected a type, struct or enum. (Item)",
            )),
        },
        Err(_) => match parse2::<TraitItem>(input)? {
            TraitItem::Type(input) => Ok(session_type_in_trait(input, attr, k)),
            item => Err(Error::new_spanned(
                item,
                "expected a type, struct or enum. not a type in trait",
            )),
        },
    }
}
