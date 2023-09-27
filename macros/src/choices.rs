use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse2, parse_macro_input, parse_quote, token::Struct, Data, DeriveInput, Error, Field, Fields,
    FieldsUnnamed, ItemEnum, ItemStruct, Result,
};

use crate::session::punctuated_prepend;

pub fn choices(attr: TokenStream, input: TokenStream) -> Result<TokenStream> {
    let mut input = parse2::<ItemEnum>(input)?;

    let ident = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let variants = &mut input.variants;

    let mut output = TokenStream::new();

    for variant in variants {
        let variant_ident = &variant.ident;
        match variant.fields {
            Fields::Unit => {
                variant.fields =
                    Fields::Unnamed(parse2::<FieldsUnnamed>(quote!((#variant_ident)))?);

                output.extend(quote! {
                    #[derive(Debug)]
                    pub struct #variant_ident;
                    impl ::rumpsteak::ChoiceV<#ident> for #variant_ident {
                        fn v(self) -> #ident {
                            #ident::#variant_ident(self)
                        }
                    }
                });
            }
            Fields::Unnamed(ref mut f) => {
                if f.unnamed.len() == 0 {
                    // Convenience, for not duplicating the name
                    variant.fields =
                        Fields::Unnamed(parse2::<FieldsUnnamed>(quote!((#variant_ident)))?);

                    output.extend(quote! {
                        impl ::rumpsteak::ChoiceV<#ident> for #variant_ident {
                            fn v(self) -> #ident {
                                #ident::#variant_ident(self)
                            }
                        }
                    });
                } else {
                    let var_struct = format_ident!("{}Variant", variant_ident);
                    let mut struc: ItemStruct = parse_quote!(pub struct #var_struct #f;);
                    struc.attrs = if variant.attrs.is_empty() {
                        input.attrs.clone()
                    } else {
                        variant.attrs.clone()
                    };
                    *f = parse_quote!((#var_struct));
                    output.extend(quote! {
                        #struc
                        impl ::rumpsteak::ChoiceV<#ident> for #var_struct {
                            fn v(self) -> #ident {
                                #ident::#variant_ident(self)
                            }
                        }
                    });
                }
            }
            _ => (),
        }
    }
    let unit = format_ident!("{}Branch", ident);
    output.extend(quote!(
        pub struct #unit;
        impl Choices for #unit {
            type Repr = #ident;
        }
    ));

    Ok(quote!(#input #output))
}
