use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{
    parse2, parse_macro_input, parse_quote, token::Struct, Data, DeriveInput, Error, Field, Fields,
    FieldsUnnamed, ItemEnum, ItemStruct, Result, Type,
};

use crate::session::punctuated_prepend;

pub fn choices(attr: TokenStream, input: TokenStream) -> Result<TokenStream> {
    let mut input = parse2::<ItemEnum>(input)?;

    let enum_id = &input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let variants = &mut input.variants;

    let mut output = TokenStream::new();

    output.extend(quote!(
        impl #impl_generics ::rumpsteak::Message<Self> for #enum_id #ty_generics #where_clause {
            fn upcast(label: Self) -> Self {
                label
            }

            fn downcast(self) -> ::core::result::Result<Self, Self> {
                ::core::result::Result::Ok(self)
            }
        }
    ));

    for variant in variants {
        let variant_ident = &variant.ident;
        match variant.fields {
            Fields::Unit => {
                variant.fields =
                    Fields::Unnamed(parse2::<FieldsUnnamed>(quote!((#variant_ident)))?);
                let mut struc: ItemStruct = parse_quote!(pub struct #variant_ident;);
                if variant.attrs.is_empty() {
                    struc.attrs.extend(input.attrs.clone());
                } else {
                    struc.attrs.extend(variant.attrs.drain(..));
                }

                output.extend(quote! {
                    #struc
                    impl ::rumpsteak::ChoiceV<#enum_id> for #variant_ident {
                        fn v(self) -> #enum_id {
                            #enum_id::#variant_ident(self)
                        }
                    }
                    impl #impl_generics ::rumpsteak::Message<#variant_ident> for #enum_id #ty_generics #where_clause {
                        fn upcast(label: #variant_ident) -> Self {
                            #enum_id::#variant_ident(label)
                        }
        
                        fn downcast(self) -> ::core::result::Result<#variant_ident, Self> {
                            match self {
                                Self::#variant_ident(label) => ::core::result::Result::Ok(label),
                                _ => ::core::result::Result::Err(self),
                            }
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
                        impl ::rumpsteak::ChoiceV<#enum_id> for #variant_ident {
                            fn v(self) -> #enum_id {
                                #enum_id::#variant_ident(self)
                            }
                        }
                        impl #impl_generics ::rumpsteak::Message<#variant_ident> for #enum_id #ty_generics #where_clause {
                            fn upcast(label: #variant_ident) -> Self {
                                #enum_id::#variant_ident(label)
                            }
            
                            fn downcast(self) -> ::core::result::Result<#variant_ident, Self> {
                                match self {
                                    Self::#variant_ident(label) => ::core::result::Result::Ok(label),
                                    _ => ::core::result::Result::Err(self),
                                }
                            }
                        }
                    });
                } else {
                    let ty_var: Type = if f.unnamed.len() > 1 {
                        let var_struct = variant_ident;
                        let mut struc: ItemStruct = parse_quote!(pub struct #variant_ident #f;);
                        if variant.attrs.is_empty() {
                            struc.attrs.extend(input.attrs.clone());
                        } else {
                            struc.attrs.extend(variant.attrs.drain(..));
                        }
                        *f = parse_quote!((#variant_ident));
                        output.extend(quote!(
                            #struc
                        ));
                        parse_quote!(#var_struct)
                    } else {
                        f.unnamed.first().unwrap().ty.clone()
                    };

                    output.extend(quote! {
                        impl ::rumpsteak::ChoiceV<#enum_id> for #ty_var {
                            fn v(self) -> #enum_id {
                                #enum_id::#variant_ident(self)
                            }
                        }
                        impl #impl_generics ::rumpsteak::Message<#ty_var> for #enum_id #ty_generics #where_clause {
                            fn upcast(label: #ty_var) -> Self {
                                #enum_id::#variant_ident(label)
                            }
            
                            fn downcast(self) -> ::core::result::Result<#ty_var, Self> {
                                match self {
                                    Self::#variant_ident(label) => ::core::result::Result::Ok(label),
                                    _ => ::core::result::Result::Err(self),
                                }
                            }
                        }
                    });
                }
            }
            _ => (),
        }

    }
    let unit = format_ident!("{}Branch", enum_id); // do I really need this
    output.extend(quote!(
        pub struct #unit;
        impl Choices for #unit {
            type Repr = #enum_id;
        }
        impl Choices for #enum_id {
            type Repr = Self;
        }
    ));

    Ok(quote!(#input #output))
}
