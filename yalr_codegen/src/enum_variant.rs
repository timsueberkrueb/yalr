use std::fmt;

use quote::{quote, ToTokens};

// TODO: Find a better solution for this
/// Wrapper representing a simple enum variant, e.g. `MyEnum::Variant`
///
/// This is currently used over syn::Path for two reasons:
/// 1) syn::Path and also Type don't implement Ord, so we'd need at least a Newtype wrapper anyway
/// 2) EnumVariant allows easy access to the TokenStream for the variant part with
///    `EnumVariant::variant_token_stream`
///
/// This is far from being a good solution though and should be replaced in the future.
#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct EnumVariant {
    enum_type: syn::Ident,
    enum_variant: syn::Ident,
}

impl EnumVariant {
    pub fn new(enum_type: syn::Ident, enum_variant: syn::Ident) -> Self {
        Self {
            enum_type,
            enum_variant,
        }
    }

    pub fn variant_token_stream(&self) -> proc_macro2::TokenStream {
        let enum_variant = &self.enum_variant;
        quote! { #enum_variant }
    }
}

impl fmt::Display for EnumVariant {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let enum_variant = &self.enum_variant;
        write!(f, "{}", quote! { #enum_variant }.to_string())
    }
}

impl ToTokens for EnumVariant {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let enum_type = &self.enum_type;
        let enum_variant = &self.enum_variant;
        tokens.extend(quote! { #enum_type::#enum_variant });
    }
}
