use std::fmt;

use quote::{quote, ToTokens};

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Terminal {
    UserDefined(syn::Ident),
    End,
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Nonterminal {
    UserDefined(syn::Ident),
    Start,
}

impl fmt::Display for Terminal {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Terminal::UserDefined(ident) => write!(f, "{}", quote! { #ident }.to_string()),
            Terminal::End => write!(f, "End"),
        }
    }
}

impl fmt::Display for Nonterminal {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            Nonterminal::UserDefined(ident) => write!(f, "{}", quote! { #ident }.to_string()),
            Nonterminal::Start => write!(f, "Start"),
        }
    }
}

impl Terminal {
    pub fn new(ident: syn::Ident) -> Self {
        Terminal::UserDefined(ident)
    }

    pub fn ident_token_stream(&self) -> proc_macro2::TokenStream {
        match self {
            Terminal::UserDefined(ident) => quote! { #ident },
            Terminal::End => quote! { END },
        }
    }
}

impl Nonterminal {
    pub fn new(ident: syn::Ident) -> Self {
        Nonterminal::UserDefined(ident)
    }

    pub fn ident_token_stream(&self) -> proc_macro2::TokenStream {
        match self {
            Nonterminal::UserDefined(ident) => {
                let prefixed_ident = syn::Ident::new(
                    &format!("UserDefined_{}", ident),
                    proc_macro2::Span::call_site(),
                );
                quote! { #prefixed_ident }
            }
            Nonterminal::Start => quote! { Start },
        }
    }
}

impl ToTokens for Terminal {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let result = match self {
            Terminal::UserDefined(ident) => quote! { __TERMINAL_TYPE::#ident },
            Terminal::End => quote! { L::END },
        };
        tokens.extend(result);
    }
}

impl ToTokens for Nonterminal {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        let ident = self.ident_token_stream();
        tokens.extend(quote! { Nonterminal::#ident });
    }
}
