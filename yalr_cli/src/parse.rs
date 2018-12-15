use std::error::Error;
use std::fmt;
use std::fs::File;
use std::io::Read;

use quote::quote;
use syn::visit::Visit;
use yalr_codegen::{Nonterminal, Terminal};

pub fn generate_parse_table(
    filename: &str,
    impl_type: Option<&str>,
) -> Result<yalr_core::ParseTable<Terminal, Nonterminal>, Box<dyn Error>> {
    let file: syn::File = parse_source_file(filename)?;
    let item_impl: &syn::ItemImpl = find_yalr_impl(&file, impl_type)?;
    let parse_table = yalr_codegen::generate_parse_table(item_impl)?;
    Ok(parse_table)
}

fn parse_source_file(filename: &str) -> Result<syn::File, Box<dyn Error>> {
    let mut file = File::open(filename)?;

    let mut content = String::new();
    file.read_to_string(&mut content)?;

    let file = syn::parse_file(&content)?;
    Ok(file)
}

fn find_yalr_impl<'a, 's>(
    file: &'a syn::File,
    impl_type: Option<&'s str>,
) -> Result<&'a syn::ItemImpl, TableError> {
    let mut visitor = YALRImplVisitor::new(impl_type);
    visitor.visit_file(file);

    if visitor.item_impls.is_empty() {
        Err(TableError::NoImpls)
    } else if visitor.item_impls.len() > 1 {
        let item_impl_types: Vec<String> = visitor
            .item_impls
            .iter()
            .map(|item_impl| {
                let self_ty = &(*item_impl.self_ty);
                quote!(#self_ty).to_string()
            })
            .collect();
        Err(TableError::MultipleImpls(item_impl_types))
    } else {
        Ok(&visitor.item_impls[0])
    }
}

struct YALRImplVisitor<'ast, 's> {
    item_impls: Vec<&'ast syn::ItemImpl>,
    impl_type: Option<&'s str>,
}

impl<'ast, 's> YALRImplVisitor<'ast, 's> {
    fn new(impl_type: Option<&'s str>) -> Self {
        Self {
            item_impls: Vec::new(),
            impl_type,
        }
    }
}

impl<'ast, 's> Visit<'ast> for YALRImplVisitor<'ast, 's> {
    fn visit_item_impl(&mut self, item_impl: &'ast syn::ItemImpl) {
        for attr in item_impl.attrs.iter().by_ref() {
            if attr.path.is_ident("lalr") {
                let self_ty = &(*item_impl.self_ty);
                let type_string = quote!(#self_ty).to_string();
                if let Some(expected_type) = self.impl_type {
                    if type_string != expected_type {
                        return;
                    }
                }
                self.item_impls.push(item_impl);
                break;
            }
        }
    }
}

#[derive(Debug)]
enum TableError {
    NoImpls,
    MultipleImpls(Vec<String>),
}

impl fmt::Display for TableError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            TableError::NoImpls => write!(f, "Found no matching YALR impl blocks"),
            TableError::MultipleImpls(types) => {
                let type_str = types.join(", ");
                writeln!(f, "Found multiple YALR impl blocks")?;
                writeln!(f, "Found impl blocks for the following types: {}", type_str)?;
                write!(
                    f,
                    "Hint: select a specific impl block using the --impl option."
                )
            }
        }
    }
}

impl Error for TableError {}
