use rustc_version::{version_meta, Channel};

fn main() {
    // TODO: Remove once type_alias_enum_variants (RFC 2338) gets stabilized
    if let Channel::Nightly = version_meta().unwrap().channel {
        println!("cargo:rustc-cfg=type_alias_enum_variants");
    }
}
