use std::error::Error;
use std::fmt;

#[derive(Debug)]
pub enum CodegenError {
    Static(&'static str),
    Owned(String),
}

impl CodegenError {
    pub fn from_static(msg: &'static str) -> Self {
        CodegenError::Static(msg)
    }

    pub fn from_owned(msg: String) -> Self {
        CodegenError::Owned(msg)
    }
}

impl fmt::Display for CodegenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            CodegenError::Static(msg) => write!(f, "Codegen error: {}", msg),
            CodegenError::Owned(msg) => write!(f, "Codegen error: {}", msg),
        }
    }
}

impl Error for CodegenError {}
