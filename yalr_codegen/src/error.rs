use std::error::Error;
use std::fmt;

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

impl fmt::Debug for CodegenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            CodegenError::Static(msg) => write!(f, "{}", msg),
            CodegenError::Owned(msg) => write!(f, "{}", msg),
        }
    }
}

impl fmt::Display for CodegenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        <Self as fmt::Debug>::fmt(self, f)
    }
}

impl Error for CodegenError {}
