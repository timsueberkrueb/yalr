#[cfg(feature = "trace")]
#[macro_export]
macro_rules! yalr_trace {
    () => {
        print!("\n")
    };
    ($($arg:tt)*) => {{

        println!("[yalr] {}", format!($($arg)*))
    }}
}

#[cfg(not(feature = "trace"))]
#[macro_export]
macro_rules! yalr_trace {
    () => {};
    ($($arg:tt)*) => {{}};
}
