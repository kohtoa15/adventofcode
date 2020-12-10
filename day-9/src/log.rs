pub const LOG_PREFIX: &str = "";

#[macro_export]
macro_rules! log {
    ($($arg:tt)*) => {
        #[cfg(debug_assertions)]
        {
            print!("{}", LOG_PREFIX);
            println!($($arg)*);
        }
    };
}


