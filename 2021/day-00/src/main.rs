macro_rules! debug_log {
    ($($arg:tt)*) => {
        #[cfg(debug_assertions)]
        {
            println!($($arg)*);
        }
    };
}

use tokio::io::BufReader;

#[tokio::main]
async fn main() {
    use tokio::io::AsyncBufReadExt;
    let reader = BufReader::new(tokio::io::stdin());
    let mut lines = reader.lines();
    while let Some(line) = lines.next_line().await.expect("IO error") {
        // Do something with the line input
        debug_log!("{}", line);
    }
}
