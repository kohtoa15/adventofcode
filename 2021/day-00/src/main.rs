use std::io::Cursor;

use tokio::io::BufReader;

#[tokio::main]
async fn main() {
    use tokio::io::AsyncBufRead;
    let reader = BufReader::new(tokio::io::stdin());
    let lines = reader.lines();
    while let Some(line) = lines.next_line() {

    }
}
