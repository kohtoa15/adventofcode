use std::sync::Mutex;
use std::error::Error;

const LOGGING: bool = true;

macro_rules! log {
    ($($arg:tt)*) => {
        if LOGGING {
            print!("# ");
            println!($($arg)*);
        }
    };
}

pub fn parse_char(c: char) -> Option<char> {
    match c {
        'F' => Some('0'),
        'B' => Some('1'),
        'L' => Some('0'),
        'R' => Some('1'),
        _ => None,
    }
}

pub fn parse_boarding_pass(mut s: String) -> Result<(u8, u8), Option<Box<dyn Error>>> {
    if s.len() != 10 {
        log!("boarding pass should be a 10-char string (provided length: {})!", s.len());
        return Err(None);
    }
    // Parse row
    let mut buf = String::with_capacity(7);
    for ch in s.drain(..7) {
        let c = parse_char(ch).ok_or(None)?;
        buf.push(c); 
    }
    let row: u8 = u8::from_str_radix(&buf, 2).map_err(|e| Some(e.into()) )?;
    buf.clear();
    // Parse column
    for ch in s.drain(..3) {
        let c = parse_char(ch).ok_or(None)?;
        buf.push(c); 
    }
    let col: u8 = u8::from_str_radix(&buf, 2).map_err(|e| Some(e.into()) )?;
    // return position
    Ok( (row, col) )
}

pub fn calculate_seat_id(row: u8, col: u8) -> u32 {
    return row as u32 * 8 + col as u32;
}

pub fn print_missing_ids(vec: &Vec<u32>, start: usize, end: usize) {
    let mut ids = vec.iter();
    let mut cur = ids.next();
    let mut missing_count: usize = 0;
    print!("## Missing seat ids: [2]\n   ");
    for i in start..end {
        match cur {
            Some(id) => {
                let id = *id as usize;
                if i < id {
                    // i missing from ids
                    if missing_count == 0 {
                        print!("{}", i);
                    } else if missing_count == 1{
                        print!("-");
                    }
                    missing_count += 1;
                } else if i == id {
                    // i is represented in ids
                    if missing_count > 0 {
                        if missing_count > 1 {
                            // print previous as missing
                            print!("{}", i - 1);
                        }
                        print!(",\n   ");
                        missing_count = 0;
                    }
                    cur = ids.next();
                }
            },
            None => {
                // All numbers after this one are missing
                if missing_count == 0 {
                    print!("{}-{}", i, end - 1);
                } else if missing_count == 1 {
                    print!("-{}", end - 1);
                } else {
                    print!("{}", end - 1 );
                }
                println!("\n");
                break;
            },
        };
    }
}

fn main() {
    println!("##### Advent of Code 2020 - Day 5 #####");
    let mut buffer = Mutex::new(String::new());

    let mut lines = Vec::new();

    loop {
        buffer.lock().unwrap().clear();
        match std::io::stdin().read_line(buffer.get_mut().unwrap()) {
            Ok(n) => {
                if n == 0 { /* Reached EOF */ break;}
                // Add to lines
                lines.push(buffer.lock().unwrap().clone());
            },
            Err(e) => {
                println!("Error occurred while trying to read stdin: {}", e);
                return;
            },
        }
    }

    let mut ids = Vec::with_capacity(lines.len());
    for line in lines.into_iter() {
        match parse_boarding_pass(line.trim().to_string()) {
            Ok((row, col)) => {
                let seat_id = calculate_seat_id(row, col);
                // Insert seat id into vector
                match ids.binary_search(&seat_id) {
                    Ok(_pos) => {}, // element is already in vector @pos
                    Err(pos) => ids.insert(pos, seat_id),
                }
            },
            Err(e) => match e {
                Some(e) => println!("{}", e.to_string()),
                None => println!("Unknown error occurred!"),
            },
        }
    }

    print_missing_ids(&ids, 0, 1024);

    let count = ids.len();
    println!("## Highest seat id encountered: {} (Processed {} boarding passes) [1]", ids[count-1], count);
}
