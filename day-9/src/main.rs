pub mod log;

#[cfg(debug_assertions)]
use crate::log::LOG_PREFIX;

fn find_sum_of(target: u64, a: u64, other: &[u64]) -> Option<u64> {
    // Check whether target can be achieved by adding 'a' and any other
    for b in other {
        if a + b == target {
            return Some(*b);
        }
    }
    return None;
}

fn check_number(prev: &[u64], target: u64, len: usize) -> Option<(u64, u64)> {
    for (i, a) in prev.iter().enumerate() {
        if i == (prev.len() - 1) { break; }
        // check if number a paired with another sums up to target
        if let Some(b) = find_sum_of(target, *a, &prev[(i+1)..len]) {
            return Some((*a, b));
        }
    }
    return None;
}

fn find_first_outlier(stream: &[u64], preamble: usize) -> Option<u64> {
    for i in preamble..stream.len() {
        // check n against its preamble
        match check_number(&stream[(i - preamble)..i], stream[i], preamble) {
            #[cfg(debug_assertions)]
            Some((a, b)) => { log!("{} is valid: (sum of {} and {})", stream[i], a, b); },
            #[cfg(not(debug_assertions))]
            Some(_ ) => {},
            None => return Some(stream[i]),
        };
    }
    return None;
}

fn contiguous_sum(data: &[u64]) -> u64 {
    data.iter().sum()
}

fn find_cont_sum_of<'a>(stream: &'a[u64], target: u64) -> Option<&'a[u64]> {
    for i in 2..stream.len() {
        // Go step by step back from i to sum up contiguous numbers
        for x in 2..i {
            let res = contiguous_sum(&stream[(i-x)..i]);
            if res == target {
                return Some(&stream[(i-x)..i]);
            }
        }
    }
    return None;
}

fn get_extremes(data: &[u64]) -> (u64, u64) {
    let mut min = u64::MAX;
    let mut max = u64::MIN;
    for x in data.iter() {
        min = min.min(*x);
        max = max.max(*x);
    }
    return (min, max);
}

fn get_weakness(stream: &[u64], target: u64) -> Option<u64> {
    find_cont_sum_of(&stream, target).map(|nums| {
        log!("found continuous stretch resulting in {}: {:?}", target, nums);
        get_extremes(nums)
    }).map(|(min, max)| {
        log!("min: {}  max: {}", min, max);
        min + max
    })
}

fn main() {
    let preamble = usize::from_str_radix(std::env::args().last().unwrap_or(String::new()).as_str(), 10).expect("Expecting preamble size as input!");
    println!("##### Advent of Code - Day 9 #####");

    let mut stream: Vec<u64> = Vec::new();
    let mut buffer = String::new();
    loop {
        buffer.clear();
        let n = std::io::stdin().read_line(&mut buffer).expect("Error occurred at IO Operation.");
        if n == 0 { break; }
        match u64::from_str_radix(buffer.trim(), 10) {
            Ok(res) => stream.push(res),
            Err(e) => {
                println!("{}", e);
                return;
            },
        }
    }

    // Find first outlier of numbers
    let outlier = find_first_outlier(&stream, preamble);
    println!("[1] => {:?}", outlier);
    if outlier.is_some() {
        let weakness = get_weakness(&stream, outlier.unwrap());
        println!("[2] => {:?}", weakness);
    }
}
