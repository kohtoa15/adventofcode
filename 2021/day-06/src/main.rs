macro_rules! debug_log {
    ($($arg:tt)*) => {
        #[cfg(debug_assertions)]
        {
            println!($($arg)*);
        }
    };
}

use std::collections::HashMap;

use itertools::Itertools;
use tokio::io::BufReader;

#[tokio::main]
async fn main() {
    use tokio::io::AsyncBufReadExt;
    let reader = BufReader::new(tokio::io::stdin());
    let mut lines = reader.lines();

    let mut all_fish = Vec::new();

    while let Some(line) = lines.next_line().await.expect("IO error") {
        let s: Vec<&str> = line.trim()
            .split(",")
            .collect();
        for k in s {
            all_fish.push(u8::from_str_radix(k, 10).expect("Could not parse input"));
        }
    }

    let mut ages: HashMap<Lanternfish, u128> = all_fish.iter()
        .sorted()
        .unique()
        .map(|x| {
            let count = all_fish.iter()
                .filter(|n| **n == *x)
                .count();
            (Lanternfish::from(*x), count as u128)
        })
        .collect();

    // Simulate for 80 days
    for day in 0..80 {
        age_all_one_day(&mut ages);
        debug_log!(
            "\nDay {}: {}",
            day + 1,
            ages.iter()
                .map(|(_,v)| *v)
                .sum::<u128>()
        );
    }
    // Record 80-day result
    let res_80d = ages.iter()
        .map(|(_, v)| v)
        .sum::<u128>();

    // Simulate until day 256
    for day in 80..256 {
        age_all_one_day(&mut ages);
        debug_log!(
            "\nDay {}: {}",
            day + 1,
            ages.iter()
                .map(|(_,v)| *v)
                .sum::<u128>()
        );
    }
    let res_256d = ages.into_iter()
        .map(|(_, v)| v)
        .sum::<u128>();

    println!(
        "1) Find a way to simulate lanternfish. How many lanternfish would there be after 80 days?\n  --> Answer: {}",
        res_80d
    );
    println!(
        "2) How many lanternfish would there be after 256 days?\n  --> Answer: {}",
        res_256d
    );
}

fn age_all_one_day(ages: &mut HashMap<Lanternfish, u128>) {
    let mut next_fish: HashMap<Lanternfish, u128> = HashMap::with_capacity(ages.len());
    // Age all groups of fish
    for (mut f, n) in ages.drain().sorted() {
        debug_log!("age {} -> {} fish", f.timer, n);
        if f.age_one_day() {
            // Add `n` new fish with age 8
            let key = Lanternfish::from(8);
            debug_log!("adding {} aged 8", n);
            if let Some(eights) = next_fish.get_mut(&key) {
                *eights += n;
            } else {
                next_fish.insert(key, n);
            }
        }
        debug_log!("aged to {} ({} fish)", f.timer, n);
        if let Some(sixes) = next_fish.get_mut(&f) {
            *sixes += n;
        } else {
            next_fish.insert(f, n);
        }
    }
    // Reassign modified fish register
    *ages = next_fish;
}

trait Aging {
    fn age_one_day(&mut self) -> bool;
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
struct Lanternfish {
    pub timer: u8
}

impl Aging for Lanternfish {
    /// Returns true if a new fish is spawned
    fn age_one_day(&mut self) -> bool {
        if self.timer == 0 {
            self.timer = 6;
            true
        } else {
            self.timer -= 1;
            false
        }
    }
}

impl From<u8> for Lanternfish {
    fn from(timer: u8) -> Self {
        Lanternfish { timer }
    }
}
