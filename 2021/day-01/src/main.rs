use std::{error::Error, collections::BTreeMap, iter::Map};

use tokio::io::BufReader;

macro_rules! debug_log {
    ($($arg:tt)*) => {
        #[cfg(debug_assertions)]
        {
            println!($($arg)*);
        }
    };
}

#[tokio::main]
async fn main() {
    use tokio::io::AsyncBufReadExt;
    let reader = BufReader::new(tokio::io::stdin());
    let mut lines = reader.lines();

    let mut increase_counter: u32 = 0;
    let mut measurements = Vec::new();
    let mut msrmt_windows = MeasurementWindow::default();

    let mut line_count = 0;
    while let Some(line) = lines.next_line().await.expect("IO error") {
        // Parse input to distance from sea floor
        let dist = u32::from_str_radix(line.as_str(), 10).expect("Could not parse line input");
        let windows = get_current_windows(line_count);

        // has distance increases since last measurement?
        if let Some(prev) = measurements.last() {
            if dist > *prev {
                increase_counter += 1;
            }
        }

        // Enter value into current measurement windows
        for w in windows {
            msrmt_windows.add_measurement(w, dist);
        }

        // Store for later use
        measurements.push(dist);
        line_count += 1;
    }

    println!(
        "1) How many measurements are larger than the previous measurement?\n --> Answer: {}",
        increase_counter
    );


    // Calculate increases between measurement windows
    let mut sum_increase_counter: u32 = 0;
    // last 2 must be discarded (not finished)
    let sums: Vec<(String, u32)> = msrmt_windows.get_sums()
        .into_iter()
        .take(msrmt_windows.len() - 2)
        .collect();
    let mut last_val: Option<u32> = None;
    for (w, sum) in sums {
        debug_log!("{}: {}", w, sum);
        if let Some(prev) = last_val {
            if sum > prev {
                sum_increase_counter += 1;
            }
        }
        last_val = Some(sum);
    }
    println!("2) How many sums are larger than the previous sum?\n --> Answer: {}", sum_increase_counter);
}

/// Returns the indices of the windows this measurement is part of
fn get_current_windows(index: usize) -> Vec<u32> {
    // First window has same index as line
    //
    // i=0 has just 1 window, i=1 has 2, all others have 3
    let mut indices = Vec::with_capacity(3);
    for i in (0..=index).rev().take(3) {
        indices.push(i as u32);
    }
    return indices;
}

struct MeasurementWindow {
    inner: BTreeMap<u32, Vec<u32>>,
}

impl MeasurementWindow {
    pub fn add_measurement(&mut self, key: u32, value: u32) {
        if let Some(v) = self.inner.get_mut(&key) {
            v.push(value);
        } else {
            // Create new vec and add to map
            self.inner.insert(key, vec![value]);
        }
    }

    pub fn get_sums(&self) -> Vec<(String, u32)> {
        self.inner.iter()
            .map(|(k,v)| {
                let ident = key_to_identifier(k);
                let sum: u32 = v.iter().sum();
                (ident, sum)
            })
            .collect()
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }
}

impl Default for MeasurementWindow {
    fn default() -> Self {
        MeasurementWindow { inner: BTreeMap::new() }
    }
}

fn key_to_identifier(key: &u32) -> String {
    let mut ident = String::new();
    let mut first_run = true;
    // We use char range A-Z (65-90)
    let mut k = *key;
    while k > 0 || first_run {
        let chr = (k % 26) + 65; // Range 65..=90
        k = k / 26;
        // Generate char from chr
        assert!(chr >= 65 && chr <= 90);
        ident.push(char::from_u32(chr).unwrap());
        // Disable first_run after initial use
        if first_run { first_run = false; }
    }
    return ident;
}
