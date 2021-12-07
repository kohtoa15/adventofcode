macro_rules! debug_log {
    ($($arg:tt)*) => {
        #[cfg(debug_assertions)]
        {
            println!($($arg)*);
        }
    };
}

use std::fmt::Debug;

use bitfield::{BitRange, Bit};
#[cfg(debug_assertions)]
use itertools::Itertools;
use tokio::io::BufReader;

#[tokio::main]
async fn main() {
    use tokio::io::AsyncBufReadExt;
    let reader = BufReader::new(tokio::io::stdin());
    let mut lines = reader.lines();

    let mut rate = GammaRate::new(5);
    let mut values = Vec::new();

    while let Some(line) = lines.next_line().await.expect("IO error") {
        values.push(u32::from_str_radix(line.as_str(), 2).expect("Could not parse binary number"));
        let bits: Vec<(usize, bool)> = line.char_indices()
            .map(|(i, c)| (i, c.to_digit(2).expect("Invalid bit character!")))
            .map(|(i, b)| (i, b != 0))
            .collect();
        rate.add_bit_vec(bits);
    }

    let bits = rate.get_bits();
    debug_log!("read all input");
    let (gamma, epsilon) = rate.into_gamma_epsilon();
    debug_log!("γ = {}, ε = {}", gamma, epsilon);
    let oxygen_gen_rating = get_rating(bits, values.clone(), true).expect("Could not generate value");
    let co2_scrubber_rating = get_rating(bits, values, false).expect("Could not generate value");
    debug_log!("oxygen generator rating:\t{}\nco2 scrubber rating:\t\t{}", oxygen_gen_rating, co2_scrubber_rating);

    let res1 = gamma as f64 * epsilon as f64;
    println!("1) What is the power consumption of the submarine?\n  --> Answer: {}", res1);
    let res2 = oxygen_gen_rating * co2_scrubber_rating;
    println!("2) What is the life support rating of the submarine??\n  --> Answer: {}", res2);
}

#[derive(Debug)]
struct GammaRate {
    // bits per individual number
    bits: u8,
    // counts occurrences of `1` for each bit
    incidence: Vec<usize>,
    // counts number of all reports to calculate against incidence
    report_count: usize,
}

impl GammaRate {
    pub fn new(bits: u8) -> Self {
        let incidence = (0..bits).into_iter()
            .map(|_| 0)
            .collect();
        GammaRate {
            bits,
            incidence,
            report_count: 0,
        }
    }

    pub fn get_bits(&self) -> u8 {
        self.bits
    }

    pub fn add_bit_vec(&mut self, bits: Vec<(usize, bool)>) {
        if self.bits < bits.len() as u8 {
            self.bits = bits.len() as u8;
        }
        for (i, b) in bits {
            // Verify that indices exist
            if self.incidence.get(i).is_none() {
                self.incidence.insert(i, 0);
            }
            // Increase count if bit is `1`
            if b {
                self.incidence[i] += 1;       
            }
        }
        self.report_count += 1;
    }

    pub fn add(&mut self, n: impl BitRange<u8>) {
        self.report_count += 1;
        // Record values of all bits
        for k in (0..self.bits).rev() {
            let i = (self.bits - k - 1) as usize;
            if n.bit(k as usize) {
                self.incidence[i] += 1;
            }
        }
    }

    pub fn into_gamma_epsilon(self) -> (u32, u32) {
        let mut gamma = u32::MIN;
        let mut epsilon = u32::MIN;
        for (i, n) in self.incidence.into_iter().rev().enumerate() {
            // when incidence is greater than half of our total count, the bit is 1, otherwise 0
            if n >= self.report_count / 2 {
                gamma.set_bit(i, true);
            }
            if n <= self.report_count / 2 {
                epsilon.set_bit(i, true);
            }
        }
        (gamma, epsilon)
    }

    /// Returning tuple of (incidence_count, total_report_count)
    pub fn get_incidence(&self, i: usize) -> (usize, usize) {
        let index = self.bits as usize - i - 1;
        (self.incidence[index], self.report_count)
    }
}

fn get_incidence_bit(bits: u8, v: &Vec<u32>, most_common_bits: bool, index: usize) -> bool {
    let mut rate = GammaRate::new(bits);
    for n in v {
        rate.add(*n);
    }
    let (i, total) = rate.get_incidence(index);

    let zeroes = total - i;
    let ones = i;
    debug_log!("bit incidence -- {} zeroes / {} ones ({} total)", zeroes, ones, total);
    if most_common_bits {
        ones >= zeroes
    } else {
        zeroes > ones
    }
}

fn get_rating(bits: u8, values: Vec<u32>, most_common_bits: bool) -> Option<u32> {
    // Compare mask bitwise to values and filter out non-matching values
    //  until just one remains
    let mut candidates = values;
    debug_log!(
        "   {:#?}",
        candidates.iter()
            .map(|x| format!("{:08b}", x))
            .sorted()
            .collect::<Vec<String>>()
    );

    for i in (0..bits).rev() {
        let i = i as usize;
        let common_bit = get_incidence_bit(bits, &candidates, most_common_bits, i);
        debug_log!("# bit {} --> common: {}\t\t(mcb: {})", i, common_bit as u8, most_common_bits);
        // Keep only numbers with the common_bit at the index
        candidates = candidates.into_iter()
            .filter(|n| n.bit(i) == common_bit)
            .collect();
        debug_log!("## {} candidates left (MCB: {})", candidates.len(), most_common_bits);
        if candidates.len() <= 1 {
            break;
        }
        debug_log!(
            "   {:#?}",
            candidates.iter()
                .map(|x| format!("{:08b}", x))
                .sorted()
                .collect::<Vec<String>>()
        );
    }
    if candidates.len() == 1 {
        return candidates.pop();
    } else {
        return None;
    }
}
