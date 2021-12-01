pub mod log;

#[cfg(debug_assertions)]
use crate::log::LOG_PREFIX;
use std::collections::HashMap;

fn calculate_diffs(input: Vec<u32>) -> Vec<u32> {
    let mut ret = Vec::with_capacity(input.len()-1);
    let mut a: u32 = *input.get(0).unwrap_or(&0);
    for b in input.iter().skip(1) {
        let diff = (*b - a) as u32;
        //log!("diff between {}, {}: {}", a, b, diff);
        ret.push(diff);
        a = *b;
    }
    return ret;
}

fn group_same<'a>(input: &'a [u32]) -> Vec<&'a[u32]> {
    let mut current = input[0].clone();
    let mut last_split = 0;
    let mut ret = vec![input];
    for (i, n) in input.iter().enumerate() {
        if *n != current {
            //log!("splitting at index {}", i);
            // split at i
            let (lhs, rhs) = ret.pop().unwrap().split_at(i - last_split);
            ret.push(lhs);
            ret.push(rhs);
            current = *n;
            last_split = i;
        }
    }
    return ret;
}

static ERR_MASK: u32 = 7;

/// We can image our jolt-diffs as binary states,
/// we know that 3 of those are not allowed in the sense
/// that we cannot skip as many
/// thus we can all options for each input number and then
/// discard those with illegal patterns (0b111 => 7)
fn calc_mult(num: u32) -> u32 {
    let to = 2_u32.pow(num - 1);
    let mut count = to;
    for i in 0..to {
        if i & ERR_MASK == ERR_MASK {
            // not allowed
            count -= 1;
        }
    }
    return count;
}

fn calc_combos(input: &[u64]) -> u64 {
    let mut mult_key: HashMap<u64, u64> = HashMap::new();
    let mut combos: u64 = 1;
    for num in input.iter() {
        let mult = match mult_key.get(num) {
            Some(res) => *res,
            None => {
                // Calculate new value and store it
                let val = calc_mult(*num as u32);
                log!("mult calc for {}: {}", num, val);
                mult_key.insert(*num, val as u64);
                val as u64
            },
        };
        combos = combos * mult;
    }
    return combos;
}

fn main() {
    println!("##### Advent of Code - Day 10 #####");

    let mut adapters = Vec::new();
    let mut buffer = String::new();
    loop {
        buffer.clear();
        if std::io::stdin().read_line(&mut buffer).expect("IO Error.") == 0 { break; }
        match u32::from_str_radix(buffer.trim(), 10) {
            Ok(res) => adapters.push(res),
            Err(e) => {
                println!("{}", e);
                return;
            },
        }
    }

    // Add 0-jolt for charging outlet
    adapters.push(0);

    // Sort input data
    adapters.sort_unstable();
    log!("Adapters: {:?}", &adapters);

    // Add device rated 3 jolts higher than highest currently
    adapters.push(adapters[adapters.len()-1] + 3);

    // Get differences between numbers
    let diff = calculate_diffs(adapters);
    log!("Diffs: {:?}", &diff);
   
    // Group diffs
    {
        let mut grouped = diff.clone();
        grouped.sort_unstable();
        let groups: HashMap<u32, usize> = group_same(&grouped).iter().filter(|x| x.len() > 0).map(|x| (x[0],x.len())).collect();
        log!("Groups: {:?}", groups);
        let diff_jolt1 = groups.get(&1).unwrap_or(&0);
        let diff_jolt3 = groups.get(&3).unwrap_or(&0);
        let result = diff_jolt1 * diff_jolt3;
        println!("[1] => {}", result);
    }

    {
        let skipgroups: Vec<u64> = group_same(&diff).iter().filter(|x| x.len() > 0 && x[0] == 1).map(|x| x.len() as u64).collect();
        log!("skip groups: {:?}", skipgroups);
        let combos = calc_combos(&skipgroups); 
        println!("[2] => {}", combos);
    }
}
