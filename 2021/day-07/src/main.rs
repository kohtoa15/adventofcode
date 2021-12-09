#![feature(fn_traits)]

macro_rules! debug_log {
    ($($arg:tt)*) => {
        #[cfg(debug_assertions)]
        {
            println!($($arg)*);
        }
    };
}

use std::{collections::HashMap, sync::{Arc, Mutex}};
#[cfg(debug_assertions)]
use std::time::Instant;

use tokio::{io::BufReader, task::JoinHandle};

#[tokio::main]
async fn main() {
    use tokio::io::AsyncBufReadExt;
    let reader = BufReader::new(tokio::io::stdin());
    let mut lines = reader.lines();

    let mut crabs = Vec::new();

    while let Some(line) = lines.next_line().await.expect("IO error") {
        let s: Vec<&str> = line.trim()
            .split(",")
            .collect();
        for k in s {
            crabs.push(u32::from_str_radix(k, 10).expect("Could not parse input"));
        }
    }
    // Wrap crab position vec into concurrent smart pointer
    let crabs = Arc::new(crabs);

    {
        #[cfg(debug_assertions)]
        let start = Instant::now();
        let target_board = SharedTargetBoard::default();
        let mut handles: Vec<JoinHandle<()>> = Vec::new();

        // Try to find out best alignment with parallel tasks
        let min = crabs.iter().min().map(|x| *x).unwrap_or_default();
        let max = crabs.iter().max().map(|x| *x).unwrap_or_default();
        for target in min..max {
            let hdl = spawn_aligment_task(target, &crabs, &target_board, ||);
            handles.push(hdl);
        }

        // Wait for all jobs to finish
        for h in handles {
            let _ = h.await.unwrap();
        }

        debug_log!("parallel execution took {}µs", start.elapsed().as_micros());
        println!(
            "1) Determine the horizontal position that the crabs can align to using the least fuel possible.\n\tHow much fuel must they spend to align to that position?\n  --> Answer: {}",
            target_board.get_cheapest(),
        );
    }

    {
        #[cfg(debug_assertions)]
        let start = Instant::now();
        let target_board = SharedTargetBoard::default();
        let mut handles: Vec<JoinHandle<()>> = Vec::new();

        // Try to find out best alignment with parallel tasks
        let min = crabs.iter().min().map(|x| *x).unwrap_or_default();
        let max = crabs.iter().max().map(|x| *x).unwrap_or_default();
        for target in min..max {
            let hdl = spawn_aligment_task(target, &crabs, &target_board, triangular_number);
            handles.push(hdl);
        }

        // Wait for all jobs to finish
        for h in handles {
            let _ = h.await.unwrap();
        }

        debug_log!("parallel execution took {}µs", start.elapsed().as_micros());
        println!(
            "2) Determine the horizontal position that the crabs can align to using the least fuel possible so they can make you an escape route!\n\tHow much fuel must they spend to align to that position?\n  --> Answer: {}",
            target_board.get_cheapest(),
        );
    }

}

#[derive(Clone, Default)]
struct SharedTargetBoard {
    fuel_results: Arc<Mutex<HashMap<u32, u32>>>,
}

impl SharedTargetBoard {
    pub fn post_result(&self, target: u32, fuel_cost: u32) {
        self.fuel_results.lock().unwrap().insert(target, fuel_cost);
    }

    /// Locks the results tab and returns the smallest fuel cost value
    pub fn get_cheapest(&self) -> u32 {
        let fuel_costs = self.fuel_results.lock().unwrap();
        let (_, cost) = fuel_costs.iter()
            .min_by(|(_,v), (_,other)| v.cmp(other))
            .map(|(k,v)| (*k,*v))
            .unwrap_or_default();
        cost
    }
}

fn spawn_aligment_task<F>(target: u32, crabs: &Arc<Vec<u32>>, target_board: &SharedTargetBoard, closure: F) -> JoinHandle<()>
    where F: Fn(u32) -> u32 + Send + 'static
{
    let pos = crabs.clone();
    let res = target_board.clone();
    tokio::spawn(async move {
        #[cfg(debug_assertions)]
        let start =  Instant::now();
        // calc diffs to target
        let fuel_cost: u32 = pos.iter()
            .map(|pos| {
                let n = target.max(*pos) - target.min(*pos);
                closure.call((n,))
            })
            .sum();
        res.post_result(target, fuel_cost);
        debug_log!("task with target {} has finished. (result = {}) -- took {}µs", target, fuel_cost, start.elapsed().as_micros());
    })
}

fn triangular_number(x: u32) -> u32 {
    (x.pow(2) + x) / 2
}
