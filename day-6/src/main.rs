use std::collections::HashSet;
use std::thread::{JoinHandle};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

#[cfg(debug_assertions)]
const LOGGING: bool = true;
#[cfg(not(debug_assertions))]
const LOGGING: bool = false;

macro_rules! log {
    ($($arg:tt)*) => {
        if LOGGING {
            print!("# ");
            println!($($arg)*);
        }
    };
}

struct LineBacklog {
    backlog: Arc<Mutex<Vec<String>>>,
    ready: AtomicBool,
}

impl LineBacklog {
    pub fn new() -> LineBacklog { LineBacklog { backlog: Arc::new(Mutex::new(Vec::new())), ready: AtomicBool::new(false), } }
    
    pub fn feed_line(&self, line: String) {
        log!("[line-backlog] feeding line...");
        {
            let mut handle = self.backlog.lock().unwrap();
            handle.insert(0, line.trim().to_string());
        }
        self.ready.store(true, Ordering::SeqCst);
    }

    pub fn poll_line(&self) -> Option<String> {
        log!("[line-backlog] polling line...");
        let mut handle = self.backlog.lock().unwrap();
        let ret = handle.pop();
        if handle.is_empty() {
            self.ready.store(false, Ordering::SeqCst);
        }
        log!("[line-backlog] {} items left after poll", handle.len());
        return ret;
    }

    pub fn is_ready(&self) -> bool { self.ready.load(Ordering::SeqCst) }
}

#[derive(Debug)]
enum Policy { Any, Every }

struct GroupManager {
    groups: Vec<HashSet<char>>,
    current: Option<HashSet<char>>,
    policy: Policy,
}

impl GroupManager {
    pub fn new(policy: Policy) -> GroupManager {
        GroupManager {
            groups: Vec::new(),
            current: None,
            policy,
        }
    }

    fn append_any(&mut self, data: String) {
        if self.current.is_none() {
            self.current = Some(HashSet::new());
        }
        log!("[group-mgr] appending {} to group", data.as_str());
        // Insert every element that hasn't been added yet
        for c in data.chars() {
            if let Some(current) = &mut self.current {
                current.insert(c);
            }
        }
    }

    fn append_every(&mut self, data: String) {
        // If the collection is empty, add all elements
        if self.current.is_none() {
            self.append_any(data);
            return;
        }
        // otherwise, remove all current elements that aren't in 'data'
        if let Some(current) = &mut self.current {
            // Remove all elements of current that are not in 'data'
            log!("[group-mgr] new input {}", data.as_str());
            let mut to_remove: HashSet<char> = HashSet::new();
            for el in current.iter() {
                // Check if 'el' can be found in 'data'
                if data.find(*el).is_none() {
                    to_remove.insert(*el);
                }
            }
            log!("[group-mgr] removing {:?} from group", &to_remove);
            for rem in to_remove.into_iter() {
                current.remove(&rem);
            }
        }
    }

    fn process_line(&mut self, line: String) {
        log!("[group-mgr] processing line...");
        if line.is_empty() {
            // Signals new group
            self.next_group();
        } else {
            match &self.policy {
                Policy::Any => self.append_any(line),
                Policy::Every => self.append_every(line),
            };
        }
    }

    fn next_group(&mut self) {
        log!("[group-mgr] advancing to next group...");
        if let Some(current) = self.current.take() {
            self.groups.push(current);
        }
    }

    pub fn drain(mut self) -> Vec<HashSet<char>> { 
        log!("[group-mgr] draining groups...");
        self.next_group();
        return self.groups;
    }
}

const WORKER_SLEEP_MS: u64 = 1;

fn start_worker(policy: Policy, backlog: Arc<LineBacklog>, end: Arc<AtomicBool>) -> JoinHandle<Vec<HashSet<char>>> {
    log!("[worker] starting worker...");
    std::thread::spawn(move || {
        let sleep_time = std::time::Duration::from_millis(WORKER_SLEEP_MS);
        let mut mgr = GroupManager::new(policy);
        while !end.load(Ordering::SeqCst) || backlog.is_ready() {
            if backlog.is_ready() {
                // poll next line
                log!("[worker] getting next line...");
                if let Some(line) = backlog.poll_line() {
                    // pass line to group manager
                    mgr.process_line(line);
                }
            } else {
                // wait for next poll
                log!("[worker] waiting for backlog...");
                std::thread::sleep(sleep_time);
            }
        }
        // Processed all lines, returning groups
        mgr.drain()
    })
}

const DEFAULT_POLICY: Policy = Policy::Every;

fn eval_args() -> Result<(Policy, bool), String> {
    let mut num: usize = 0;
    let mut last_arg: String = String::new();
    let mut found_v = None;
    for (i, arg) in std::env::args().enumerate() {
        num = i;
        if found_v.is_none() && arg == "-v" {
            found_v = Some(i);
        } else {
            last_arg = arg;
        }
    }

    let verbose = found_v.is_some();
    if let Some(i) = found_v {
        if num == i {
            // in case '-v' was the last argument, decrease the counter
            num -= 1;
        }
    }

    if num > 0 {
        use std::str::FromStr;
        // last arg should be part id
        let res = usize::from_str(&last_arg).map_err(|e| e.to_string())?;
        match res {
            1 => Ok((Policy::Any, verbose)),
            2 => Ok((Policy::Every, verbose)),
            _ => Err(String::from("Accepted variant options: [1, 2]")),
        }
    } else {
        return Ok((DEFAULT_POLICY, verbose));
    }
}

fn main() {
    println!("##### Advent of Code 2020 - Day 6 #####");
    let policy: Policy;
    let verbose: bool;
    match eval_args() {
        Ok((res, v)) => {
            println!("Variant: {:?}", res);
            policy = res;
            verbose = v;
        }
        Err(e) => {
            println!("Wrong arguments! {}", e);
            return;
        },
    };

    let lines = Arc::new(LineBacklog::new());
    let end = Arc::new(AtomicBool::new(false));
    let worker = start_worker(policy, Arc::clone(&lines), Arc::clone(&end));
    let mut buffer = String::new();
    loop {
        match std::io::stdin().read_line(&mut buffer) {
            Ok(n) => {
                if n == 0 {
                    log!("[main] reached eof...");
                    // eof
                    break;
                }
                lines.feed_line(buffer.clone());
                log!("[main] fed line to backlog.");   
                buffer.clear();
            },
            Err(e) => println!("Error: {}", e),
        }
    }
    log!("[main] stdin finished.");

    // signal that end of lines is reached
    end.store(true, Ordering::SeqCst);
    log!("[main] signal end to worker");

    match worker.join() {
        Ok(res) => {
            let mut sum: usize = 0;
            for g in res.into_iter() {
                if verbose {
                    print!("Group:");
                    for x in g.iter() {
                        print!(" {}", x);
                    }
                    println!(" [{} elements]", g.len());
                }
                sum += g.len();
            }
            println!("Result: {}", sum);
        },
        Err(_) => println!("Error while closing thread..."),
    };
}
