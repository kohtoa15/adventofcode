use std::rc::Rc;
use std::sync::Mutex;
use std::convert::TryFrom;

#[derive(Copy, Clone)]
enum Operation{
    Acc, Jmp, Nop
}

impl<'a> TryFrom<&'a str> for Operation{
    type Error = &'static str;

    fn try_from(input: &'a str) -> Result<Self, Self::Error> {
        match input {
            "jmp" => Ok(Operation::Jmp),
            "acc" => Ok(Operation::Acc),
            "nop" => Ok(Operation::Nop),
            _ => Err("Illegal instruction code provided!"),
        }
    }
}

impl std::fmt::Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let v = match self {
            Operation::Acc => "ACC",
            Operation::Jmp => "JMP",
            Operation::Nop => "NOP",
        };
        write!(f, "{}", v)
    }
}

struct Instruction(Operation, i32);

impl Instruction {
    pub fn from(op: Operation, arg: i32) -> Instruction { Instruction(op, arg) }

    pub fn execute(&self, accumulator: &mut i32, instruction_ptr: &mut usize) {
        let (op, arg) = (self.0, self.1);
        match op {
            Operation::Acc => *accumulator += arg,
            Operation::Jmp => {
                if arg >= 0 {
                    *instruction_ptr = instruction_ptr.wrapping_add(arg as usize);
                } else {
                    *instruction_ptr = instruction_ptr.wrapping_sub(arg.wrapping_abs() as usize);
                }
            },
            Operation::Nop => {},
        };
    }

    pub fn flip_jmp_nop(&mut self) -> bool {
        let op = &mut self.0;
        match op {
            Operation::Jmp => {
                *op = Operation::Nop;
                true
            },
            Operation::Nop => {
                *op = Operation::Jmp;
                true
            },
            _ => false,
        }
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.0, self.1)
    }
}

fn parse_instruction(input: &str) -> Result<Instruction, &'static str> {
    let mid = input.trim().find(' ').ok_or("Instruction has no argument!")?;
    let (op_str, arg_str) = input.split_at(mid);
    // Parse operation
    let op = Operation::try_from(op_str.trim())?;
    let arg = i32::from_str_radix(arg_str.trim(), 10).map_err(|_| "Instruction argument is not a valid number!")?;
    // Return instruction
    Ok( Instruction::from(op, arg) )
}

struct HandheldEmulator {
    execution_history: Vec<usize>, // Saves previous IP values
    program: Rc<Mutex<Vec<Instruction>>>,
    accumulator: i32,
    instruction_ptr: usize,
    exited: Option<bool>,
}

impl HandheldEmulator {
    pub fn init(program: Rc<Mutex<Vec<Instruction>>>) -> HandheldEmulator {
        HandheldEmulator {
            execution_history: Vec::new(),
            program,
            accumulator: 0,
            instruction_ptr: 0,
            exited: None,
        }
    }

    fn reset(&mut self) {
        self.accumulator = 0;
        self.instruction_ptr = 0;
        self.execution_history.clear();
        self.exited = None;
    }

    pub fn run_boot_code(&mut self, verbose: bool) -> (bool, i32)
    {
        self.reset();
        loop {
            if self.check_termination() { break; } 
            self.execute_next(verbose); 
            // Halt execution when the code loops
            if self.execution_history.iter().any(|ip| *ip == self.instruction_ptr) {
                break;
            }
        }
        // Returning whatever value was last in the accumulator
        return (self.exited.unwrap_or(false), self.accumulator);
    }

    fn execute_next(&mut self, verbose: bool) {
        // Execute instruction that the instruction pointer is pointing to
        self.execution_history.push(self.instruction_ptr);
        let ip_prev = self.instruction_ptr.clone();
        let instruction = &self.program.lock().unwrap()[self.instruction_ptr];
        instruction.execute(&mut self.accumulator, &mut self.instruction_ptr);
        // If instruction pointer has not been changed, increase by 1
        if self.instruction_ptr == ip_prev {
            self.instruction_ptr += 1;
        }
        if verbose {
            println!("---\n  executed [{}]", instruction);
            println!("  ACM: {}\tIP: {}", self.accumulator, self.instruction_ptr);
        }
    }

    fn check_termination(&mut self) -> bool {
        // When the program is trying to execute the instruction after the last,
        // it is terminating without error
        if self.instruction_ptr == self.program.lock().unwrap().len() {
            self.exit(true);
        }
        self.exited.is_some()
    }

    fn exit(&mut self, successful: bool) {
        self.exited = Some(successful);
    }
}

fn fix_program(program: Rc<Mutex<Vec<Instruction>>>, verbose: bool) -> Option<i32> {
    let mut emulator = HandheldEmulator::init(Rc::clone(&program));
    // Flip one instruction until the execution is successful
    let len = program.lock().unwrap().len();
    for i in 0..len {
        {
            let instr = &mut program.lock().unwrap()[i];
            // If instruction cannot be flipped, advance on to next
            if !instr.flip_jmp_nop() {
                continue;
            }
        }
        if verbose { println!("# Try flipping instruction [{}] ...", i); }
        // Try execution
        let (success, acm) = emulator.run_boot_code(verbose); 
        if success {
            println!("  Flipped instruction [{}] to fix the program.", i);
            return Some(acm);
        }
        // Flip instruction back to original
        program.lock().unwrap()[i].flip_jmp_nop();
    }
    // Program can't be fixed
    return None;
}

fn eval_args() -> Result<(Option<usize>, bool), String> {
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

    let part;
    if num > 0 {
        use std::str::FromStr;
        // last arg should be part id
        let res = usize::from_str(&last_arg).map_err(|e| e.to_string())?;
        part = Some(res);
    } else {
        part = None;
    }
    return Ok((part, verbose));
}

fn main() {
    let (part, verbose) = eval_args().expect("USAGE: solve [-v] [1 | 2]");
    println!("##### Advent of Code - Day 8 #####");
    let mut instructions = Vec::new();
    let mut buffer = String::new();
    let mut line_cnt: usize = 0;
    loop {
        buffer.clear();
        let n = std::io::stdin().read_line(&mut buffer).expect("IO error occurred!");
        if n == 0 { break; }
        match parse_instruction(buffer.as_str()) {
            Ok(instr) => instructions.push(instr),
            Err(e) => {
                println!("Error occurred on line {}: {}", line_cnt, e);
                return;
            },
        };
        line_cnt += 1;
    }

    // Get accumulator value before first loop
    let program = Rc::new(Mutex::new(instructions));
    if part.is_none() || part.unwrap() == 1 {
        let mut em = HandheldEmulator::init(Rc::clone(&program));
        let (_, result) = em.run_boot_code(verbose);
        println!("[1] => {}", result);
    }
    if part.is_none() || part.unwrap() == 2 {
        println!("-- Part 2:");
        let result = fix_program(program, verbose);
        println!("[2] => {}", result.map(|v| v.to_string()).unwrap_or(String::from("None")));
    }
}
