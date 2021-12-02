use std::error::Error;
macro_rules! debug_log {
    ($($arg:tt)*) => {
        #[cfg(debug_assertions)]
        {
            println!($($arg)*);
        }
    };
}

use tokio::io::BufReader;

#[tokio::main]
async fn main() {
    // Task 1
    let mut pos = Position::default();
    // Task 2
    let mut aim = Aim::default();

    use tokio::io::AsyncBufReadExt;
    let reader = BufReader::new(tokio::io::stdin());
    let mut lines = reader.lines();
    while let Some(line) = lines.next_line().await.expect("IO error") {
        let instr = instr_from_str(line.as_str()).expect("Could not parse input");
        debug_log!("{}\n--> {:?}", "*".repeat(60), &instr);
        pos.apply_step(instr);
        debug_log!("current pos #1: {:?}", pos.to_pos_tuple());
        aim.apply_step(instr);
        debug_log!(
            "current pos #2: {:?} (aim: {})",
            aim.to_pos_tuple(),
            aim.get_aimed_depth()
        );
    }
    // Calculate product of both dimensions for pos (#1)
    {
        let (x, y) = pos.to_pos_tuple();
        println!(
            "1) What do you get if you multiply your final horizontal position by your final depth?\n --> Answer: {}",
            x * y
        );
    }

    // Calculate product of both dimensions for aim (#2)
    {
        let (x, y) = aim.to_pos_tuple();
        println!(
            "2) What do you get if you multiply your final horizontal position by your final depth?\n --> Answer: {}",
            x * y
        );
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Direction {
    Forward,
    Down,
    Up,
}

impl TryFrom<&str> for Direction {
    type Error = std::io::Error;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "forward" => Ok(Self::Forward),
            "down" => Ok(Self::Down),
            "up" => Ok(Self::Up),
            _ => Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "invalid direction",
            )),
        }
    }
}

type Instruction = (Direction, u32);

fn instr_from_str(input: &str) -> Result<Instruction, Box<dyn Error>> {
    let mut words: Vec<&str> = input.split_whitespace().collect();
    // Try to get direction from word 0
    let dir = Direction::try_from(words.remove(0))?;
    // Try to parse word 1 to int
    let val = u32::from_str_radix(words.remove(0), 10)?;
    Ok((dir, val))
}

pub trait ApplyInstruction {
    fn apply_step(&mut self, i: Instruction);
}

pub trait PositionTuple {
    fn to_pos_tuple(&self) -> (u32, u32);
}

#[derive(Default)]
struct Position {
    horizontal: u32,
    depth: u32,
}

impl PositionTuple for Position {
    /// Transforms the Position into a tuple of (horizontal, depth)
    fn to_pos_tuple(&self) -> (u32, u32) {
        (self.horizontal, self.depth)
    }
}

impl ApplyInstruction for Position {
    fn apply_step(&mut self, i: Instruction) {
        let (dir, x) = i;
        match dir {
            Direction::Forward => self.horizontal += x,
            Direction::Down => self.depth += x,
            Direction::Up => self.depth -= x,
        }
    }
}

#[derive(Default)]
struct Aim {
    // aimed depth
    aim: u32,
    // actual current depth
    depth: u32,
    horizontal: u32,
}

impl PositionTuple for Aim {
    fn to_pos_tuple(&self) -> (u32, u32) {
        (self.horizontal, self.depth)
    }
}

impl ApplyInstruction for Aim {
    fn apply_step(&mut self, i: Instruction) {
        let (dir, x) = i;
        match dir {
            Direction::Forward => {
                // Increase hor pos by x units
                self.horizontal += x;
                // Increase depth by aim * x
                self.depth += self.aim * x;
            }
            // Decrease aimed depth
            Direction::Up => self.aim -= x,
            // Increase aimed depth
            Direction::Down => self.aim += x,
        }
    }
}

#[cfg(debug_assertions)]
impl Aim {
    pub fn get_aimed_depth(&self) -> u32 {
        self.aim
    }
}
