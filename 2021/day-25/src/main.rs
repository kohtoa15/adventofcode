use std::{cell::Cell, collections::HashMap};

enum Direction {
    East, South,
}

impl Direction {
    pub fn get_delta(&self) -> (u32, u32) {
        match self {
            Self::East => (1, 0),
            Self::South => (0, 1),
        }
    }
}

struct SeaCucumber {
    pub x: u32,
    pub y: u32,
    pub facing: Direction,
    pub is_moving: Cell<bool>,
}

impl SeaCucumber {
    pub fn new(x: u32, y: u32, facing: Direction) -> Self {
        Self { x, y, facing, is_moving: Cell::from(false) }
    }

    pub fn try_to_move(&self, state: &SeaCucumberState) -> bool {
        let _ = self.is_moving.take(); // set false by default
        let delta = self.facing.get_delta();
        let dx = self.x + delta.0;
        let dy = self.y + delta.1;
        if state.is_empty(dx, dy) {
            let _ = self.is_moving.replace(true);
        }
        // report back move outcome
        return self.is_moving.get();
    }

    // Moves SeaCucumber if it has the is_moving flag set
    pub fn move_cucumber(&mut self) {
        if self.is_moving.take() {
            let delta = self.facing.get_delta();
            self.x += delta.0;
            self.y += delta.1;
        }
    }
}

struct SeaCucumberState {
    herd_east: Vec<SeaCucumber>,
    herd_south: Vec<SeaCucumber>,
    width: u32,
    height: u32,
}

impl SeaCucumberState {
    fn normalize_position(&self, x: u32, y: u32) -> (u32, u32) {
        (x % self.width, y % self.height)
    }

    fn any_cucumber_on_position(&self, cucumbers: &Vec<SeaCucumber>, position: (u32, u32)) -> bool {
        cucumbers.iter().any(|c| {
            let (cx, cy) = self.normalize_position(c.x, c.y);
            cx == position.0 && cy == position.1
        }) 
    }

    pub fn is_empty(&self, x: u32, y: u32) -> bool {
        let pos = self.normalize_position(x, y);
        if self.any_cucumber_on_position(&self.herd_east, pos) {
            // already someone there
            return false;
        }
        if self.any_cucumber_on_position(&self.herd_south, pos) {
            // already someone there
            return false;
        }
        // no sea cucumber found there
        true
    }

    fn check_cucumber_moves(&self, cucumbers: &Vec<SeaCucumber>) -> u32 {
        let mut total_moves = 0;
        for c in cucumbers.iter() {
            if c.try_to_move(&self) {
                total_moves += 1;
            }
        }
        return total_moves;
    }

    // Returns the number of moves
    pub fn move_all_easterners(&mut self) -> u32 {
        let ret = self.check_cucumber_moves(&self.herd_east);
        if ret > 0 {
            for c in self.herd_east.iter_mut() {
                c.move_cucumber();
            }
        }
        return ret;
    }

    // Returns the number of moves
    pub fn move_all_southerners(&mut self) -> u32 {
        let ret = self.check_cucumber_moves(&self.herd_south);
        if ret > 0 {
            for c in self.herd_south.iter_mut() {
                c.move_cucumber();
            }
        }
        return ret;
    }

    pub fn move_one_step(&mut self) -> u32 {
        let n = self.move_all_easterners();
        let m = self.move_all_southerners();
        return n + m;
    }
}

struct Grid {
    empty_tokens: Vec<char>,
    width: Option<u32>,
    height: Option<u32>,
    tracked_tokens: HashMap<char, Vec<(u32, u32)>>,
}

impl Grid {
    pub fn new(empty_tokens: Vec<char>, tracked: Vec<char>) -> Self {
        let mut tracked_tokens = HashMap::with_capacity(tracked.len());
        for t in tracked.into_iter() {
            tracked_tokens.insert(t, Vec::new());
        }
        Self { empty_tokens, tracked_tokens, width: None, height: None }
    }

    pub fn read_from_string(&mut self, input: &String) -> std::io::Result<()> {
        for (y, line) in input.lines().enumerate() {
            if let Some(width) = &self.width {
                assert_eq!(*width as usize, line.len());
            } else {
                // Set new width
                let _ = self.width.replace(line.len() as u32);
            }

            for (x, c) in line.char_indices() {
                if let Some(v) = self.tracked_tokens.get_mut(&c) {
                    v.push((x as u32, y as u32));
                } else if !self.empty_tokens.contains(&c) {
                    // invalid input token
                    return Err(std::io::Error::new(std::io::ErrorKind::InvalidData, format!("invalid grid token at {},{}", x, y)));
                }
            }
            // update height with each newline
            let _ = self.height.replace((y + 1) as u32);
        }
        Ok(())
    }
}

impl TryFrom<Grid> for SeaCucumberState {
    type Error = std::io::Error;

    fn try_from(grid: Grid) -> Result<Self, Self::Error> {
        use std::io::Error;
        use std::io::ErrorKind;
        Ok(SeaCucumberState {
            herd_east: grid.tracked_tokens.get(&'>')
                .ok_or(Error::new(ErrorKind::InvalidData, "grid has no east cucumbers"))?
                .into_iter().map(|(x, y)| SeaCucumber::new(*x, *y, Direction::East)).collect(),
            herd_south: grid.tracked_tokens.get(&'v')
                .ok_or(Error::new(ErrorKind::InvalidData, "grid has no south cucumbers"))?
                .into_iter().map(|(x, y)| SeaCucumber::new(*x, *y, Direction::South)).collect(),
            width: grid.width
                .ok_or(Error::new(ErrorKind::InvalidInput, "grid has no width"))?,
            height: grid.height
                .ok_or(Error::new(ErrorKind::InvalidInput, "grid has no height"))?,
        })
    }
}

fn main() -> std::io::Result<()> {
    let filename = std::env::args().skip(1).next().unwrap_or("input".to_string());
    let content = std::fs::read_to_string(filename)?;
    println!("file imported.");
    let mut grid = Grid::new(vec!['.'], vec!['>', 'v']);
    grid.read_from_string(&content)?;
    println!("grid imported.");
    let mut cucumber_state = SeaCucumberState::try_from(grid)?;
    println!("state converted.");
    let mut steps = 0;
    loop {
        steps += 1;
        let moves = cucumber_state.move_one_step();
        println!("step {}: {} moves", steps, moves);
        if moves == 0 {
            // step without any moves
            println!("a. What is the first step on which no sea cucumbers move? - {}", steps);
            break;
        }
        // escape hatch
        if steps >= 1000000 {
            println!("Not any step without sea cucumbers moving in {} steps", steps);
            break;
        }
    }

    Ok(())
}
