macro_rules! debug_log {
    ($($arg:tt)*) => {
        #[cfg(debug_assertions)]
        {
            println!($($arg)*);
        }
    };
}

use std::{error::Error, fmt::Display};

use tokio::io::BufReader;

#[tokio::main]
async fn main() {
    use tokio::io::AsyncBufReadExt;
    let reader = BufReader::new(tokio::io::stdin());
    let mut lines = reader.lines();

    let mut bingo = BingoSubsystem::default();

    #[cfg(debug_assertions)]
    let mut count = 0;

    while let Some(line) = lines.next_line().await.expect("IO error") {
        #[cfg(debug_assertions)]
        {
            print!("line {} - ", count);
            count += 1;
        }
        bingo.feed_input(line).expect("Could not parse input");
    }
    let _ = bingo.flush_cache();

    // Go through draws until we have a winner
    let ranked = bingo.draw_to_end();
    let winner = ranked.first().unwrap();
    let last = ranked.last().unwrap();
    println!(
        "1) To guarantee victory against the giant squid, figure out which board will win first.\n\tWhat will your final score be if you choose that board?\n  --> Answer: {}",
        winner.score
    );
    println!(
        "2) Figure out which board will win last.\n\tOnce it wins, what would its final score be?\n  --> Answer: {}",
        last.score
    );
}

fn parse_nums_from_vec<'a>(input: impl Iterator<Item=&'a str>) -> Result<Vec<u8>, Box<dyn Error>> {
    let mut values = Vec::new();
    // Unpack errors
    for val in input {
        let n = u8::from_str_radix(val, 10)?;
        values.push(n);
    }
    debug_log!("\tread {} values", values.len());
    return Ok(values);

}

fn read_comma_separated_values(line: String) -> Result<Vec<u8>, Box<dyn Error>> {
    let vals = line.trim()
        .split(",")
        .filter(|x| x.len() > 0);
    parse_nums_from_vec(vals)
}

fn read_space_separated_values(line: String) -> Result<Vec<u8>, Box<dyn Error>> {
    let vals = line.trim()
        .split_whitespace()
        .filter(|x| x.len() > 0);
    parse_nums_from_vec(vals)
}

#[derive(Default)]
struct BingoSubsystem {
    draws: Vec<u8>,
    boards: Vec<Option<BingoBoard>>,
    bingo_cache: Option<Vec<Vec<u8>>>,
}

impl BingoSubsystem {
    pub fn feed_input(&mut self, line: String) -> Result<(), Box<dyn Error>> {
        // If no draws yet, parse input as draws
        if self.draws.is_empty() {
            debug_log!("# reading draws as csv");
            let mut other = read_comma_separated_values(line)?;
            self.draws.append(&mut other);
            debug_log!("\t draws: {:?}", &self.draws);
        } else if self.bingo_cache.is_none() {
            if line.is_empty() {
                debug_log!("# skipping empty line after draws");
                return Ok(());
            }
            debug_log!("# reading new bingo board into cache");
            // Enter to new bingo cache
            let row = read_space_separated_values(line)?;
            self.bingo_cache.replace(vec![row]);
        } else if self.bingo_cache.is_some() {
            // Add as new row to existing bingo cache
            if line.is_empty() {
                debug_log!("# converting bingo cache into board");
                // Try to create new bingo board from cached values
                self.flush_cache()?;
            } else {
                debug_log!("# reading values into bingo cache");
                // Add to bingo cache
                let row = read_space_separated_values(line)?;
                let cache = self.bingo_cache.as_mut().unwrap();
                cache.push(row);
            }
        }
        Ok(())
    }

    /// Called to try and parse board from bingo cache (if it's filled)
    pub fn flush_cache(&mut self) -> Result<(), Box<dyn Error>> {
        if let Some(cache) = self.bingo_cache.take() {
            let board = BingoBoard::try_from(cache)?;   
            debug_log!("\tBoard: {}", &board);
            self.boards.push(Some(board));
        }
        Ok(())
    }

    /// Remove draws sequentially and apply them to all boards until all boards have finished
    /// Returns all individual board results in the ranked order
    pub fn draw_to_end(&mut self) -> Vec<BingoResult> {
        let mut results = Vec::with_capacity(self.boards.len());
        // Flip order in our draw queue, so we can just pop our last elements until we have gone
        // through all of them
        self.draws.reverse();
        while let Some(draw) = self.draws.pop() {
            debug_log!("# drawing next number: {}", draw);
            // Apply to all boards
            // also check if we had any hits
            let mut hits: Vec<usize> = Vec::new();
            for (i, board_opt) in self.boards.iter_mut().enumerate() {
                if let Some(board) = board_opt {
                    if board.mark_number(draw) {
                        // Mark board with this index as hit
                        hits.push(i);
                    }
                }
            }
            // Check if any boards that has a hit has finished already
            for i in hits {
                if self.boards[i].as_mut().unwrap().has_finished() {
                    // create result for board and remove it from the active ones
                    if let Some(result) = self.finish_board(i, draw as u32) {
                        results.push(result);
                    }
                }
            }
        }
        // Return the bingo results
        return results;
    }

    /// Removes the board at the index and calculates the score
    fn calculate_score(&mut self, index: usize, last_draw: u32) -> Option<(BingoBoard, u32)> {
        if let Some(board) = self.boards[index].take() {
            debug_log!("# board {} finished\n\t{}", index + 1, board);
            if let Some(unmarked_sum) = board.get_score() {
                debug_log!("# remaining numbers: {}, last_draw: {}", unmarked_sum, last_draw);
                let score = unmarked_sum * last_draw;
                debug_log!("# score: {}", score);
                return Some((board, score));
            }
        }
        None
    }

    /// creates result if board at the index has finished
    fn finish_board(&mut self, index: usize, last_draw: u32) -> Option<BingoResult> {
        self.calculate_score(index, last_draw)
            .map(|(board, score)| {
                BingoResult { board, score, index }
            })
    }
}

#[allow(dead_code)]
struct BingoResult {
    board: BingoBoard,
    score: u32,
    index: usize,
}

#[derive(Debug, Clone)]
struct BingoBoard {
    /// Number board -> None represents number that has already been drawn
    nums: [[Option<u8>; 5]; 5],
    has_finished: bool,
}

impl Display for BingoBoard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let nums_str: Vec<String> = self.nums.clone()
            .into_iter()
            .map(|v| {
                let row: Vec<String> = v.into_iter()
                 .map(|x| x.map(|n| n.to_string()).unwrap_or("X".to_string()) )
                 .collect();
                row.join("\t")
            })
            .collect();
        let rows = nums_str.join("\n\t");
        write!(f, 
            "BingoBoard <\n\t{}\n\t>",
            rows
        )
    }
}

impl TryFrom<Vec<Vec<u8>>> for BingoBoard {
    type Error = std::io::Error;
    fn try_from(value: Vec<Vec<u8>>) -> Result<Self, Self::Error> {
        if value.len() != 5 {
            return Err(std::io::Error::new(std::io::ErrorKind::Other, "wrong number of rows"));
        }
        let mut nums: [[Option<u8>; 5]; 5] = [[None; 5]; 5];
        // Enter values into nums
        for (y, row) in value.into_iter().enumerate() {
            if row.len() != 5 {
                return Err(std::io::Error::new(std::io::ErrorKind::Other, "wrong number of cols"));
            }
            for (x, col) in row.into_iter().enumerate() {
                nums[y][x].replace(col);
            }
        }
        Ok(BingoBoard {
            nums,
            has_finished: false,
        })
    }
}

impl BingoBoard {
    pub fn mark_number(&mut self, draw: u8) -> bool {
        for x in 0..5 {
            for y in 0..5 {
                if let Some(num) = &self.nums[y][x] {
                    if *num == draw {
                        self.nums[y][x].take();
                        return true;
                    }
                }
            }
        }
        return false;
    }

    /// Returns true if number at position has been drawn already
    fn number_drawn(&self, row: usize, col: usize) -> bool {
        self.nums[row][col].is_none()
    }

    pub fn has_finished(&mut self) -> bool {
        if self.has_finished {
            return self.has_finished;
        }
        // Check horizontal rows
        for row in 0..5 {
            let full_row = (0..5).map(|col| (row, col))
                .all(|(row, col)| self.number_drawn(row, col));
            if full_row {
                self.has_finished = true;
                return self.has_finished;
            }
        }
        // Check vertical columns
        for col in 0..5 {
            let full_column = (0..5).map(|row| (row, col))
                .all(|(row, col)| self.number_drawn(row, col));
            if full_column {
                self.has_finished = true;
                return self.has_finished;
            }
        }
        /* Note: diagonals DON'T count
        // Check 2 diagonals
        let rows = 0..5;
        let cols = 0..5;
        if rows.clone().zip(cols.clone()).all(|(row, col)| self.number_drawn(row, col)) ||
            rows.zip(cols.rev()).all(|(row, col)| self.number_drawn(row, col)) {
                self.has_finished = true;
                return self.has_finished;
        }
        */
        // No full rows/cols found -> not won yet
        return false;
    }

    pub fn get_score(&self) -> Option<u32> {
        if self.has_finished {
            let mut sum: u32 = 0;
            for x in 0..5 {
                for y in 0..5 {
                    sum += self.nums[y][x].unwrap_or_default() as u32;
                }
            }
            Some(sum)
        } else {
            None
        }
    }
}
