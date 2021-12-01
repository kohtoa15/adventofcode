use std::collections::HashMap;

macro_rules! log {
    ($($arg:tt)*) => {
        #[cfg(debug_assertions)]
        {
            println!($($arg)*);
        }
    }
}

type SeatId = u64;

struct Seat {
    occupied: bool,
    next: Option<bool>,
    state_changed: bool,
}

impl Seat {
    pub fn new() -> Seat {
        Seat {
            occupied: false,
            next: None,
            state_changed: true,
        }
    }

    pub fn is_occupied(&self) -> bool {
        self.occupied
    }

    pub fn determine_next(&mut self, adjacent: usize, tolerant: bool) {
        let allowed_adj = if tolerant { 5 } else { 4 };
        if !self.occupied && adjacent == 0 {
            // If a seat is empty and there are no occupied seats adjacent to it, the seat becomes occupied.
            self.next = Some(true);
            // println!("occupying seat... ({} adj)", adjacent);
        } else if self.occupied && adjacent >= allowed_adj {
            // If a seat is occupied and four or more seats adjacent to it are also occupied, the seat becomes empty.
            self.next = Some(false);
            // println!("leaving seat... ({} adj)", adjacent);
        } else {
            // Otherwise, the seat's state does not change.
            self.next = None;
            // println!("no change. ({} adj)", adjacent);
        }
    }

    pub fn apply_next(&mut self) {
        if let Some(newval) = self.next.take() {
            self.occupied = newval;
            self.state_changed = true;
        } else {
            self.state_changed = false;
        }
    }

    pub fn has_changed(&self) -> bool {
        self.state_changed
    }
}

impl std::fmt::Display for Seat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let symbol = if self.occupied { '#' } else { 'L' };
        write!(f, "{}", symbol)
    }
}

fn pos_to_id(x: u32, y: u32) -> SeatId{
    let vec = vec![x.to_ne_bytes(), y.to_ne_bytes()];
    let mut bytes: [u8; 8] = [0; 8];
    for (n, arr) in vec.into_iter().enumerate() {
        for i in 0..4 {
            bytes[n*4+i] = arr[i];
        }
    }
    u64::from_ne_bytes(bytes)
}

fn id_to_pos(id: SeatId) -> (u32, u32) {
    use std::convert::TryInto;
    let bytes = id.to_ne_bytes();
    let x = u32::from_ne_bytes((&bytes[0..4]).try_into().unwrap());
    let y = u32::from_ne_bytes((&bytes[4..8]).try_into().unwrap());
    (x, y)
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_id_pos_conversion() {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        for _i in 0..100 {
            println!("testing 100 random conversions...");
            let x: u32 = rng.gen();
            let y: u32 = rng.gen();
            let id = crate::pos_to_id(x, y);
            let res = crate::id_to_pos(id);
            assert_eq!(res, (x, y), "Converted position has been mutated!");
        }
    }
}

struct SeatingGrid {
    seats: HashMap<SeatId, Seat>,
    dimension: (u32, u32),
}

impl SeatingGrid {
    pub fn from(input: Vec<Vec<bool>>) -> SeatingGrid {
        let mut seats = HashMap::new();
        let mut rows = 0;
        let mut cols = 0;
        for (y, row) in input.into_iter().enumerate() {
            rows = rows.max(y+1);
            for (x, col) in row.into_iter().enumerate() {
                cols = cols.max(x+1);
                // Only create seat if not floor
                if col {
                    let id = pos_to_id(x as u32, y as u32);
                    let seat = Seat::new();
                    seats.insert(id, seat);
                }
            }
        }
        SeatingGrid {
            seats,
            dimension: (rows as u32, cols as u32),
        }
    }

    pub fn get_neighbors(&self, id: SeatId) -> usize {
        let mut neighbors = 0;
        let (x,y) = id_to_pos(id);
        for dy in 0..=2 {
            for dx in 0..=2 {
                if (dy != 1 || dx != 1) && (dy > 0 || y > 0) && (dx > 0 || x > 0) {
                    let cx = x + dx - 1;
                    let cy = y + dy - 1;
                    if cx < self.dimension.1 && cy < self.dimension.0 {
                        let posid = pos_to_id(x+dx-1, y+dy-1);
                        if self.seats.get(&posid).map(|s| s.is_occupied()).unwrap_or(false) {
                            neighbors += 1;
                        }
                    }
                }
            }
        }
        // println!("Seat {:?} has {} neighbors. ({} checked)", (x,y), neighbors, checked);
        return neighbors;
    }

    pub fn get_first_8_directions(&self, id: SeatId) -> usize {
        let mut occupied = 0;
        let (x,y) = id_to_pos(id);
        let dirs = [(-1,-1), (-1,0), (-1,1), (0,1), (1,1), (1,0), (1,-1), (0,-1)];
        for dir in &dirs {
            // search for occupied until encountered seat or out of grid bounds
            let mut mult = 1;
            loop {
                let (dx,dy): (i64,i64) = (dir.0 * mult, dir.1 * mult);
                let x: i64 = x as i64 + dx;
                let y: i64 = y as i64 + dy;
                if x < 0 || x >= self.dimension.1 as i64 || y < 0 || y >= self.dimension.0 as i64 {
                    // out of grid bounds
                    break;
                } else {
                    // check seat on position
                    let posid = pos_to_id(x as u32, y as u32);
                    if let Some(seat) = self.seats.get(&posid) {
                        // found seat
                        if seat.is_occupied() {
                            occupied += 1;
                        }
                        break;
                    }
                }
                mult += 1;
            }
        }
        return occupied;
    }

    pub fn next_round(&mut self, part2: bool) {
        let neighbor_cache: HashMap<SeatId, usize> = self.seats.iter().map(|(id,_)| {
            let adj = if part2 {
                self.get_first_8_directions(*id) 
            } else {
                self.get_neighbors(*id)
            };
            (*id, adj)
        }).collect();
        for (id, seat) in self.seats.iter_mut() {
            let adjacent = neighbor_cache.get(id).unwrap();
            seat.determine_next(*adjacent, part2);
        }
        drop(neighbor_cache);
        // Apply changes after determination
        for seat in self.seats.values_mut() {
            seat.apply_next();
        }
    }

    pub fn count_occupied(&self) -> usize {
        let mut occupied = 0;
        for (_, seat) in self.seats.iter() {
            if seat.is_occupied() {
                occupied += 1;
            }
        }
        return occupied;
    }

    pub fn is_stable(&self) -> bool {
        let mut stable = true;
        for seat in self.seats.values() {
            stable = !seat.has_changed();
            if !stable { break; }
        }
        return stable;
    }
}

impl std::fmt::Display for SeatingGrid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::with_capacity((self.dimension.0 * self.dimension.1) as usize);
        for y in 0..self.dimension.0 {
            for x in 0..self.dimension.1 {
                let id = pos_to_id(x as u32, y as u32);
                let c = match self.seats.get(&id) {
                    Some(seat) => format!("{}", seat).pop().unwrap(),
                    None => '.',
                };
                s.push(c);
            }
            s.push('\n');
        }
        write!(f, "{}", s)
    }
}

fn main() {
    let part = std::env::args().skip(1).last().map(|s| usize::from_str_radix(s.as_str(), 10).expect("Only part numbers allowed as parameter!"));
    println!("##### Advent of Code - Day 11 #####");
    let mut lines = Vec::new();
    let mut buffer = String::new();
    while std::io::stdin().read_line(&mut buffer).expect("IO Error.") > 0 {
        lines.push(buffer.clone());
        buffer.clear();
    }

    let mut grid = Vec::with_capacity(lines.len());
    for line in lines.into_iter() {
        let mut col = Vec::with_capacity(line.len());
        for c in line.trim().chars() {
            match c {
                'L' => col.push(true),
                '.' => col.push(false),
                _ => {
                    println!("Illegal character in input data: {}", c);
                    return;
                },
            }
        }
        grid.push(col);
    }

    if part.is_none() || part.unwrap() == 1 {
        // Feed input data to grid
        let mut seating = SeatingGrid::from(grid.clone());
        while !seating.is_stable() {
            log!("next round (last {} occupied)", seating.count_occupied());
            seating.next_round(false);
            log!("{}", &seating);
        }
        let result = seating.count_occupied();
        println!("[1] => {}", result);
    }
    if part.is_none() || part.unwrap() == 2 {
         // Feed input data to grid
        let mut seating = SeatingGrid::from(grid);
        while !seating.is_stable() {
            log!("next round (last {} occupied)", seating.count_occupied());
            seating.next_round(true);
            log!("{}", &seating);
        }
        let result = seating.count_occupied();
        println!("[2] => {}", result);       
    }
}
