
macro_rules! log {
    ($($arg:tt)*) => {
        #[cfg(debug_assertions)]
        {
            println!($($arg)*);
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Direction {
    North, East, South, West
}

impl Direction { 
    pub fn turn_by_degrees(&mut self, deg: i32) {
        assert!(deg % 90 == 0, "Illegal degree input!");
        let mut d = (deg / 90) % 4;
        let rest;
        if d > 1 {
            rest = d - 1;
            d = 1;
        } else if d < -1 {
            rest = d + 1;
            d = -1;
        } else {
            rest = 0;
        }
        match (&self, d) {
            (_, 0) => {},
            (Direction::North, 1) => *self = Direction::West,
            (Direction::North, -1) => *self = Direction::East,

            (Direction::West, 1) => *self = Direction::South,
            (Direction::West, -1) => *self = Direction::North,
            
            (Direction::South, 1) => *self = Direction::East,
            (Direction::South, -1) => *self = Direction::West,

            (Direction::East, 1) => *self = Direction::North,
            (Direction::East, -1) => *self = Direction::South,

            (_, _) => panic!("Impossible match case!"),
        };
        if rest != 0 {
            self.turn_by_degrees(rest*90);
        }
    }

    pub fn delta(&self) -> (i32, i32) {
        match self {
            Direction::East => (1, 0),
            Direction::North => (0, 1),
            Direction::West => (-1, 0),
            Direction::South => (0, -1),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::Direction; 

    #[test]
    fn test_turn_left() {
        let start = Direction::North;
        let mut dir = start.clone();
        for _i in 0..20 {
            dir.turn_by_degrees(-90);
        }
        assert_eq!(start, dir);
    }

    #[test]
    fn test_turn_right() {
        let start = Direction::South;
        let mut dir = start.clone();
        for _i in 0..20 {
            dir.turn_by_degrees(90);
        }
        assert_eq!(start, dir);
    }

    #[test]
    fn test_turn_left_mixed_intervals() {
        let start = Direction::East;
        let mut dir = start.clone();
        for _i in 0..8 {
            dir.turn_by_degrees(-270);
            dir.turn_by_degrees(-180);
            dir.turn_by_degrees(-90);
        }
        assert_eq!(start, dir);
    }
    
    #[test]
    fn test_turn_right_mixed_intervals() {
        let start = Direction::East;
        let mut dir = start.clone();
        for _i in 0..8 {
            dir.turn_by_degrees(270);
            dir.turn_by_degrees(180);
            dir.turn_by_degrees(90);
        }
        assert_eq!(start, dir);
    }

    #[test]
    fn test_turn_around() {
        let start = Direction::West;
        let mut dir = start.clone();
        dir.turn_by_degrees(360);
        assert_eq!(start, dir);
        dir.turn_by_degrees(-360);
        assert_eq!(start, dir);
    }

    #[test]
    fn test_turn_back() {
        let start = Direction::East;
        let mut dir = start.clone();
        dir.turn_by_degrees(90);
        dir.turn_by_degrees(-90);
        assert_eq!(start, dir);
        dir.turn_by_degrees(180);
        dir.turn_by_degrees(-180);
        assert_eq!(start, dir);
        dir.turn_by_degrees(270);
        dir.turn_by_degrees(-270);
        assert_eq!(start, dir);
    }
}

struct Ship {
    x: i32,
    y: i32,
    facing: Direction,
}

impl Ship {
    pub fn new() -> Ship {
        Ship {
            x: 0,
            y: 0,
            facing: Direction::East,
        }
    }

    pub fn advance(&mut self, units: i32, direction: Direction) {
        let (dx, dy) = direction.delta();
        self.x += dx * units;
        self.y += dy * units;
        log!("# ship moved to east {}, north {}", self.x, self.y);
    }

    pub fn advance_forward(&mut self, units: i32) {
        self.advance(units, self.facing);
    }

    pub fn turn_left(&mut self, degrees: i32) {
        self.facing.turn_by_degrees(degrees);
        log!("# ship turned; faces now {:?}", &self.facing);
    }

    pub fn turn_right(&mut self, degrees: i32) {
        self.facing.turn_by_degrees(-degrees);
        log!("# ship turned; faces now {:?}", &self.facing);
    }

    pub fn inner(self) -> (i32, i32) {
        (self.x, self.y)
    }
}

fn parse_action_part1(ship: &mut Ship, input: &str) -> Result<(), String> {
    let val = i32::from_str_radix(&input[1..], 10).map_err(|e| e.to_string())?;
    match input.chars().next().unwrap() {
        'N' => ship.advance(val, Direction::North),
        'S' => ship.advance(val, Direction::South),
        'E' => ship.advance(val, Direction::East),
        'W' => ship.advance(val, Direction::West),
        'L' => ship.turn_left(val),
        'R' => ship.turn_right(val),
        'F' => ship.advance_forward(val),
        _ => return Err(format!("Illegal input string '{}'", input)),
    };
    Ok(())
}

struct Waypoint(i32, i32);

impl Waypoint {
    pub fn advance(&mut self, val: i32, dir: Direction) {
        let (dx, dy) = dir.delta();
        self.0 += val * dx;
        self.1 += val * dy;
        log!("# waypoint moved to east {}, north {}", self.0, self.1);
    }

    fn tuple(&self) -> (i32,i32) { (self.0, self.1) }

    fn turn(&mut self, deg: i32) {
        let mut dir = Direction::East;
        dir.turn_by_degrees(deg);
        match dir {
            Direction::East => {}, // Standard direction
            Direction::South => {
                // Turn once clockwise
                let (x,y) = self.tuple();
                self.0 = y;
                self.1 = -x;
            },
            Direction::West => {
                // Flip coordinates
                let (x,y) = self.tuple();
                self.0 = -x;
                self.1 = -y;
            },
            Direction::North => {
                // Turn once counter-clockwise
                let (x,y) = self.tuple();
                self.0 = -y;
                self.1 = x;
            },
        }
        log!("# waypoint turned by {} degrees; now at east {}, north {}", deg, self.0, self.1);
    }

    pub fn turn_left(&mut self, deg: i32) { self.turn(deg) }

    pub fn turn_right(&mut self, deg: i32) { self.turn(-deg) }
}

type WaypointShip = (i32, i32);

fn parse_action_part2(ship: &mut WaypointShip, waypoint: &mut Waypoint, input: &str) -> Result<(), String> {
    let val = i32::from_str_radix(&input[1..], 10).map_err(|e| e.to_string())?;
    match input.chars().next().unwrap() {
        'N' => waypoint.advance(val, Direction::North),
        'S' => waypoint.advance(val, Direction::South),
        'E' => waypoint.advance(val, Direction::East),
        'W' => waypoint.advance(val, Direction::West),
        'L' => waypoint.turn_left(val),
        'R' => waypoint.turn_right(val),
        'F' => {
            ship.0 += waypoint.0 * val;
            ship.1 += waypoint.1 * val;
            log!("# ship moved to east {}, north {}", ship.0, ship.1);
        },
        _ => return Err(format!("Illegal input string '{}'", input)),
    };
    Ok(())
}

fn manhattan_distance(x: i32, y: i32) -> i32 {
    x.abs() + y.abs()
}

fn main() {
    let option = std::env::args().skip(1).last();
    println!("##### Advent of Code - Day 12 #####");

    let mut lines = Vec::new();
    let mut buffer = String::new();
    while std::io::stdin().read_line(&mut buffer).expect("IO Error.") > 0 {
        lines.push(buffer.clone());
        buffer.clear();
    }

    if option.is_none() || option.clone().unwrap() == "1" {
        let mut ship = Ship::new();
        for (i, line) in lines.iter().enumerate() {
            let input = line.trim();
            #[cfg(debug_assertions)]
            print!("{}\t>>\t", input);
            match parse_action_part1(&mut ship, input) {
                Ok(_) => {},
                Err(e) => {
                    println!("Error occurred on line {}: {}", i, e);
                    return;
                },
            }
        }
        let pos = ship.inner();
        log!("  resulting position of ship: {:?}", &pos);
        let res = manhattan_distance(pos.0, pos.1);
        println!("[1] => {}", res);
    }
    if option.is_none() || option.unwrap() == "2" {
        let mut ship = (0, 0);
        let mut waypoint = Waypoint(10, 1);
        for (i, line) in lines.iter().enumerate() {
            let input = line.trim();
            #[cfg(debug_assertions)]
            print!("{}\t>>\t", input);
            match parse_action_part2(&mut ship, &mut waypoint, input) {
                Ok(_) => {},
                Err(e) => {
                    println!("Error occurred on line {}: {}", i, e);
                    return;
                },
            }
        }
        log!("  resulting position of ship: {:?}", &ship);
        let res = manhattan_distance(ship.0, ship.1);
        println!("[2] => {}", res);
    }
}
