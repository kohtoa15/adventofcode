macro_rules! debug_log {
    ($($arg:tt)*) => {
        #[cfg(debug_assertions)]
        {
            println!($($arg)*);
        }
    };
}

use std::{error::Error, fmt::Display, collections::HashMap};

use tokio::io::BufReader;

#[tokio::main]
async fn main() {
    use tokio::io::AsyncBufReadExt;
    let reader = BufReader::new(tokio::io::stdin());
    let mut lines = reader.lines();

    // Count straight and diagonal intersecting lines separately
    // (straight, diagonal)
    let mut points: HashMap<(u32, u32), (usize, usize)> = HashMap::with_capacity(1000000);

    while let Some(line) = lines.next_line().await.expect("IO error") {
        let vent_line = Line::try_from(line.as_str()).expect("Could not parse input");
        debug_log!("new vent line {}", &vent_line);
        if vent_line.is_straight() {
            debug_log!("\tline is straight -> increase positions intersecting");
            for pt in vent_line.points() {
                if let Some(count) = points.get_mut(&pt) {
                    count.0 += 1;
                } else {
                    points.insert(pt, (1 as usize, 0 as usize));
                }
            }
        } else if vent_line.is_diagonal() {
            debug_log!("\tline is diagonal -> increase positions intersecting");
            for pt in vent_line.points() {
                if let Some(count) = points.get_mut(&pt) {
                    count.1 += 1;
                } else {
                    points.insert(pt, (0 as usize, 1 as usize));
                }
            }
        }
    }

    #[cfg(debug_assertions)]
    {
        let width = points.iter()
            .map(|((x,_),_)| *x)
            .max()
            .unwrap_or_default();
        let height = points.iter()
            .map(|((_,y),_)| *y)
            .max()
            .unwrap_or_default();
        for y in 0..=height {
            for x in 0..=width {
                if let Some((s, d)) = points.get(&(x, y)) {
                    print!("{}", *s + *d);
                } else {
                    print!(".");
                }
            }
            println!("");
        }
    }

    println!(
        "1) Consider only horizontal and vertical lines. At how many points do at least two lines overlap?\n  --> Answer: {}",
        points.iter().filter(|(_,(straight, _))| *straight >= 2).count()
    );
    println!(
        "2) Consider all of the lines. At how many points do at least two lines overlap?\n  --> Answer: {}",
        points.iter()
            .map(|(_, (straight, diagonal))| *straight + *diagonal)
            .filter(|n| *n >= 2)
            .count()
    );
}

trait Intersecting {
    fn intersects_point(&self, x: u32, y: u32) -> bool {
        for (x1, y1) in self.points() {
            if x1 == x && y1 == y {
                return true;
            }
        }
        false
    }

    fn intersects(&self, other: &dyn Intersecting) -> bool {
        // checking on every own point if `other` intersects it
        for (x1, y1) in self.points() {
            if other.intersects_point(x1, y1) {
                return true;
            }
        }
        false
    }

    fn points(&self) -> Vec<(u32, u32)>;
}

#[derive(Clone, Copy)]
struct Point {
    pub x: u32,
    pub y: u32,
}

impl TryFrom<&str> for Point {
    type Error = Box<dyn Error>;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let coords: Vec<&str> = value.trim()
            .split(",")
            .map(|s| s.trim())
            .collect();
        if coords.len() != 2 {
            return Err(Box::new(std::io::Error::new(std::io::ErrorKind::Other, "Wrong number of coordinates")));
        }
        Ok(Point {
            x: u32::from_str_radix(coords[0], 10)?,
            y: u32::from_str_radix(coords[1], 10)?,
        })
    }
}

impl From<(u32, u32)> for Point {
    fn from(coords: (u32, u32)) -> Self {
        let (x, y) = coords;
        Point { x, y }
    }
}

impl Intersecting for Point {
    fn points(&self) -> Vec<(u32, u32)> {
        vec![(self.x, self.y)]
    }
}

impl Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{},{}", self.x, self.y)
    }
}

#[derive(Clone)]
struct Line {
    from: Point,
    to: Point,
}

impl TryFrom<&str> for Line {
    type Error = Box<dyn Error>;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let points_str: Vec<&str> = value.trim()
            .split(" -> ")
            .collect();
        if points_str.len() != 2 {
            return Err(Box::new(std::io::Error::new(std::io::ErrorKind::Other, "Wrong number of Points")));
        }
        let from = Point::try_from(points_str[0])?;
        let to = Point::try_from(points_str[1])?;
        Ok(Line {
            from, to
        })
    }
}

impl Intersecting for Line {
    fn points(&self) -> Vec<(u32, u32)> {
        let mut points = Vec::new();

        let start_x = self.from.x.min(self.to.x);
        let end_x = self.from.x.max(self.to.x);
        let start_y = self.from.y.min(self.to.y);
        let end_y = self.from.y.max(self.to.y);


        if self.is_straight() {
            for x in start_x..=end_x {
                for y in start_y..=end_y {
                    // Construct individual points
                    points.push((x, y));
                }
            }
        } else if self.is_diagonal() {
            let dx = self.to.x as i64 - self.from.x as i64;
            let ix_neg = dx < 0;
            let dy = self.to.y as i64 - self.from.y as i64;
            let iy_neg = dy < 0;

            for (ix, iy) in (0..=dx.abs()).zip(0..=dy.abs()) {
                let mut x = self.from.x as i64;
                if ix_neg {
                    x -= ix;
                } else {
                    x += ix;
                }
                let mut y = self.from.y as i64;
                if iy_neg {
                    y -= iy;
                } else {
                    y += iy;
                }
                points.push((x as u32, y as u32));
            }
        }
        debug_log!("points of line {}: \n\t{:?}", &self, &points);
        points
    }
}

impl Display for Line {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} -> {}", &self.from, &self.to)
    }
}

impl Line {
    pub fn is_straight(&self) -> bool {
        self.from.x == self.to.x || self.from.y == self.to.y
    }

    pub fn is_diagonal(&self) -> bool {
        let dx = self.from.x.max(self.to.x) - self.from.x.min(self.to.x);
        let dy = self.from.y.max(self.to.y) - self.from.y.min(self.to.y);
        dx == dy
    }

    pub fn get_endpoints(&self) -> Vec<&Point> {
        vec![&self.from, &self.to]
    }
}

