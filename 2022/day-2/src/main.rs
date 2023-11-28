use std::io::{Error, ErrorKind};

enum Outcome { Win, Draw, Loss }

impl Outcome {
    pub fn from_str(s: &str) -> Option<Outcome> {
        match s {
            "X" => Some(Outcome::Loss),
            "Y" => Some(Outcome::Draw),
            "Z" => Some(Outcome::Win),
            _ => None,
        }
    }

    pub fn score(&self) -> u32 {
        match self {
            Outcome::Win => 6,
            Outcome::Draw => 3,
            Outcome::Loss => 0,
        }
    }
}

enum Shape { Rock, Paper, Scissors }

impl Shape {
    pub fn from_str(c: &str) -> Option<Shape> {
        match c {
            "A" => Some(Shape::Rock),
            "X" => Some(Shape::Rock),
            "B" => Some(Shape::Paper),
            "Y" => Some(Shape::Paper),
            "C" => Some(Shape::Scissors),
            "Z" => Some(Shape::Scissors),
            _ => None,
        }
    }

    pub fn outcome_against(&self, other: &Shape) -> Outcome {
        match (self, other) {
            // Draws
            (Shape::Rock, Shape::Rock) => Outcome::Draw,
            (Shape::Paper, Shape::Paper) => Outcome::Draw,
            (Shape::Scissors, Shape::Scissors) => Outcome::Draw,
            // Losses
            (Shape::Rock, Shape::Paper) => Outcome::Loss,
            (Shape::Paper, Shape::Scissors) => Outcome::Loss,
            (Shape::Scissors, Shape::Rock) => Outcome::Loss,
            // Wins
            (Shape::Rock, Shape::Scissors) => Outcome::Win,
            (Shape::Paper, Shape::Rock) => Outcome::Win,
            (Shape::Scissors, Shape::Paper) => Outcome::Win,
        }
    }

    pub fn score(&self) -> u32 {
        match self {
            Shape::Rock => 1,
            Shape::Paper => 2,
            Shape::Scissors => 3,
        }
    }

    pub fn play_for_outcome(&self, outcome: &Outcome) -> Shape {
        match (outcome, self) {
            (Outcome::Win, Shape::Rock) => Shape::Paper,
            (Outcome::Win, Shape::Paper) => Shape::Scissors,
            (Outcome::Win, Shape::Scissors) => Shape::Rock,
            (Outcome::Draw, Shape::Rock) => Shape::Rock,
            (Outcome::Draw, Shape::Paper) => Shape::Paper,
            (Outcome::Draw, Shape::Scissors) => Shape::Scissors,
            (Outcome::Loss, Shape::Rock) => Shape::Scissors,
            (Outcome::Loss, Shape::Paper) => Shape::Rock,
            (Outcome::Loss, Shape::Scissors) => Shape::Paper,
        }
    }
}

fn calculate_game_score(my_shape: &Shape, other: &Shape) -> u32 {
    let shape_score = my_shape.score();
    let outcome_score = my_shape.outcome_against(other).score();
    return shape_score + outcome_score;
}

fn calculate_score_from_outcome(opponent: &Shape, outcome: &Outcome) -> u32 {
    let my_shape = opponent.play_for_outcome(outcome);
    return my_shape.score() + outcome.score();
}

fn main() -> std::io::Result<()> {
    let file = std::env::args().skip(1).next().unwrap_or(String::from("input.txt"));
    let content = std::fs::read_to_string(file)?;

    let mut score_v1 = 0;
    let mut score_v2 = 0;
    for line in content.lines() {
        let words: Vec<&str> = line.split_whitespace()
            .collect();
        if words.len() == 2 {
            let opponent = Shape::from_str(words[0]).expect("invalid shape for opponent");
            let you = Shape::from_str(words[1]).expect("invalid shape for you");
            let outcome = Outcome::from_str(words[1]).expect("invalid outcome of game");
            score_v1 += calculate_game_score(&you, &opponent);
            score_v2 += calculate_score_from_outcome(&opponent, &outcome);
        } else {
            return Err(Error::new(ErrorKind::InvalidData, "invalid input file data"));
        }
    }

    println!("Total score V1: {}", score_v1);
    println!("Total score V2: {}", score_v2);

    Ok(())
}
