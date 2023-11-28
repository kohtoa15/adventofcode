use std::io::Error;
use std::io::ErrorKind;

fn main() -> std::io::Result<()> {
    let file = std::env::args().skip(1).next().unwrap_or(String::from("input.txt"));
    let content = std::fs::read_to_string(file)?;

    let mut elves: Vec<u32> = Vec::new();

    for line in content.lines() {
        if line.is_empty() {
            // separator, move on to next elf
            elves.push(0);
        } else {
            let calories = line.parse::<u32>().map_err(|e| Error::new(ErrorKind::InvalidData, e))?;
            if let Some(elf) = elves.last_mut() {
                *elf += calories;
            } else {
                // No entry yet
                elves.push(calories);
            }
        }
    }

    elves.sort_unstable();
    let max: u32 = elves.last().map(|x| *x).unwrap_or(0);
    println!("Most calories carried by a single elf: {}", max);
    let top_three: u32 = elves.iter().rev().take(3).sum();
    println!("Calories carried by the top three most-carrying elves: {}", top_three);

    Ok(())
}
