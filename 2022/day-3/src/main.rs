use std::io::{Error, ErrorKind};

fn calculate_priority(c: char) -> u8 {
    let lowercase = 'a' as u8;
    let uppercase = 'A' as u8;
    assert!(c.is_ascii_alphabetic());
    let prio;
    if c.is_lowercase() {
        prio = c as u8 - lowercase + 1;
    } else if c.is_uppercase() {
        prio = c as u8 - uppercase + 27;
    } else {
        unreachable!("char is neither lower nor uppercase");
    }
    return prio;
}

#[test]
fn test_priority_calculation() {
    assert_eq!(calculate_priority('p'), 16);
    assert_eq!(calculate_priority('L'), 38);
    assert_eq!(calculate_priority('P'), 42);
    assert_eq!(calculate_priority('v'), 22);
    assert_eq!(calculate_priority('t'), 20);
    assert_eq!(calculate_priority('s'), 19);
}

fn get_common_item(lists: Vec<&[u8]>) -> std::io::Result<u8> {
    assert!(!lists.is_empty());
    let mut common = None;
    for i in *lists.first().unwrap() {
        // check if item exists in all lists
        if lists.iter().skip(1).map(|list| list.binary_search(i)).all(|x| x.is_ok()) {
            // element exists in all lists
            let old = common.replace(*i);
            if let Some(old_i) = old {
                if old_i != *i {
                    return Err(Error::new(ErrorKind::InvalidData, format!("more than one common item in rucksack ({}, {})", old_i, i)));
                }
            }
        }
    }
    return common.ok_or(Error::new(ErrorKind::InvalidData, "no common item in rucksack"));
}

struct Rucksack (Vec<u8>, Vec<u8>);

impl Rucksack {
    pub fn from_str(s: &str) -> std::io::Result<Self> {
        if s.len() % 2 == 0 {
            let mid = s.len() / 2;
            let mut v1: Vec<u8> = s[0..mid].chars().map(calculate_priority).collect();
            v1.sort();
            let mut v2: Vec<u8> = s[mid..].chars().map(calculate_priority).collect();
            v2.sort();
            return Ok(Self(v1, v2));
        } else {
            return Err(Error::new(ErrorKind::InvalidData, "invalid rucksack length"));
        }
    }

    pub fn get_common_item(&self) -> std::io::Result<u8> {
        return get_common_item(vec![self.0.as_slice(), self.1.as_slice()]);
        // let mut common = None;
        // for i in self.0.iter() {
        //     if self.1.binary_search(i).is_ok() {
        //         // element exists in both halves
        //         let old = common.replace(*i);
        //         if let Some(old_i) = old {
        //             if old_i != *i {
        //                 return Err(Error::new(ErrorKind::InvalidData, format!("more than one common item in rucksack ({}, {})", old_i, i)));
        //             }
        //         }
        //     }
        // }
        // return common.ok_or(Error::new(ErrorKind::InvalidData, "no common item in rucksack"));
    }

    pub fn get_all_unique_items(&self) -> Vec<u8> {
        let mut v = Vec::new();
        v.extend_from_slice(self.0.as_slice());
        v.extend_from_slice(self.1.as_slice());
        v.sort();
        return v;
    }
}

fn main() -> std::io::Result<()> {
    let file = std::env::args().skip(1).next().unwrap_or(String::from("input.txt"));
    let content = std::fs::read_to_string(file)?;


    let mut priority_sum: u64 = 0;
    let mut rucksacks = Vec::new();
    for line in content.lines() {
        let ruck = Rucksack::from_str(line)?;
        let common = ruck.get_common_item()?;
        rucksacks.push(ruck);
        priority_sum += common as u64;
    }
    println!("Total sum of priorities: {}", priority_sum);

    let mut badge_sum: u64 = 0;
    for group in rucksacks.chunks(3) {
        let group: Vec<Vec<u8>> = group.iter().map(|r| r.get_all_unique_items()).collect();
        let lists = vec![group[0].as_slice(), group[1].as_slice(), group[2].as_slice()];
        let badge = get_common_item(lists)?;
        badge_sum += badge as u64;
    }
    println!("Total sum of badges: {}", badge_sum);

    Ok(())
}
