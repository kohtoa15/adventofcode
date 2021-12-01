use std::collections::HashSet;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Mutex;
use regex::Regex;

fn string_from_match(capture: &regex::Captures, i: usize) -> Option<String> {
    capture.get(i).map(|v| v.as_str().trim().to_string())
}

fn unwrap_capture(capture: regex::Captures) -> Result<(String, Vec<(u8, String)>), String>{
    let bag = string_from_match(&capture, 1).ok_or(String::from("could not find bag definition!"))?;
    let contains = string_from_match(&capture, 2).ok_or(String::from("could not find contain definition!"))?;
    // split up contains
    let mut contain: Vec<(u8, String)> = Vec::new();
    for part in contains.split(", ") {
        let mid = part.find(' ').ok_or(format!("no number in contain part '{}'!", part))?;
        let (lhs, rhs) = part.split_at(mid);
        let num;
        if lhs == "no" {
            num = 0;
        } else {
            num = u8::from_str_radix(&lhs, 10).map_err(|e| e.to_string())?;
        }
        contain.push((num, rhs.replace("bags", "").replace("bag", "").trim().to_string()));
    }
    Ok((bag, contain))
}

fn get_parts(pattern: &Regex, input: &str) -> Result<(String, Vec<(u8, String)>), String> {
    unwrap_capture(pattern.captures(input).ok_or(format!("Illegal input format: {}", input))?)
}

struct BagNode {
    color: String,
    parents: Vec<Rc<Mutex<BagNode>>>,
    children: Vec<(Rc<Mutex<BagNode>>, u8)>,
}

impl BagNode {
    pub fn from(color: String) -> BagNode {
        BagNode {
            color,
            parents: Vec::new(),
            children: Vec::new(),
        }
    }

    pub fn add_child(&mut self, node: Rc<Mutex<BagNode>>, num: u8) {
        self.children.push((node, num));
    }

    pub fn add_parent(&mut self, node: Rc<Mutex<BagNode>>) {
        self.parents.push(node);
    }

    pub fn get_containers(&self, ret: &mut HashSet<String>) {
        if !ret.contains(&self.color) {
            ret.insert(self.color.clone());
            for parent in self.parents.iter() {
                parent.lock().unwrap().get_containers(ret);
            }
        }
    }

    pub fn count_children(&self) -> u32 {
        let mut count = 0;
        for (child, n) in self.children.iter() {
            let sum = child.lock().unwrap().count_children();
            // println!("counting {} times {}", sum + 1, n);
            count += (sum + 1) * (*n as u32);
        }
        return count;
    }
}

struct NodeCollection {
    nodes: HashMap<String, Rc<Mutex<BagNode>>>,
}

impl NodeCollection {
    pub fn new() -> NodeCollection { NodeCollection { nodes: HashMap::new() } }

    pub fn get_node(&mut self, name: String) -> Rc<Mutex<BagNode>> {
        if let Some(res) = self.nodes.get(&name) {
            Rc::clone(res)
        } else {
            // Create new BagNode and insert into Map
            let bn = Rc::new(Mutex::new(BagNode::from(name.clone())));
            self.nodes.insert(name, Rc::clone(&bn));
            bn
        }
    }

    pub fn enter(&mut self, key: String, contains: Vec<(u8, String)>) {
        let node = self.get_node(key.clone());
        // repeat enter process for children
        for (n, child) in contains.into_iter() {
            if n > 0 {
                // add node as parent
                let childnode = self.get_node(child);
                childnode.lock().unwrap().add_parent(Rc::clone(&node));
                // add as child
                node.lock().unwrap().add_child(Rc::clone(&childnode), n);
            }
        }
    }

    pub fn get_containers(&self, key: &String) -> Option<HashSet<String>> {
        self.nodes.get(key).map(|n| {
            let mut ret = HashSet::new();
            n.lock().unwrap().get_containers(&mut ret);
            ret
        })
    }

    pub fn count_children(&self, key: &String) -> Option<u32> {
        self.nodes.get(key).map(|n| {
            n.lock().unwrap().count_children()
        })
    }
}

fn main() {
    let option = std::env::args().skip(1).last();
    println!("##### Advent of Code - Day 7 #####");
    let pattern = Regex::new(r"^([a-z\s]*) bags contain(( (\d|no)[a-z\s]*bags?,?)+)\.").unwrap();
    let mut lines = Vec::new();
    let mut buffer = String::new();
    loop {
        buffer.clear();
        match std::io::stdin().read_line(&mut buffer) {
            Ok(n) => {
                if n == 0 { break; }
                lines.push(buffer.clone());
                buffer.clear();
            },
            Err(_) => println!("Error occurred trying to read stdin!"),
        }
    }

    let mut nodes = NodeCollection::new();
    // Match pattern against input
    for line in lines.into_iter() {
        let (bag, subset) = get_parts(&pattern, &line).unwrap();
        // Convert into node
        nodes.enter(bag, subset);
    }

    let gold = String::from("shiny gold");
    if option.is_none() || option.as_ref().unwrap() == "1" {
        let mut containers = nodes.get_containers(&gold).unwrap();
        containers.remove(&gold);
        let len = containers.len();
        println!("[1] => [{}]", len);
    }
    if option.is_none() || option.as_ref().unwrap() == "2" {
        let bags_inside = nodes.count_children(&gold).unwrap();
        println!("[2] => [{}]", bags_inside);
    }
}
