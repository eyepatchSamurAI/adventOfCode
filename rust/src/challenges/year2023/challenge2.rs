use crate::file::read_challenge;
use nom::AsChar;

fn part1_solution() -> usize {
    let max_red = 12;
    let max_green = 13;
    let max_blue = 14;
    
    let contents = read_challenge("../data/year2023/challenge2/puzzle.txt");
    let lines = contents.lines();
    let mut sum = 0;
    lines.into_iter().enumerate().for_each(|(index, line)| {
        let log_space = if index == 0 {
            0.0
        } else {
            ((index + 1) as f32).log10().floor()
        };
        let extra_space = (log_space + 8.0) as usize;
        let mut num_acc = String::new();
        let mut iterator = line.chars().skip(extra_space).peekable();
        let mut found = false;
        while let Some(char) = iterator.next() {
            if char.is_dec_digit() {
                num_acc.push(char);
            } else {
                match iterator.peek() {
                    Some(&'b') => {
                        let parsed_number: i32 = num_acc.parse().expect("Not a number!");
                        if max_blue - parsed_number < 0 {
                            found = true;
                            break;
                        }
                        for _ in 0..4 {
                            iterator.next();
                        }
                        
                        num_acc = String::new();
                        
                    }
                    Some(&'g') => {
                        let parsed_number: i32 = num_acc.parse().expect("Not a number!");
                        if max_green - parsed_number < 0 {
                            found = true;
                            break;
                        }
                        for _ in 0..5 {
                            iterator.next();
                        }
                        num_acc = String::new();
                    }
                    Some(&'r') => {
                        let parsed_number: i32 = num_acc.parse().expect("Not a number!");

                        if max_red - parsed_number < 0 {
                            found = true;
                            break;
                        }
                        for _ in 0..3 {
                            iterator.next();
                        }
                        num_acc = String::new();
                    }
                    _ => {}
                }
            }
            
        }
        if !found {
            found = !found;
            sum += index + 1;
        }

    });
    sum
}

fn part1_solution_nom() {

}

#[derive(Debug)]
struct Color {
    id: char,
    highest: usize,
    skip: usize,
}

impl Color {
    pub fn new(id: char, skip: usize) -> Self {
        Color { id, highest: 0, skip}
    }
}

fn part2_solution() -> usize {
    let contents = read_challenge("../data/year2023/challenge2/puzzle.txt");
    let lines = contents.lines();
    let sum= lines.enumerate().map(|(index, line)| {
        let log_space = if index == 0 {
            0.0
        } else {
            ((index + 1) as f32).log10().floor()
        };
        let extra_space = (log_space + 8.0) as usize;
        let mut num_acc = String::new();
        let mut iterator = line.chars().skip(extra_space).peekable();
        let mut highests_colors = [Color::new('b', 4), Color::new('r', 3), Color::new('g', 5)];
        while let Some(char) = iterator.next() {
            if char.is_dec_digit() {
                num_acc.push(char);
                continue;
            }
            if char == ' ' {
                continue;
            }
            if let Some(color) = highests_colors.iter_mut().find(|c| c.id == char) {
                if let Ok(parsed_number) = num_acc.parse::<usize>() {
                    if parsed_number > color.highest {
                        color.highest = parsed_number;
                    }
                    for _ in 0..color.skip {
                        iterator.next();
                    }
                    num_acc.clear();
                }
                
            }
        }
        println!("{:#?}", highests_colors);
        highests_colors.into_iter().fold(1, |acc, a| acc * a.highest)
    }).sum();
    sum
}

pub fn run() {
    let part1_solution = part1_solution();
    println!("Part1: {:#?}", part1_solution);

    let part2_solution = part2_solution();
    println!("Part2: {:#?}", part2_solution);
}
