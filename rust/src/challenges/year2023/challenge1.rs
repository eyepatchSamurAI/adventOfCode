use nom::AsChar;

use crate::file::read_challenge;

const NAMES: [&str; 9] = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];

fn part1_solution() -> u32 {
    let data = read_challenge("../data/year2023/challenge1/puzzle.txt");
    let contents: u32 = data
        .lines()
        .map(|line| {
            let first = line
                .chars()
                .find(|char| char.is_digit(10))
                .unwrap()
                .to_digit(10)
                .unwrap()
                * 10;
            let last = line
                .chars()
                .rev()
                .find(|char| char.is_digit(10))
                .unwrap()
                .to_digit(10)
                .unwrap();
            first + last
        })
        .sum();
    contents
}

fn digit_parser(input: &str, is_rev: bool) -> u32 {
    let mut chars = input.chars();
    let mut acc = String::new();

    while let Some(char) = chars.next() {
        if char.is_dec_digit() {
            return char.to_digit(10).unwrap();
        }
        acc.push(char);
        for (index, digit) in NAMES.iter().enumerate() {
            let mod_digit = if is_rev { digit.chars().rev().collect::<String>()} else {digit.to_string()};
            if acc.contains(mod_digit.as_str()) {
                return (index + 1) as u32;
            }
        }
    }
    0
}

fn part2_solution() -> u32 {
    let data = read_challenge("../data/year2023/challenge1/puzzle.txt");
    data.lines().map(|line: &str| {
        let first_digit = digit_parser(line, false);
        let last_digit = digit_parser(line.chars().rev().collect::<String>().as_str(), true);
        (first_digit * 10) + last_digit
    }).sum()
}

pub fn run() {
    let part1_solution = part1_solution();
    println!("Part1: {:#?}", part1_solution);

    let part2_solution = part2_solution();
    println!("Part2: {:#?}", part2_solution);

}
