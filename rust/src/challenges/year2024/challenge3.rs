use regex::Regex;

use crate::file::read_challenge;

fn part1_solution() -> i32 {
    let contents = read_challenge("../data/year2024/challenge3/puzzle.txt");

    let mut results = vec![];
    let re = Regex::new(r"mul\((\d{1,3}),(\d{1,3})\)").unwrap();

    for line in contents.lines() {
        for cap in re.captures_iter(line) {
            let num1: i32 = cap[1].parse().unwrap();
            let num2: i32 = cap[2].parse().unwrap();
            results.push(num1 * num2);
        }
    }
    results.iter().sum()
}

fn part2_solution() -> i32 {
    let contents = read_challenge("../data/year2024/challenge3/puzzle.txt");

    let mut results = vec![];
    let re = Regex::new(r"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)").unwrap();
    let mut is_active = true;

    for line in contents.lines() {
        for cap in re.captures_iter(line) {
            println!("{:?}", cap);
            let capture_name = &cap[0];
            if capture_name == "don't()" {
                is_active = false;
                continue;
            } else if capture_name == "do()" {
                is_active = true;
                continue;
            }
            if is_active {
                let num1: i32 = cap[1].parse().unwrap();
                let num2: i32 = cap[2].parse().unwrap();
                results.push(num1 * num2);
            }
        }
    }

    results.iter().sum()
}

pub fn run() {
    let part1_solution = part1_solution();
    println!("Part1: {:#?}", part1_solution);

    let part2_solution = part2_solution();
    println!("Part2: {:#?}", part2_solution);

}
