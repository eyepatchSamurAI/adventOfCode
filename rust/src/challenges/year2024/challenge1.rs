use std::collections::HashMap;

use crate::file::read_challenge;

fn to_lists(contents: String) -> (Vec<usize>, Vec<usize>) {
    let split_content = contents.split('\n');
    let lists = split_content.fold( (vec![], vec![]), |mut acc, x| {
        let split_str: (&str, &str) = x.split_once("   ").unwrap_or(("0", "0"));
        let first_number = split_str.0.parse().unwrap_or(0);
        let second_number = split_str.1.parse().unwrap_or(0);
        acc.0.push(first_number);
        acc.1.push(second_number);
        return acc;
    });
    return lists;
}

fn part1_solution() -> usize {
    let contents = read_challenge("../data/year2024/challenge1/puzzle.txt");
    let (mut left, mut right) = to_lists(contents);
    left.sort();
    right.sort();

    let total_difference: usize = left
    .iter()
    .zip(right.iter())
    .map(|(l, r)| l.abs_diff(*r))
    .sum();

    total_difference
}

fn part2_solution() -> usize {
    let contents = read_challenge("../data/year2024/challenge1/puzzle.txt");
    let (left, right) = to_lists(contents);
    let frequency_map: HashMap<usize, usize> = right.iter().fold(HashMap::new(), |mut acc, &x| {
        *acc.entry(x).or_insert(0) += 1;
        acc
    });
    let result: usize = left.iter().map(|l| {
        let multipler = frequency_map.get(l).unwrap_or(&0);
        l * multipler
    }).sum();

    result

}

pub fn run() {
    let part1_solution = part1_solution();
    println!("Part1: {:#?}", part1_solution);

    let part2_solution = part2_solution();
    println!("Part2: {:#?}", part2_solution);

}
