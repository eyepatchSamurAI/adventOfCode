use crate::file::read_challenge;
use nom::AsChar;

struct Found {
    height: usize,
    width: usize,
    length: usize,
    number: usize,
}

fn part1_solution() {
    let contents = read_challenge("../data/year2023/challenge3/sample.txt");
    let engine_schema = contents.lines().into_iter();
    let mut num_acc = String::new();
    let mut found_numbers = Vec::<Found>::new();
    let mut found_symbols = Vec::<Found>::new();

    // for (height_index, line) in engine_schema.enumerate() {
    //     let engine_line = line.chars();
    //     for (width_index, char) in line.chars().into_iter().enumerate() {
    //         if char.is_dec_digit() {
    //             num_acc.push(char);
    //             continue;
    //         } else if char != '.' {
    //             let found = Found {
    //                 height: height_index,
    //                 width: width_index,
    //                 length: 1,
    //                 number: 0,
    //             };
    //             found_symbols.push(found);
    //             continue;
    //         } else if char == '.' && !num_acc.is_empty() {
    //             let number = num_acc.parse::<usize>().expect("Not a number");
    //             let found = Found {
    //                 height: height_index,
    //                 width: width_index,
    //                 length: num_acc.len(),
    //                 number: number,
    //             };
    //             found_numbers.push(found);
    //             num_acc.clear();
    //         }
    //     }
    //     if !num_acc.is_empty() {
    //         let number = num_acc.parse::<usize>().expect("Not a number");
    //         let found = Found {
    //             height: height_index,
    //             width: width_index,
    //             length: num_acc.len(),
    //             number: number,
    //         };
    //         found_numbers.push(found);
    //         num_acc.clear();
    //     }
    // }
}

fn part2_solution() {
    let contents = read_challenge("../data/year2023/challenge3/sample.txt");
}

pub fn run() {
    let part1_solution = part1_solution();
    println!("Part1: {:#?}", part1_solution);

    let part2_solution = part2_solution();
    println!("Part2: {:#?}", part2_solution);
}
