use std::process::exit;

use crate::file::read_challenge;

const MAGIC_WORD: &str = "XMAS";
const MAS_WORD: &str = "MAS";
const MAS_WORD_REV: &str = "SAM";

fn is_match(first: String) -> bool {
    (first == MAGIC_WORD)
}

fn collect_direction<F>(
    word_search: &Vec<Vec<char>>,
    range: std::ops::Range<usize>,
    direction_fn: F,
) -> String
where
    F: Fn(usize) -> Option<(usize, usize)>,
{
    range
        .filter_map(|i| {
            direction_fn(i).and_then(|(row, col)| word_search.get(row).and_then(|row| row.get(col)))
        })
        .collect()
}

fn search(word_search: &Vec<Vec<char>>, row_pos: usize, column_pos: usize) -> usize {
    let mut number_found = 0;
    let current_row_length = word_search[row_pos].len();
    let current_col_length = word_search.len();

    // right
    if column_pos + 3 < current_row_length {
        let right = collect_direction(&word_search, 0..4, |i| Some((row_pos, column_pos + i)));
        // println!("right: {}", right);
        number_found += is_match(right) as usize;
    }

    // left
    if (column_pos as i32) - 3 >= 0 {
        let left = collect_direction(&word_search, 0..4, |i| Some((row_pos, column_pos - i)));
        // println!("left: {}", left);
        number_found += is_match(left) as usize;
    }

    // Up
    if (row_pos as i32) - 3 >= 0 {
        let up = collect_direction(&word_search, 0..4, |i| Some((row_pos - i, column_pos)));
        // println!("up: {}", up);
        number_found += is_match(up) as usize;
    }

    // down
    if row_pos + 3 < current_col_length {
        let down = collect_direction(&word_search, 0..4, |i| Some((row_pos + i, column_pos)));
        // println!("down: {}", down);
        number_found += is_match(down) as usize;
    }

    // right_down_diagonal
    if column_pos + 3 < current_row_length && row_pos + 3 < current_col_length {
        let right_down_diagonal =
            collect_direction(&word_search, 0..4, |i| Some((row_pos + i, column_pos + i)));
        // println!("right_down_diagonal: {}", right_down_diagonal);
        number_found += is_match(right_down_diagonal) as usize;
    }

    // left_down_diagonal
    if (column_pos as i32) - 3 >= 0 && row_pos + 3 < current_col_length {
        let right_down_diagonal =
            collect_direction(&word_search, 0..4, |i| Some((row_pos + i, column_pos - i)));
        // println!("left_down_diagonal: {}", right_down_diagonal);
        number_found += is_match(right_down_diagonal) as usize;
    }

    // right_up_diagonal
    if column_pos + 3 < current_row_length && (row_pos as i32) - 3 >= 0 {
        let right_down_diagonal =
            collect_direction(&word_search, 0..4, |i| Some((row_pos - i, column_pos + i)));
        // println!("right_up_diagonal: {}", right_down_diagonal);
        number_found += is_match(right_down_diagonal) as usize;
    }

    // left_up_diagonal
    if (column_pos as i32) - 3 >= 0 && (row_pos as i32) - 3 >= 0 {
        let right_down_diagonal =
            collect_direction(&word_search, 0..4, |i| Some((row_pos - i, column_pos - i)));
        // println!("left_up_diagonal: {}", right_down_diagonal);
        number_found += is_match(right_down_diagonal) as usize;
    }
    number_found
}

fn ai_search(word_search: &Vec<Vec<char>>, row_pos: usize, column_pos: usize) -> usize {
    let directions = [
        (0, 1),   // right
        (0, -1),  // left
        (-1, 0),  // up
        (1, 0),   // down
        (1, 1),   // right_down_diagonal
        (1, -1),  // left_down_diagonal
        (-1, 1),  // right_up_diagonal
        (-1, -1), // left_up_diagonal
    ];

    directions
        .iter()
        .fold(0, |number_found, &(row_step, col_step)| {
            let result = collect_direction(&word_search, 0..4, |i| {
                let row = row_pos as isize + i as isize * row_step;
                let col = column_pos as isize + i as isize * col_step;
                if row >= 0 && col >= 0 {
                    Some((row as usize, col as usize))
                } else {
                    None
                }
            });
            number_found + is_match(result) as usize
        })
}

fn part1_solution() -> usize {
    let contents = read_challenge("../data/year2024/challenge4/puzzle.txt");
    let word_search = contents
        .lines()
        .map(|line| line.chars().collect())
        .collect::<Vec<Vec<char>>>();
    let length = word_search[0].len();

    let total: usize = (0..word_search.len())
        .map(|row_pos| {
            (0..length)
                .map(|col_pos| search(&word_search, row_pos, col_pos))
                .sum::<usize>()
        })
        .sum();
    total
}

fn is_match_any_dir(first: String) -> bool {
    first == MAS_WORD || first == MAS_WORD_REV
}

fn collect_mas_direction<F>(
    word_search: &Vec<Vec<char>>,
    range: std::ops::Range<isize>,
    direction_fn: F,
) -> String
where
    F: Fn(isize) -> Option<(usize, usize)>,
{
    range
        .filter_map(|i| {
            direction_fn(i).and_then(|(row, col)| word_search.get(row).and_then(|row| row.get(col)))
        })
        .collect()
}

fn find_x_mas_pattern(word_search: &Vec<Vec<char>>, row_pos: usize, column_pos: usize) -> usize {
    let diagonals_right = [(1, 1), (-1, 1)]; // Top-right and bottom-right
    let diagonals_left = [(1, -1), (-1, -1)]; // Top-left and bottom-left
    let all_directions = [diagonals_right, diagonals_left];

    let collected_diagonals = all_directions
        .iter()
        .map(|direction_set| {
            direction_set
                .iter()
                .map(|&(row_step, col_step)| {
                    collect_mas_direction(word_search, -1..2, |i| {
                        let row = row_pos as isize + i * row_step;
                        let col = column_pos as isize + i * col_step;

                        if row >= 0 && col >= 0 {
                            Some((row as usize, col as usize))
                        } else {
                            None
                        }
                    })
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let all_match = collected_diagonals
        .iter()
        .all(|diagonal_set| diagonal_set.iter().all(|direction| is_match_any_dir(direction.to_string())));

    all_match as usize
}

fn part2_solution() -> usize {
    let contents = read_challenge("../data/year2024/challenge4/puzzle.txt");
    let word_search = contents
        .lines()
        .map(|line| line.chars().collect())
        .collect::<Vec<Vec<char>>>();
    let length = word_search[0].len();

    let total = (1..word_search.len() - 1)
        .map(|row_pos| {
            (1..length - 1)
                .map(|col_pos| find_x_mas_pattern(&word_search, row_pos, col_pos))
                .sum::<usize>()
        })
        .collect::<Vec<_>>();
    total.iter().sum()
}

pub fn run() {
    let part1_solution = part1_solution();
    println!("Part1: {:#?}", part1_solution);

    let part2_solution = part2_solution();
    println!("Part2: {:#?}", part2_solution);
}
