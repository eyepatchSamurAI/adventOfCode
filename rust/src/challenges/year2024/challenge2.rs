use std::cmp::Ordering;
use crate::file::read_challenge;

fn format_input(content: String) -> Vec<Vec<i32>> {
    content
        .lines()
        .map(|line| {
            line.split_whitespace()
                .map(|num| num.parse::<i32>().unwrap())
                .collect()
        })
        .collect()
}

fn is_report_safe(report: &Vec<i32>, increment_direction: Ordering) -> bool {
    report.iter().map_windows(|&[current, next] | {
        let current_pair_ord = next.cmp(current);
        if current_pair_ord != increment_direction {
            return false;
        }
        let difference = next.abs_diff(*current);
        difference >= 1 && difference <= 3 

    }).all(|is_valid_inc| is_valid_inc)
}

fn number_of_safe_reports(reports: Vec<Vec<i32>>) -> usize {
    reports.iter().map(|report| {
        let increment_direction = report[1].cmp(&report[0]);
        match increment_direction {
            Ordering::Equal => false as usize,
            _ => {
                is_report_safe(report, increment_direction) as usize
            }
        }
    }).sum()
}

fn part1_solution() -> usize {
    let contents = read_challenge("../data/year2024/challenge2/puzzle.txt");
    let formatted_input = format_input(contents);
    number_of_safe_reports(formatted_input)
}

fn is_report_safe_with_dampener(report: &Vec<i32>, increment_direction: Ordering, already: bool) -> bool {
    if is_report_safe(report, increment_direction) {
        return true;
    }
    if already {
        return false;
    }
    for i in 0..report.len() {
        let mut modified_report = report.clone();
        modified_report.remove(i);
        if is_report_safe(&modified_report, increment_direction) {
            return true;
        }
    }
    false
}

// fn is_report_safe_with_dampener(report: &Vec<i32>, increment_direction: Ordering) -> bool {
//     if is_report_safe_with_error(report, increment_direction, false) {
//         return true;
//     }
//     for i in 0..report.len() {
//         let mut modified_report = report.clone();
//         modified_report.remove(i);
//         if is_report_safe_with_error(&modified_report, increment_direction, true) {
//             return true;
//         }
//     }
//     false
// }

fn is_report_safe_with_error(report: &Vec<i32>, increment_direction: Ordering, use_fix_value: bool) -> bool {
    let mut prev = vec![];
    let mut used_fix = use_fix_value;
    let is_report_safe = report.iter().map_windows(|&[current, next] | {
        let current_pair_ord = next.cmp(current);
        println!("window: {}, {}", current, next);

        let difference = next.abs_diff(*current);
        let is_good = difference >= 1 && difference <= 3 && current_pair_ord == increment_direction;
        if is_good {
            prev.push(current);
            return true
        } else if !used_fix {
            println!("using fix");
            let the_prev = **prev.last().unwrap_or(&next);
            let fix_diff = next.abs_diff(the_prev);
            println!("{}, {}",the_prev, next);
            let is_fix_good = fix_diff >= 1 && fix_diff <= 3;
            used_fix = true;
            prev.push(next);
            return is_fix_good;
        } else {
            return false;
        }
    }).all(|x| x);
    println!("is_report_safe: {}", is_report_safe);
    is_report_safe
}

// fn number_of_safe_reports_with_reactor(reports: Vec<Vec<i32>>) -> usize {
//     reports.iter().map(|report| {
//         let increment_direction = report[1].cmp(&report[0]);
//         match increment_direction {
//             Ordering::Equal => {
//                 let mut report_clone = report.clone();
//                 report_clone.remove(0);
//                 is_report_safe_with_error(&report_clone, increment_direction, true) as usize
//             },
//             _ => {
//                 is_report_safe_with_error(report, increment_direction, false) as usize
//             }
//         }
//     }).sum()
// }

fn number_of_safe_reports_with_reactor(reports: Vec<Vec<i32>>) -> usize {
    reports.iter().map(|report| {
        let increment_direction = report[1].cmp(&report[0]);
        match increment_direction {
            Ordering::Equal => {
                let mut report_clone = report.clone();
                report_clone.remove(0);
                let increment_direction2 = report_clone[1].cmp(&report[0]);

                is_report_safe_with_dampener(&report_clone, increment_direction2, true) as usize
            },
            _ => {
                is_report_safe_with_dampener(report, increment_direction, false) as usize
            }
        }
    }).sum()
}


fn is_safe(report: &[i32]) -> bool {
    if report.len() < 2 {
        return false;
    }

    // Compute differences between adjacent levels
    let differences: Vec<i32> = report.windows(2).map(|w| w[1] - w[0]).collect();

    // Sequence must be strictly increasing or decreasing (no zero differences)
    if differences.contains(&0) {
        return false;
    }

    let all_positive = differences.iter().all(|&d| d > 0);
    let all_negative = differences.iter().all(|&d| d < 0);

    // Differences must not change sign (sequence must be monotonic)
    if !all_positive && !all_negative {
        return false;
    }

    // Differences must be within -3 to +3 (excluding zero)
    let within_range = differences.iter().all(|&d| d.abs() <= 3);

    if !within_range {
        return false;
    }

    true
}

/// Checks if a report can be made safe by removing at most one level.
fn is_safe_with_removal(report: &[i32]) -> bool {
    if is_safe(report) {
        return true;
    }

    // Try removing each level in turn
    for i in 0..report.len() {
        let mut modified_report = report.to_vec();
        modified_report.remove(i);
        if is_safe(&modified_report) {
            return true;
        }
    }

    false
}

fn part2_solution() -> usize {
    // let contents = read_challenge("../data/year2024/challenge2/sample.txt");
    let contents = read_challenge("../data/year2024/challenge2/puzzle.txt");
    let formatted_input = format_input(contents);
    // number_of_safe_reports_with_reactor(formatted_input)} // 498 too low // 580 too high  // 521, 520, 555, 524 incorrect
    formatted_input
        .iter()
        .filter(|report| is_safe_with_removal(report))
        .count()
}

pub fn run() {
    let part1_solution = part1_solution();
    println!("Part1: {:#?}", part1_solution);

    let part2_solution = part2_solution();
    println!("Part2: {:#?}", part2_solution);
}
