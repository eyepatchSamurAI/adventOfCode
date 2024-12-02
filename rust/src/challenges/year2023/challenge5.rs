use crate::file::read_challenge;

fn get_seeds_and_ranges(contents: &String) -> (Vec<u64>, Vec<Vec<u64>>){
    let mut seeds: Vec<u64> = Vec::new();
    let mut all_mappings = Vec::new();
    contents.lines().for_each(|line| {
        if line.starts_with("seeds:") {
            seeds = line.split(' ').skip(1).map(|str_number| str_number.parse::<u64>().unwrap()).collect();
            return;
        }
        let first_char = line.chars().next();
        if let Some(first_char) = first_char {
            if first_char.is_digit(10) {
                let ranges : Vec<u64> = line.split(' ').map(|str_number| str_number.parse::<u64>().unwrap()).collect();
                all_mappings.push(ranges);
            }
        }
    });
    return (seeds, all_mappings)
}

fn part1_solution() {
    let contents = read_challenge("../data/year2023/challenge5/sample.txt");
    let (seeds, all_mappings) = get_seeds_and_ranges(&contents);
    
    
    println!("seeds: {:?}", seeds);
    println!("allMappings: {:?}", all_mappings);
}
fn part2_solution() {}

pub fn run() {
    let part1_solution = part1_solution();
    println!("Part1: {:#?}", part1_solution);

    let part2_solution = part2_solution();
    println!("Part2: {:#?}", part2_solution);
}
