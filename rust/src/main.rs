mod challenges;
mod file;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 3 {
        eprintln!("Usage: advent_of_code <year> <challenge_number>");
        return;
    }

    let year = &args[1];
    let challenge_number = &args[2];

    match (year.as_str(), challenge_number.as_str()) {
        ("2023", "1") => challenges::year2023::challenge1::run(),
        ("2023", "2") => challenges::year2023::challenge2::run(),
        ("2023", "3") => challenges::year2023::challenge3::run(),
        ("2023", "5") => challenges::year2023::challenge5::run(),
        // Add more cases as needed
        _ => eprintln!("Challenge not found"),
    }
}
