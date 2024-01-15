use std::fs;

pub fn read_challenge(path: &str) -> String {
    let contents = fs::read_to_string(path)
        .expect("Should have been able to read the file");
    return contents;
}