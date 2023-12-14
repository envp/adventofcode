use std::{error::Error, io::Read};

/// The newly-improved calibration document consists of lines of text; each line originally
/// contained a specific calibration value that the Elves now need to recover. On each line, the
/// calibration value can be found by combining the first digit and the last digit (in that order)
/// to form a single two-digit number.
///
/// For example:
///
/// 1abc2
/// pqr3stu8vwx
/// a1b2c3d4e5f
/// treb7uchet
///
/// In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding
/// these together produces 142.
///
/// Consider your entire calibration document. What is the sum of all of the calibration values?
fn solve_part1(input: &str) {
    let mut total = 0;
    for (idx, line) in input.lines().enumerate() {
        let bytes = line.as_bytes();
        let first_num: u32 = bytes
            .iter()
            .find(|c| c.is_ascii_digit())
            .map(|c| c - b'0')
            .expect(&format!("No ascii digits exist on line {}", idx + 1))
            .into();
        let last_num: u32 = bytes
            .iter()
            .find(|c| c.is_ascii_digit())
            .map(|c| c - b'0')
            .expect(&format!("No ascii digits exist on line {}", idx + 1))
            .into();
        total += 10 * first_num + last_num;
    }
    println!("Part 1: {total}")
}

/// Your calculation isn't quite right. It looks like some of the digits are actually spelled out
/// with letters: one, two, three, four, five, six, seven, eight, and nine also count as valid
/// "digits".
///
/// Equipped with this new information, you now need to find the real first and last digit on each
/// line. For example:
///
/// two1nine
/// eightwothree
/// abcone2threexyz
/// xtwone3four
/// 4nineeightseven2
/// zoneight234
/// 7pqrstsixteen
///
/// In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76. Adding these
/// together produces 281.
///
/// What is the sum of all of the calibration values?
fn solve_part2(input: &str) {
    let mut total = 0;
    enum Direction {
        Forward,
        Reverse,
    }

    let search = |content: &str, direction: Direction| {
        let mut content = content.as_bytes();
        let check_multibyte = |haystack: &[u8], needle: &[u8]| match direction {
            Direction::Forward => haystack.starts_with(needle),
            Direction::Reverse => haystack.ends_with(needle),
        };
        let check_byte = |haystack: &[u8]| match direction {
            Direction::Forward => haystack
                .first()
                .iter()
                .find(|c| c.is_ascii_digit())
                .map(|c| (*c - b'0') as u32),
            Direction::Reverse => haystack
                .last()
                .iter()
                .find(|c| c.is_ascii_digit())
                .map(|c| (*c - b'0') as u32),
        };
        loop {
            if let Some(value) = check_byte(content) {
                return Some(value);
            } else if check_multibyte(content, b"one") {
                return Some(1);
            } else if check_multibyte(content, b"two") {
                return Some(2);
            } else if check_multibyte(content, b"three") {
                return Some(3);
            } else if check_multibyte(content, b"four") {
                return Some(4);
            } else if check_multibyte(content, b"five") {
                return Some(5);
            } else if check_multibyte(content, b"six") {
                return Some(6);
            } else if check_multibyte(content, b"seven") {
                return Some(7);
            } else if check_multibyte(content, b"eight") {
                return Some(8);
            } else if check_multibyte(content, b"nine") {
                return Some(9);
            } else if !content.is_empty() {
                content = match direction {
                    Direction::Forward => content.split_first().unwrap().1,
                    Direction::Reverse => content.split_last().unwrap().1,
                };
            } else {
                break None;
            }
        }
    };

    for line in input.lines() {
        let first_num = search(line, Direction::Forward).unwrap();
        let last_num = search(line, Direction::Reverse).unwrap();
        total += 10 * first_num + last_num;
    }

    println!("Part 2: {total}");
}

fn main() -> Result<(), Box<dyn Error>> {
    let input = if let Some(path) = std::env::args().nth(1) {
        let mut buf = String::new();
        std::fs::OpenOptions::new()
            .read(true)
            .open(path)?
            .read_to_string(&mut buf)?;
        buf
    } else {
        let mut buf = String::new();
        std::io::stdin().read_to_string(&mut buf)?;
        buf
    };

    debug_assert!(
        input.is_ascii(),
        "Found non-ascii characters in input stream!"
    );

    solve_part1(&input);
    solve_part2(&input);
    Ok(())
}
