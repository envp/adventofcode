#![allow(dead_code)]
use std::error::Error;
use std::io::{stdin, BufRead};

#[derive(Debug, PartialEq, Eq, Clone)]
enum Move {
    Rock,
    Paper,
    Scissors,
}

impl Move {
    fn as_score(&self) -> u32 {
        match self {
            Self::Rock => 1,
            Self::Paper => 2,
            Self::Scissors => 3,
        }
    }

    fn compare(&self, other: &Self) -> Outcome {
        match (self, other) {
            (Move::Rock, Move::Rock) => Outcome::Draw,
            (Move::Rock, Move::Paper) => Outcome::Loss,
            (Move::Rock, Move::Scissors) => Outcome::Win,
            (Move::Paper, Move::Rock) => Outcome::Win,
            (Move::Paper, Move::Paper) => Outcome::Draw,
            (Move::Paper, Move::Scissors) => Outcome::Loss,
            (Move::Scissors, Move::Rock) => Outcome::Loss,
            (Move::Scissors, Move::Paper) => Outcome::Win,
            (Move::Scissors, Move::Scissors) => Outcome::Draw,
        }
    }
}

#[derive(Debug)]
enum Outcome {
    Loss,
    Draw,
    Win,
}

impl Outcome {
    fn as_score(&self) -> u32 {
        match self {
            Outcome::Loss => 0,
            Outcome::Draw => 3,
            Outcome::Win => 6,
        }
    }

    /// Return the move to play given the outcome and opposing move
    fn which_move(&self, lhs: &Move) -> Move {
        match self {
            Self::Draw => lhs.clone(),
            Self::Loss => match lhs {
                Move::Rock => Move::Scissors,
                Move::Paper => Move::Rock,
                Move::Scissors => Move::Paper,
            },
            Self::Win => match lhs {
                Move::Rock => Move::Paper,
                Move::Paper => Move::Scissors,
                Move::Scissors => Move::Rock,
            },
        }
    }
}

mod part1 {
    use super::Move;

    fn opponent_move_from_text(text: &str) -> Move {
        match text {
            "A" => Move::Rock,
            "B" => Move::Paper,
            "C" => Move::Scissors,
            _ => unreachable!("Impossible opponent move"),
        }
    }

    fn our_move_from_text(text: &str) -> Move {
        match text {
            "X" => Move::Rock,
            "Y" => Move::Paper,
            "Z" => Move::Scissors,
            _ => unreachable!("Impossible move"),
        }
    }

    pub fn compute_score(theirs: &str, ours: &str) -> u32 {
        let their_move = opponent_move_from_text(theirs);
        let our_move = our_move_from_text(ours);
        let outcome = our_move.compare(&their_move);

        our_move.as_score() + outcome.as_score()
    }
}

mod part2 {
    use crate::{Move, Outcome};

    fn opponent_move_from_text(text: &str) -> Move {
        match text {
            "A" => Move::Rock,
            "B" => Move::Paper,
            "C" => Move::Scissors,
            _ => unreachable!("Impossible opponent move"),
        }
    }

    fn compute_outcome_from_text(text: &str) -> Outcome {
        match text {
            "X" => Outcome::Loss,
            "Y" => Outcome::Draw,
            "Z" => Outcome::Win,
            _ => unreachable!("Impossible outcome"),
        }
    }

    pub fn compute_score(theirs: &str, ours: &str) -> u32 {
        let their_move = opponent_move_from_text(theirs);
        let outcome = compute_outcome_from_text(ours);
        let our_move = outcome.which_move(&their_move);

        our_move.as_score() + outcome.as_score()
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let input = stdin().lock();
    let mut elf_strategy_guide_score = 0u32;
    let mut our_strategy_guide_score = 0u32;

    for line in input.lines() {
        let content: &str = &line?;
        if content.is_empty() {
            break;
        }
        let (theirs, ours) = content.split_once(" ").expect("Failed to split line");
        elf_strategy_guide_score += part1::compute_score(theirs, ours);
        our_strategy_guide_score += part2::compute_score(theirs, ours);
    }

    println!("Part 1: {}", elf_strategy_guide_score);
    println!("Part 2: {}", our_strategy_guide_score);
    Ok(())
}
