#![feature(iterator_try_collect)]

use std::{error::Error, io::stdin, ops::RangeInclusive};

trait RangeExtensions {
    type Idx;
    fn covers(&self, other: &RangeInclusive<Self::Idx>) -> bool;
    fn overlaps(&self, other: &RangeInclusive<u32>) -> bool;
}

impl RangeExtensions for RangeInclusive<u32> {
    type Idx = u32;

    fn covers(&self, other: &RangeInclusive<u32>) -> bool {
        self.start() <= other.start() && self.end() >= other.end()
    }

    fn overlaps(&self, other: &RangeInclusive<u32>) -> bool {
        self.contains(other.start()) || self.contains(other.end())
            || other.contains(self.start())
            || other.contains(self.end())
    }
}

fn range_from_input(text: &str) -> RangeInclusive<u32> {
    let (start, end) = text.split_once('-').expect("Missing '-'");
    (start.parse::<u32>().unwrap())..=(end.parse::<u32>().unwrap())
}

fn main() -> Result<(), Box<dyn Error>> {
    let lines = stdin().lines().try_collect::<Vec<_>>()?;
    let mut covered_count = 0u32;
    let mut overlap_count = 0u32;

    for line in lines {
        let (left, right) = line.split_once(',').expect("No comma on line");
        let left_range = range_from_input(left);
        let right_range = range_from_input(right);

        if left_range.covers(&right_range) || right_range.covers(&left_range) {
            covered_count += 1;
        }
        if left_range.overlaps(&right_range) {
            overlap_count += 1;
        }
    }

    println!("Part 1: {covered_count}");
    println!("Part 2: {overlap_count}");

    Ok(())
}
