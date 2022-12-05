#![feature(array_chunks)]
#![feature(iterator_try_collect)]
#![feature(split_array)]

use std::collections::{HashMap, HashSet};
use std::slice::ArrayChunks;
use std::{error::Error, io::stdin};

fn main() -> Result<(), Box<dyn Error>> {
    let mut left_items = HashSet::<char>::new();
    let mut right_items = HashSet::<char>::new();

    let mut score_table = HashMap::<char, u32>::new();
    ('a'..='z').enumerate().for_each(|(idx, item)| {
        score_table.insert(item, (idx + 1).try_into().unwrap());
    });
    ('A'..='Z').enumerate().for_each(|(idx, item)| {
        score_table.insert(item, (idx + 27).try_into().unwrap());
    });

    let lines = stdin().lines().try_collect::<Vec<_>>()?;

    let mut score = 0u32;
    for line in &lines {
        let (left, right) = line.split_at(line.len() / 2);

        left_items.extend(left.chars());
        right_items.extend(right.chars());

        let common_items = left_items.intersection(&right_items);

        for item in common_items {
            score += score_table[item];
        }

        left_items.clear();
        right_items.clear();
    }

    left_items.clear();
    right_items.clear();

    let mut group_score = 0u32;
    for chunk in lines.array_chunks::<6>() {
        let (first_group, second_group) = chunk.split_array_ref::<3>();
        left_items = first_group
            .iter()
            .map(|s| HashSet::from_iter(s.chars()))
            .reduce(|acc, e| acc.intersection(&e).cloned().collect())
            .unwrap();
        right_items = second_group
            .iter()
            .map(|s| HashSet::from_iter(s.chars()))
            .reduce(|acc, e| acc.intersection(&e).cloned().collect())
            .unwrap();

        group_score += left_items.iter().map(|x| score_table[x]).sum::<u32>();
        group_score += right_items.iter().map(|x| score_table[x]).sum::<u32>();

        left_items.clear();
        right_items.clear();
    }

    println!("Part 1: {}", score);
    println!("Part 2: {}", group_score);

    Ok(())
}
