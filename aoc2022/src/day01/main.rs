use std::collections::BinaryHeap;
use std::io::stdin;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut totals = vec![0u32];

    for line in stdin().lines() {
        let content = line?;
        // Start calculating totals for a new elf
        if content.is_empty() {
            totals.push(0);
        } else {
            let calories = content.parse::<u32>()?;
            let last = totals.last_mut().unwrap();
            *last += calories;
        }
    }
    totals.sort_by_key(|pair| std::cmp::Reverse(*pair));
    let mut heap = BinaryHeap::from_iter(totals.iter());

    let maximum = heap.peek().unwrap();
    println!("Part 1: {}", maximum);

    let mut top3 = 0;
    for _ in 0..3 {
        top3 += heap.pop().unwrap();
    }
    println!("Part 2: {}", top3);
    Ok(())
}
