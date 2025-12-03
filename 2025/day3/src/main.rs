use std::fs;

struct Bank {
    batteries: Vec<u64>,
}

fn split_simple(input: &str) -> Vec<u64> {
    input
        .split("")
        .into_iter()
        .skip(1)
        .take(input.len())
        .map(|f| f.parse().unwrap())
        .collect()
}

fn read_input(fname: &str) -> Vec<Bank> {
    fs::read_to_string(&fname)
        .unwrap()
        .lines()
        .map(|line| Bank {
            batteries: split_simple(&line),
        })
        .collect()
}

fn find_biggest_with_min_characters_behind(nums: &[u64], min_remaining: usize) -> (u64, usize) {
    if nums.len() <= min_remaining {
        panic!("Programming error")
    }

    let mut current_pos = 0;
    let mut current_biggest = nums[0];

    for i in 1..nums.len() - min_remaining {
        if current_biggest < nums[i] {
            current_pos = i;
            current_biggest = nums[i]
        }
    }
    return (current_biggest, current_pos);
}

fn find_big(battery: &[u64], desired_length: usize) -> u64 {
    if desired_length == 1 {
        return find_biggest_with_min_characters_behind(&battery, 0).0;
    }
    let (val, pos) = find_biggest_with_min_characters_behind(&battery, desired_length - 1);
    let trail = find_big(&battery[pos + 1..], desired_length - 1);

    return val * 10_u64.pow((desired_length - 1).try_into().unwrap()) + trail;
}

fn main() {
    let input = read_input("data.txt");

    let sum = input.iter().map(|f| find_big(&f.batteries, 2)).sum::<u64>();
    println!("Part1 {}", sum);

    let part2 = input
        .iter()
        .map(|f| find_big(&f.batteries, 12))
        .sum::<u64>();

    println!("Part2 {}", part2);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn split_simple_works() {
        assert_eq!(vec![1, 2, 3, 3], split_simple("1233"))
    }

    #[test]
    fn it_works() {
        assert_eq!(542, find_big(&[1, 2, 3, 5, 4, 1, 2], 3));
    }
}
