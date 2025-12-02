use std::fs;


fn is_invalid(id: &str) -> bool {
    if id.len() % 2 != 0 {
        return false;
    }

    let first_half = &id[..id.len() / 2];
    let second_half = &id[id.len() / 2..];

    return first_half == second_half;
}


fn char_repeats(id: &str, start: usize, hop: usize) -> bool {
    let char = id.chars().nth(start).unwrap();
    for index in (start..id.len()).step_by(hop) {
        if char != id.chars().nth(index).unwrap() {
            return false;
        }
    }

    return true;
}

fn is_invalid_part2_v2(id: &str) -> bool {
    for hop in 1..=id.len() / 2 {
        if  id.len() % hop != 0 {
            continue;
        }
        //println!("{} {}", id, hop);
        let mut all_match = true;
        for start in 0..=hop {
            if !all_match {
                break;
            }
            all_match = all_match && char_repeats(id, start, hop)
        }

        if all_match {
            return true;
        }
    }

    return false;
}

struct Range {
    start: u64,
    end: u64,
}

fn read_inputs(fname: &str) -> Vec<Range> {
    fs::read_to_string(fname)
        .unwrap()
        .split(',')
        .map(|s| {
            let (f, s) = s.split_once('-').unwrap();

            return Range {
                start: f.parse().unwrap(),
                end: s.parse().unwrap(),
            };
        })
        .collect()
}

fn get_invalid_ids_from_range<F>(range: &Range, validator: F) -> Vec<u64>
where
    F: Fn(&str) -> bool,
{
    (range.start..=range.end)
        .into_iter()
        .filter(|x| validator(&x.to_string()))
        .collect()
}

fn part1_solve(ranges: &[Range]) -> u64 {
    let mut sum = 0;
    for range in ranges {
        let invalid_ids = get_invalid_ids_from_range(range, is_invalid);

        sum += invalid_ids.iter().sum::<u64>();
    }

    sum
}

fn part2_solve(ranges: &[Range]) -> u64 {
    let mut sum = 0;
    for range in ranges {
        let invalid_ids = get_invalid_ids_from_range(range, is_invalid_part2_v2);

        sum += invalid_ids.iter().sum::<u64>();
    }

    sum
}

fn main() {
    let ranges = read_inputs("data.txt");
    let part1_solution = part1_solve(&ranges);
    println!("Part 1: {}", part1_solution);
    let part2_solution = part2_solve(&ranges);
    println!("Part 2: {}", part2_solution);
}
