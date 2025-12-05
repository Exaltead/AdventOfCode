use std::fs;

#[derive(Copy, Clone, Debug)]
struct Range {
    start: u64,
    end: u64,
}

impl Range {
    fn from(content: &str) -> Range {
        let range_parts: Vec<&str> = content.split('-').collect();

        if range_parts.len() != 2 {
            panic!("Range parsing error")
        }

        return Range {
            start: range_parts[0].parse().unwrap(),
            end: range_parts[1].parse().unwrap(),
        };
    }

    fn contains(&self, value: u64) -> bool {
        return self.start <= value && value <= self.end;
    }

    fn overlaps(&self, other: &Range) -> bool {
        return self.contains(other.start) || self.contains(other.end);
    }

    fn merge(&self, other: &Range) -> Range {
        return Range {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        };
    }
}

fn read_input(fname: &str) -> (Vec<Range>, Vec<u64>) {
    let contents = fs::read_to_string(fname).unwrap();
    let parts: Vec<&str> = contents.split("\n\n").collect();
    if parts.len() != 2 {
        panic!("Parsing error");
    }

    let ranges = parts[0].lines().map(|f| Range::from(&f)).collect();

    let ingredients: Vec<u64> = parts[1].lines().map(|f| f.parse().unwrap()).collect();

    (ranges, ingredients)
}

fn part1_solve(ranges: &[Range], ingredients: &[u64]) -> usize {
    ingredients
        .iter()
        .filter(|x| ranges.iter().any(|a| a.contains(**x)))
        .count()
}

fn merge_ranges(ranges: &[Range]) -> Vec<Range> {
    let mut clone = ranges.to_vec();
    clone.sort_by(|f, s| f.start.cmp(&s.start));
    let mut current = clone[0];

    let mut result = vec![];

    for i in 1..clone.len() {
        if current.overlaps(&clone[i]) {
            current = current.merge(&clone[i])
        } else {
            result.push(current);
            current = clone[i]
        }
    }

    result.push(current);

    return result;
}

fn part2_solve(ranges: &[Range]) -> u64 {
    let merged = merge_ranges(ranges);
    return merged.iter().map(|r| r.end - r.start + 1).sum();
}

fn main() {
    let (ranges, ingredients) = read_input("data.txt");
    println!("Part 1: {}", part1_solve(&ranges, &ingredients));
    println!("Part 2: {}", part2_solve(&ranges));
}
