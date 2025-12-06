use std::fs::{self};

type Matrix = Vec<Vec<String>>;
fn read_input(fname: &str) -> Vec<Vec<String>> {
    fs::read_to_string(fname)
        .unwrap()
        .lines()
        .map(|f| {
            f.split(' ')
                .filter(|s| !s.is_empty())
                .map(|a| a.to_string())
                .collect()
        })
        .collect()
}

fn read_grid(fname: &str) -> Vec<Vec<char>> {
    let grid: Vec<Vec<char>> = fs::read_to_string(&fname)
        .unwrap()
        .lines()
        .map(|f| f.chars().collect())
        .collect();

    return grid;
}

fn get_col_values(matrix: &Matrix, col_index: usize) -> Vec<String> {
    let mut acc = vec![];
    for i in 0..matrix.len() - 1 {
        acc.push(matrix[i][col_index].clone());
    }
    return acc;
}

fn eval_col(matrix: &Matrix, col_index: usize) -> u64 {
    let is_sum = &matrix.last().unwrap()[col_index] == "+";

    let mut acc: u64 = matrix[0][col_index].parse().unwrap();

    for i in 1..matrix.len() - 1 {
        let current: u64 = matrix[i][col_index].parse().unwrap();
        if is_sum {
            acc += current
        } else {
            acc *= current
        }
    }

    return acc;
}

fn evaluate(vals: &[u64], operand: char) -> u64 {
    let mut acc = vals[0];
    for val in vals.iter().skip(1) {
        if operand == '+' {
            acc += val
        } else {
            acc *= val
        }
    }

    return acc;
}

fn part1_solve(input: &Matrix) -> u64 {
    let mut acc = 0;
    for col in 0..input[0].len() {
        acc += eval_col(input, col)
    }

    return acc;
}

fn is_column_separator(grid: &Vec<Vec<char>>, col: usize) -> bool {
    for i in 0..grid.len() {
        if grid[i][col] != ' ' {
            return false;
        }
    }

    return true;
}
fn find_col_ranges(grid: &Vec<Vec<char>>) -> Vec<(usize, usize)> {
    let mut acc = vec![];
    let mut current_col_start = 0;
    for i in 0..grid[0].len() - 1 {
        if is_column_separator(grid, i) {
            acc.push((current_col_start, i - 1));
            current_col_start = i + 1;
        }
    }

    acc.push((current_col_start, grid[0].len() - 1));

    return acc;
}

fn get_val_for_col(grid: &Vec<Vec<char>>, col: usize) -> u64 {
    let mut acc = vec![];
    for i in 0..grid.len() - 1 {
        if grid[i][col] != ' ' {
            acc.push(grid[i][col]);
        }
    }
    acc.into_iter().collect::<String>().parse().unwrap()
}

fn get_column(grid: &Vec<Vec<char>>, col_start: usize, col_end: usize) -> (Vec<u64>, char) {
    let mut acc = vec![];

    for i in col_start..=col_end {
        let col_val = get_val_for_col(grid, i);
        acc.push(col_val);
    }

    let operand = grid.last().unwrap()[col_start];
    (acc, operand)
}

fn part2_solve(grid: &Vec<Vec<char>>) -> u64 {
    let col_ranges = find_col_ranges(&grid);
    col_ranges
        .iter()
        .map(|(start, end)| get_column(&grid, *start, *end))
        .map(|(vals, op)| evaluate(&vals, op))
        .sum()
}

fn main() {
    let input = read_input("data.txt");
    println!("Part 1: {}", part1_solve(&input));

    let input = read_grid("data.txt");

    println!("Part 2: {}", part2_solve(&input));
}
