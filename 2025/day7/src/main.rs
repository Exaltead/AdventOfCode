use std::collections::HashMap;

use crate::grid::Grid;

mod grid;

fn part1_solve(grid: &Grid) -> u64 {
    let mut grid = grid.clone();
    let mut counter = 0;
    for y in 1..grid.height {
        for x in 0..grid.width {
            if grid[(x, y - 1)] == 'S' {
                grid[(x, y)] = '|'
            }

            if grid[(x, y - 1)] == '|' && grid[(x, y)] == '^' {
                counter += 1;
                // No splitter at edge of map
                grid[(x + 1, y)] = '|';
                grid[(x - 1, y)] = '|';
            } else if grid[(x, y - 1)] == '|' {
                grid[(x, y)] = '|';
            }
        }
    }
    counter
}

fn count_paths(
    grid: &Grid,
    (x, y): (usize, usize),
    table: &mut HashMap<(usize, usize), u64>,
) -> u64 {
    if table.contains_key(&(x, y)) {
        return *table.get(&(x, y)).unwrap();
    }
    if y == grid.height - 1 {
        return 1;
    }
    if grid[(x, y + 1)] == '.' {
        let val = count_paths(grid, (x, y + 1), table);
        table.insert((x, y), val);
        table.insert((x, y + 1), val);
        return val;
    } else {
        let left = count_paths(grid, (x - 1, y + 1), table);
        table.insert((x - 1, y + 1), left);
        let right = count_paths(grid, (x + 1, y - 1), table);
        table.insert((x + 1, y + 1), right);
        return left + right;
    }
}

fn part2_solve(grid: &Grid) -> u64 {
    let mut memoization_table = HashMap::new();
    let start = grid.iter().find(|f| f.value == 'S').unwrap();
    count_paths(grid, (start.x, 1), &mut memoization_table)
}

fn main() {
    let grid = Grid::read_grid("data.txt");
    println!("Part 1 {}", part1_solve(&grid));
    println!("Part 2 {}", part2_solve(&grid));
}
