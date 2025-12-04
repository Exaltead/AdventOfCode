use core::fmt;
use std::fs;

#[derive(Clone)]
struct Grid {
    width: usize,
    height: usize,
    grid: Vec<Vec<char>>,
}

impl Grid {
    fn read_grid(fname: &str) -> Grid {
        let grid = fs::read_to_string(fname)
            .unwrap()
            .lines()
            .map(|f| {
                return f.chars().collect::<Vec<char>>();
            })
            .collect::<Vec<Vec<char>>>();

        Grid {
            width: grid[0].len(),
            height: grid.len(),
            grid,
        }
    }

    fn from(items: Vec<Vec<char>>) -> Grid {
        Grid {
            width: items[0].len(),
            height: items.len(),
            grid: items,
        }
    }

    fn get_adjacent(&self, x: usize, y: usize) -> Vec<GridPosition> {
        let mut positions: Vec<(usize, usize)> = vec![];
        let left_border = x == 0;
        let right_border = x == self.width - 1;
        let top_border = y == 0;
        let bottom_border = y == self.height - 1;

        if !left_border {
            positions.push((x - 1, y));
        }
        if !top_border && !left_border {
            positions.push((x - 1, y - 1));
        }
        if !top_border {
            positions.push((x, y - 1));
        }

        if !top_border && !right_border {
            positions.push((x + 1, y - 1));
        }
        if !right_border {
            positions.push((x + 1, y));
        }
        if !right_border && !bottom_border {
            positions.push((x + 1, y + 1));
        }
        if !bottom_border {
            positions.push((x, y + 1));
        }
        if !bottom_border && !left_border {
            positions.push((x - 1, y + 1));
        }

        positions
            .iter()
            .map(|(x, y)| {
                //println!("{}, {}", x, y);
                return GridPosition {
                    value: self.grid[*y][*x],
                    x: *x,
                    y: *y,
                };
            })
            .collect()
    }

    fn replace_with(&mut self, replacements: &[GridPosition]) {
        for replacement in replacements {
            self.grid[replacement.y][replacement.x] = replacement.value;
        }
    }

    fn iter<'a>(&'a self) -> GridIterator<'a> {
        return GridIterator {
            grid: &self,
            x: 0,
            y: 0,
        };
    }
}

struct GridIterator<'a> {
    grid: &'a Grid,
    x: usize,
    y: usize,
}

impl<'a> Iterator for GridIterator<'a> {
    type Item = GridPosition;
    fn next(&mut self) -> Option<GridPosition> {
        if self.x >= self.grid.width {
            self.x = 0;
            self.y += 1;
        }
        if self.y >= self.grid.height {
            return None;
        }
        let val = self.grid.grid[self.y][self.x];
        let ret = GridPosition {
            x: self.x,
            y: self.y,
            value: val,
        };
        self.x += 1;

        return Some(ret);
    }
}

struct GridIntoIterator {
    grid: Grid,
    x: usize,
    y: usize,
}

impl Iterator for GridIntoIterator {
    type Item = GridPosition;
    fn next(&mut self) -> Option<GridPosition> {
        if self.x >= self.grid.width {
            self.x = 0;
            self.y += 1;
        }
        if self.y >= self.grid.height {
            return None;
        }
        let val = self.grid.grid[self.y][self.x];
        let ret = GridPosition {
            x: self.x,
            y: self.y,
            value: val,
        };
        self.x += 1;

        return Some(ret);
    }
}

#[derive(Debug)]
struct GridPosition {
    value: char,
    x: usize,
    y: usize,
}

impl IntoIterator for Grid {
    type IntoIter = GridIntoIterator;
    type Item = GridPosition;

    fn into_iter(self) -> Self::IntoIter {
        return GridIntoIterator {
            grid: self,
            x: 0,
            y: 0,
        };
    }
}

impl fmt::Display for Grid {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for y in 0..self.height {
            for x in 0..self.width {
                write!(f, "{}", self.grid[y][x])?;
            }
            writeln!(f, "")?;
        }

        Ok(())
    }
}

fn find_clearable_paper_rolls(grid: &Grid) -> Vec<GridPosition> {
    grid.iter()
        .filter(|pos| pos.value == '@')
        .filter(|pos| {
            grid.get_adjacent(pos.x, pos.y)
                .iter()
                .filter(|apos| apos.value == '@')
                .count()
                < 4
        })
        .collect()
}

fn replace_clearable(grid: &Grid, clearable: &[GridPosition]) -> Grid {
    let mut cloned = grid.clone();
    let replaced = clearable
        .iter()
        .map(|f| GridPosition {
            value: '.',
            x: f.x,
            y: f.y,
        })
        .collect::<Vec<GridPosition>>();
    cloned.replace_with(&replaced);

    return cloned;
}

fn part1_solve(grid: &Grid) -> u32 {
    find_clearable_paper_rolls(&grid).len().try_into().unwrap()
}

fn part2_solve(grid: &Grid) -> u32 {
    let mut sum = 0;
    let mut grid = grid.clone();
    loop {
        let clearable = find_clearable_paper_rolls(&grid);
        //println!("{}", clearable.len());
        if clearable.len() == 0 {
            break;
        }

        sum += clearable.len();
        grid = replace_clearable(&grid, &clearable)

    }

    sum.try_into().unwrap()
}

fn main() {
    let grid = Grid::read_grid("data.txt");
    let count = part1_solve(&grid);
    println!("Part 1: {}", count);
    let count = part2_solve(&grid);
    println!("Part 2: {}", count);
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn grid_replace_works() {
        let mut grid = Grid::from(vec![vec!['a', 'b'], vec!['c', 'd']]);

        grid.replace_with(&vec![
            GridPosition {
                x: 1,
                y: 1,
                value: 'p',
            },
            GridPosition {
                x: 0,
                y: 1,
                value: 'l',
            },
        ]);

        assert_eq!('l', grid.grid[1][0]);
        assert_eq!('p', grid.grid[1][1]);
    }
}
