use core::fmt;
use std::{
    fs,
    ops::{Index, IndexMut},
};

#[derive(Clone)]
pub struct Grid {
    pub width: usize,
    pub height: usize,
    grid: Vec<Vec<char>>,
}

impl Grid {
    pub fn read_grid(fname: &str) -> Grid {
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

    pub fn init(width: usize, height: usize, initial_value: char) -> Grid{
        let grid = vec![vec![initial_value; width]; height];
        Grid { width, height, grid }
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
                return GridPosition {
                    value: self.grid[*y][*x],
                    x: *x,
                    y: *y,
                };
            })
            .collect()
    }

    pub fn replace_with(&mut self, replacements: &[GridPosition]) {
        for replacement in replacements {
            self.grid[replacement.y][replacement.x] = replacement.value;
        }
    }

    pub fn iter<'a>(&'a self) -> GridIterator<'a> {
        return GridIterator {
            grid: &self,
            x: 0,
            y: 0,
        };
    }
}

pub struct GridIterator<'a> {
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

pub struct GridIntoIterator {
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
pub struct GridPosition {
    pub value: char,
    pub x: usize,
    pub y: usize,
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

impl Index<(usize, usize)> for Grid {
    type Output = char;

    fn index(&self, (x, y): (usize, usize)) -> &char {
        return &self.grid[y][x];
    }
}

impl IndexMut<(usize, usize)> for Grid {
    fn index_mut(&mut self, (x, y): (usize, usize)) -> &mut Self::Output {
        return &mut self.grid[y][x];
    }
}
