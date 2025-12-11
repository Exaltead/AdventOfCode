use std::fs;

use crate::grid::Grid;

mod grid;

#[derive(Debug, Clone)]
struct Point {
    x: i64,
    y: i64,
}

impl Point {
    fn new(str: &str) -> Point {
        let (x, y) = str.split_once(',').unwrap();
        Point {
            x: x.parse().unwrap(),
            y: y.parse().unwrap(),
        }
    }

    fn area_between(&self, other: &Point) -> i64 {
        let x_len = (self.x - other.x).abs() + 1;
        let y_len = (self.y - other.y).abs() + 1;

        return x_len * y_len;
    }

    fn as_pos(&self) -> (usize, usize) {
        return (self.x.try_into().unwrap(), self.y.try_into().unwrap());
    }

    fn pos_between(&self, other: &Point) -> Vec<(usize, usize)> {
        let mut poses = vec![];
        if self.x == other.x {
            let (start, end) = if self.y < other.y {
                (self.y, other.y)
            } else {
                (other.y, self.y)
            };
            for i in start..=end {
                poses.push((self.x.try_into().unwrap(), i.try_into().unwrap()));
            }
        } else {
            let (start, end) = if self.x < other.x {
                (self.x, other.x)
            } else {
                (other.x, self.x)
            };
            for i in start..=end {
                poses.push((i.try_into().unwrap(), self.y.try_into().unwrap()));
            }
        }

        poses
    }
}
#[derive(PartialEq)]
enum Direction {
    Vertical,
    Horizontal,
}

struct Edge {
    start: Point,
    end: Point,
    direction: Direction,
}
fn point_inside(seg_f: i64, seg_s: i64, val: i64) -> bool {
    if seg_f > seg_s {
        return seg_s <= val && val <= seg_f;
    }
    return seg_f <= val && val <= seg_s;
}

impl Edge {
    fn from(start: &Point, end: &Point) -> Edge {
        // as the movement is on the grid, each edge may only mov up or down
        let direction = if start.x - end.x == 0 {
            Direction::Vertical
        } else {
            Direction::Horizontal
        };
        Edge {
            start: start.clone(),
            end: end.clone(),
            direction: direction,
        }
    }

    fn intersects(&self, other: &Edge) -> bool {
        if self.direction == other.direction {
            return false;
        }

        let (horizontal, vertical) = if self.direction == Direction::Horizontal {
            (self, other)
        } else {
            (other, self)
        };

        return point_inside(horizontal.start.x, horizontal.start.x, other.start.x)
            && point_inside(vertical.start.y, vertical.end.y, horizontal.start.y);
    }
}

fn materialize(points: &[Point]) -> Grid {
    let max_x = points.iter().max_by_key(|f| f.x).unwrap().x;
    let max_y = points.iter().max_by_key(|f| f.y).unwrap().y;

    println!("{} {}", max_x, max_y);
    let mut grid = Grid::init(
        (max_x + 1).try_into().unwrap(),
        (max_y + 1).try_into().unwrap(),
        '.',
    );

    let mut prev = &points[0];
    for point in points.iter().skip(1) {
        for pos in prev.pos_between(point) {
            grid[pos] = 'X'
        }
        grid[prev.as_pos()] = '#';
        prev = point
    }

    for pos in prev.pos_between(&points[0]) {
        grid[pos] = 'X'
    }
    grid[prev.as_pos()] = '#';
    grid[points[0].as_pos()] = '#';

    grid
}


fn read_input(fname: &str) -> Vec<Point> {
    fs::read_to_string(fname)
        .unwrap()
        .lines()
        .map(Point::new)
        .collect()
}

fn part1_solve(points: &[Point]) -> i64 {
    let mut areas: Vec<i64> = vec![];
    for (i, point) in points.iter().enumerate() {
        for other in &points[i + 1..points.len()] {
            //println!("{:?} to {:?} for {}", point, other, point.area_between(other));
            areas.push(point.area_between(other));
        }
    }

    let biggest = areas.iter().copied().max().unwrap();
    return biggest;
}

fn part2_solve(points: &[Point]) {
    let grid = materialize(points);

    //println!("{}", grid)
}

fn main() {
    let input = read_input("data.txt");
    println!("Part 1: {}", part1_solve(&input));
    part2_solve(&input);
}
