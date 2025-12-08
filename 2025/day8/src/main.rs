use std::{collections::HashSet, fs, time::Instant};

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
struct Point {
    x: i64,
    y: i64,
    z: i64,
}

impl Point {
    fn parse(val: &str) -> Point {
        let parts: Vec<i64> = val.split(',').map(|f| f.parse().unwrap()).collect();
        if parts.len() != 3 {
            panic!("Invalid input")
        }
        Point {
            x: parts[0],
            y: parts[1],
            z: parts[2],
        }
    }

    fn distance_to_sq(&self, other: &Point) -> i64 {
        (self.x - other.x).pow(2) + (self.y - other.y).pow(2) + (self.z - other.z).pow(2)
    }
}

#[derive(Clone, Debug)]
struct Edge {
    a: Point,
    b: Point,
    dist: i64,
}

impl Edge {
    fn between(a: &Point, b: &Point) -> Edge {
        let dist = a.distance_to_sq(b);
        Edge {
            a: *a,
            b: *b,
            dist: dist,
        }
    }
}

fn read_input(fname: &str) -> Vec<Point> {
    fs::read_to_string(fname)
        .unwrap()
        .lines()
        .map(Point::parse)
        .collect()
}

fn get_edges(points: &[Point]) -> Vec<Edge> {
    let mut points = points.to_vec();
    let mut edges = vec![];

    for i in (0..points.len()).rev() {
        let point = points.pop().unwrap();

        edges.extend(points[0..i].iter().map(|f| Edge::between(f, &point)));
    }

    edges
}

fn merge_circuits(circuits: &mut Vec<HashSet<Point>>, first: usize, second: usize) {
    let second_contents: Vec<Point> = circuits[second].iter().copied().collect();
    circuits[first].extend(second_contents.iter());

    circuits.swap_remove(second);
}

fn find_from_circuits(circuits: &Vec<HashSet<Point>>, edge: &Edge) -> (usize, usize) {
    let finds: Vec<usize> = circuits
        .iter()
        .enumerate()
        .filter(|(_, circ)| circ.contains(&edge.a) || circ.contains(&edge.b))
        .take(2)
        .map(|(i, _)| i)
        .collect();

    //println!("{:?}", finds);
    if finds.len() == 2 {
        return (finds[0], finds[1]);
    }
    return (finds[0], finds[0]);
}

fn part1_solve(edges: &[Edge], points: &[Point], iter_count: usize) -> i64 {
    let mut circuits: Vec<HashSet<Point>> =
        points.iter().copied().map(|f| HashSet::from([f])).collect();

    for i in 0..iter_count {
        let (f, s) = find_from_circuits(&circuits, &edges[i]);
        if f == s {
            continue;
        }
        merge_circuits(&mut circuits, f, s);
    }

    circuits.sort_by(|f, s| s.len().cmp(&f.len()));
    circuits
        .iter()
        .map(|f| f.len().try_into().unwrap())
        .take(3)
        .reduce(|acc, e| acc * e)
        .unwrap()
}

fn part2_solve(edges: &[Edge], points: &[Point]) -> i64 {
    let mut circuits: Vec<HashSet<Point>> =
        points.iter().copied().map(|f| HashSet::from([f])).collect();

    let mut i = 0;
    loop {
        //println!("{}", circuits.len());
        if circuits.len() == 1 {
            panic!("WTF")
        }

        let (f, s) = find_from_circuits(&circuits, &edges[i]);
        if f == s {
            i += 1;
            continue;
        }

        if circuits.len() == 2 {
            return edges[i].a.x * edges[i].b.x;
        }
        merge_circuits(&mut circuits, f, s);

        i += 1;
    }
}

fn main() {
    let now = Instant::now();
    let points = read_input("data.txt");
    let mut edges = get_edges(&points);
    edges.sort_by(|f, s| f.dist.cmp(&s.dist));
    println!("Edges ready in {} ms", now.elapsed().as_millis());

    let now = Instant::now();
    let result = part1_solve(&edges, &points, 1000);
    println!("Part 1: {}, in {} ms", result, now.elapsed().as_millis());

    let now = Instant::now();
    let result = part2_solve(&edges, &points);
    println!("Part 2: {} in {} ms", result, now.elapsed().as_millis());
}
