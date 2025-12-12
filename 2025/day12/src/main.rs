use std::fs;

#[derive(Clone)]
struct Area {
    width: u64,
    height: u64,

    areas: Vec<u64>,
}

fn read_areas(fname: &str) -> Vec<Area> {
    fs::read_to_string(fname)
        .unwrap()
        .split("\n\n")
        .last()
        .unwrap()
        .lines()
        .map(|f| {
            let (f, s) = f.split_once(": ").unwrap();
            let (width, height) = f.split_once("x").unwrap();
            let parts = s
                .split(" ")
                .map(|f| f.parse().unwrap())
                .collect::<Vec<u64>>();

            Area {
                width: width.parse().unwrap(),
                height: height.parse().unwrap(),
                areas: parts,
            }
        })
        .collect()
}

fn could_theoretically_fit(area: &Area) -> bool {
    let total_size_required: u64 = area
        .areas
        .iter()
        .enumerate()
        .map(|(i, val)| {
            return match i {
                0 => 7 * val,
                1 => 7 * val,
                2 => 7 * val,
                3 => 5 * val,
                4 => 7 * val,
                5 => 6 * val,
                _ => panic!("WTF"),
            };
        })
        .sum();

    let max_space = area.height * area.width;

    return total_size_required <= max_space;
}

fn trivial_fit(area: &Area) -> bool {
    // TO be very presice, these should be divible by three
    let total_size = area.height * area.width;
    let blocks: u64 = area.areas.iter().sum();

    return total_size >= blocks * 9;
}

fn part1_solve(areas: &[Area]) {
    let theoretical: Vec<&Area> = areas
        .iter()
        .filter(|a| could_theoretically_fit(a))
        .collect();

    let trivial = theoretical.iter().filter(|f| trivial_fit(f));

    println!("{} {}", theoretical.iter().count(), trivial.count())
}

fn main() {
    let areas = read_areas("data.txt");

    part1_solve(&areas);
}
