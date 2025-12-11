use std::{
    collections::{HashMap},
    fs,
};

fn read_input(fname: &str) -> HashMap<String, Vec<String>> {
    let binding = fs::read_to_string(fname).unwrap();
    let items = binding.lines().map(|f| {
        let (f, s) = f.split_once(':').unwrap();

        let parts: Vec<String> = s
            .split(' ')
            .filter(|f| !f.is_empty())
            .map(|f| f.to_string())
            .collect();

        (f.to_string(), parts)
    });

    HashMap::from_iter(items)
}

fn count_paths(current_node: &str, nodes: &HashMap<String, Vec<String>>) -> u64 {
    let mut acc = 0;
    for node in &nodes[current_node] {
        if node == "out" {
            return 1;
        }

        acc += count_paths(&node, nodes)
    }

    return acc;
}

fn count_paths_part2(
    current_node: &str,
    nodes: &HashMap<String, Vec<String>>,
    (v_dac, v_fft): (bool, bool),
    cache: &mut HashMap<(String, bool, bool), u64>,
) -> u64 {
    let mut v_dac = v_dac;
    let mut v_fft = v_fft;

    if current_node == "dac" {
        v_dac = true
    } else if current_node == "fft" {
        v_fft = true
    }
    if let Some(res) = cache.get(&(current_node.to_string(), v_dac, v_fft)) {
        return *res;
    }

    let mut acc = 0;
    for node in &nodes[current_node] {
        if node == "out" {
            if v_dac && v_fft {
                return 1;
            }
            return 0;
        }

        let res = count_paths_part2(&node, nodes, (v_dac, v_fft), cache);
        cache.insert((node.to_string(), v_dac, v_fft), res);
        acc += res;
    }

    return acc;
}

fn part1_solve(nodes: &HashMap<String, Vec<String>>) {
    println!("Part 1: {}", count_paths("you", nodes))
}

fn part2_solve(nodes: &HashMap<String, Vec<String>>) {
    println!(
        "Part 2: {}",
        count_paths_part2("svr", nodes, (false, false), &mut HashMap::new())
    )
}

fn main() {
    let nodes = read_input("data.txt");
    part1_solve(&nodes);
    let nodes = read_input("data.txt");
    part2_solve(&nodes);
}
