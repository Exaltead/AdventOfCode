use regex::Regex;
use std::fs;

#[derive(Clone, Debug)]
struct Machine {
    target: String,
    current: Vec<char>,
    buttons: Vec<Vec<usize>>,
    voltages: Vec<i64>,
}

impl Machine {
    fn press(&self, button: usize) -> Machine {
        let mut copy = self.clone();
        for val in &self.buttons[button] {
            if copy.current[*val] == '#' {
                copy.current[*val] = '.'
            } else {
                copy.current[*val] = '#'
            }
        }

        copy
    }

    fn is_solved_buttons(&self) -> bool {
        let stringed: String = self.current.iter().collect();
        return self.target == stringed;
    }

    fn press_voltages(&self, current_voltage: &[i64], button: usize) -> Vec<i64> {
        let mut vec = current_voltage.to_vec();
        for i in &self.buttons[button] {
            vec[*i] += 1
        }

        vec
    }
}

fn read_input(fname: &str) -> Vec<Machine> {
    let regex = Regex::new(r"\[(?<lights>.*)\]\s(?<buttons>.*)\s\{(?<voltages>.*)}").unwrap();
    fs::read_to_string(fname)
        .unwrap()
        .lines()
        .map(|f| {
            let captures = regex.captures(f).unwrap();
            let heading = captures["lights"].to_string();
            let buttons: Vec<Vec<usize>> = captures["buttons"]
                .split(' ')
                .map(|button| {
                    button
                        .trim_start_matches("(")
                        .trim_end_matches(")")
                        .split(',')
                        .map(|f| f.parse().unwrap())
                        .collect()
                })
                .collect();

            let voltages = captures["voltages"]
                .split(',')
                .map(|f| f.parse::<i64>().unwrap())
                .map(|f| 0 - f)
                .collect();

            Machine {
                current: vec!['.'; heading.len()],
                target: heading,
                buttons: buttons,
                voltages,
            }
        })
        .collect()
}

fn solve_machine(machine: &Machine) -> u64 {
    let mut counter = 0;
    let mut checkable = vec![machine.clone()];

    loop {
        let mut next = vec![];

        for mac in checkable {
            if mac.is_solved_buttons() {
                return counter;
            }

            for button in 0..mac.buttons.len() {
                next.push(mac.press(button));
            }
        }
        counter += 1;
        checkable = next;
    }
}

fn part1_solve(machines: &[Machine]) -> u64 {
    machines.iter().map(|f| solve_machine(f)).sum()
}

fn solve_voltage(machine: &Machine) -> u64 {
    let mut counter = 0;
    let mut checkable = vec![machine.voltages.clone()];

    loop {
        let mut next = vec![];
        if checkable.is_empty() {
            panic!("WTF");
        }

        for voltage_set in checkable {
            if voltage_set.iter().all(|f| *f == 0) {
                return counter;
            }

            for button in 0..machine.buttons.len() {
                let possible = machine.press_voltages(&voltage_set, button);
                if possible.iter().any(|f| *f > 0) {
                    // As negative buttons can not be pressed, this is now a dead end
                    continue;
                }
                next.push(possible);
            }
        }
        counter += 1;
        checkable = next;
    }
}

fn solve_voltage_v2(machine: &Machine, voltages: &[i64], counter: u64) -> Option<u64> {
    let mut options = vec![];
    for button in 0..machine.buttons.len() {
        let next = machine.press_voltages(voltages, button);
        if next.iter().any(|f| *f > 0) {
            continue;
        }

        if next.iter().all(|f| *f == 0) {
            return Some(counter + 1);
        }

        if let Some(val) = solve_voltage_v2(machine, &next, counter + 1) {
            options.push(val);
        }
    }
    return options.iter().copied().min();
}

fn part2_solve(machines: &[Machine]) -> u64 {
    //machines.iter().map(|f| solve_voltage(f)).sum()
    machines
        .iter()
        .map(|f| solve_voltage_v2(f, &f.voltages, 0).unwrap())
        .sum()
}

fn main() {
    let machines = read_input("test_data.txt");

    //println!("{:?}", machines[0]);
    //println!("{:?}", machines[0].press(1));
    // Commented out due to taking quite a bit of time to compute
    //println!("Part 1: {}", part1_solve(&machines));
    println!("Part 2: {}", part2_solve(&machines));
}
