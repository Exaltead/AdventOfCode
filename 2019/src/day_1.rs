use std::fs::File;
use std::io::{BufRead, BufReader};
use std::iter::successors;

pub fn get_fuel_mass_compensated(filename: &str) -> (i64, i64) {
    read_integers(filename)
        .map(fuel_mass_of_object)
        .map(|fuel| (fuel, total_fuel(fuel)))
        .fold((0, 0), |(acc_fuel, acc_total), (fuel, total)| (acc_fuel + fuel, acc_total + total))
}

fn read_integers(filename: &str) -> impl Iterator<Item = i64> {
    BufReader::new(File::open(filename).unwrap())
        .lines()
        .map(|line| line.unwrap().parse::<i64>().unwrap())
}

fn total_fuel(fuel_mass: i64) -> i64 {
    successors(Some(fuel_mass), |fuel_mass| Some(fuel_mass_of_object(*fuel_mass)))
        .take_while(|mass| mass > &0)
        .sum()
}

fn fuel_mass_of_object(mass: i64) -> i64 {
    return mass / 3 - 2;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example_input1() {
        assert_eq!(fuel_mass_of_object(1969), 654)
    }

    #[test]
    fn example_input2() {
        assert_eq!(fuel_mass_of_object(100756), 33583)
    }

    #[test]
    fn example2_input1() {
        assert_eq!(total_fuel(1969), 1969 + 966)
    }

    #[test]
    fn example2_input2() {
        assert_eq!(total_fuel(100756), 100756 + 50346)
    }
}
