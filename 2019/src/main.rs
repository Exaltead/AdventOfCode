#![allow(dead_code)]

mod day_1;
mod day_2;
mod utils;

fn main() {
    //let (payload_fuel, total_fuel) = day_1::get_fuel_mass_compensated("res/day1.txt");
    //println!("Day 1-1: Payload fuel: {}, Total fuel: {}", payload_fuel, total_fuel);

    //let code = day_2::execute("res/day2-1.txt");
    //println!("Day 2-1: Code {}", code);

    let code = day_2::find_pair("res/day2-1.txt", 19690720);
    println!("Day 2-1: Code {}", code);
}
