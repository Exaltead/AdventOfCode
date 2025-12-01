use std::fs;

#[derive(PartialEq)]
enum Direction {
    Left,
    Right,
}

struct Rotation {
    direction: Direction,
    amount: i64,
}

fn parse_rotation(command: &str) -> Rotation {
    let first = command.chars().nth(0).unwrap();
    let direction = if first == 'L' {
        Direction::Left
    } else {
        Direction::Right
    };

    let numbers = command
        .chars()
        .into_iter()
        .skip(1)
        .collect::<String>()
        .parse::<i64>()
        .unwrap();

    Rotation {
        direction,
        amount: numbers,
    }
}

fn read_puzzle_input(fname: &str) -> Vec<Rotation> {
    fs::read_to_string(fname)
        .unwrap()
        .lines()
        .map(parse_rotation)
        .collect()
}

fn rotate(current: i64, rotation: &Rotation) -> i64 {
    let theoretical = match rotation.direction {
        Direction::Left => current - rotation.amount,
        Direction::Right => current + rotation.amount,
    };

    theoretical.rem_euclid(100)
}

fn times_will_pass_zero(current: i64, rotation: &Rotation) -> i64 {
    let loops = rotation.amount / 100;

    let real_rotation = rotation.amount.rem_euclid(100);

    let reminder_will_loop = current + real_rotation >= 100
        && rotation.direction == Direction::Right
        || (current != 0 && current - real_rotation <= 0 && rotation.direction == Direction::Left);

    let total_loops = loops + if reminder_will_loop { 1 } else { 0 };

    total_loops
}

fn part2_solve(rotations: &[Rotation]) -> i64 {
    let mut current = 50;
    let mut zero_positions = 0;
    for rotation in rotations {
        zero_positions += times_will_pass_zero(current, &rotation);

        current = rotate(current, &rotation);
    }
    zero_positions
}

fn part1_solve(rotations: &[Rotation]) -> i64 {
    let mut current = 50;
    let mut zero_positions = 0;
    for rotation in rotations {
        current = rotate(current, &rotation);
        if current == 0 {
            zero_positions += 1;
        }
    }
    zero_positions
}

fn main() {
    let input = read_puzzle_input("data.txt");
    println!("Part1 {}", part1_solve(&input));
    println!("Part2 {}", part2_solve(&input));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rotate_right() {
        assert_eq!(
            rotate(
                40,
                &Rotation {
                    direction: Direction::Right,
                    amount: 47
                }
            ),
            87
        );

        assert_eq!(
            rotate(
                40,
                &Rotation {
                    direction: Direction::Right,
                    amount: 147
                }
            ),
            87
        )
    }

    #[test]
    fn rotate_left() {
        assert_eq!(
            rotate(
                40,
                &Rotation {
                    direction: Direction::Left,
                    amount: 37
                }
            ),
            3
        );

        assert_eq!(
            rotate(
                40,
                &Rotation {
                    direction: Direction::Left,
                    amount: 147
                }
            ),
            93
        )
    }
}
