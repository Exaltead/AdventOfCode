pub fn find_pair(fname: &str, target: i64) -> i64 {
    let mut values = parse_csv(fname);

    for i in 0..100 {
        for j in 0..100 {
            values[1] = i;
            values[2] = j;
            if first_code(values.clone()) == target {
                return i * 100 + j;
            }
        }
        println!("Run for i: {} complete", i)
    }
    0
}

pub fn execute(fname: &str) -> i64 {
    let values = parse_csv(fname);
    first_code(values)
}

fn parse_csv(fname: &str) -> Vec<i64> {
    let content = std::fs::read_to_string(fname).unwrap();
    content.split(',').map(|value| value.parse::<i64>().unwrap()).collect()
}

fn first_code(mut input: Vec<i64>) -> i64 {
    run_instruction_loop(&mut input);
    input[0]
}

fn run_instruction_loop(input: &mut Vec<i64>) {
    let mut index = 0;
    while index < input.len() {
        if !run_instruction(input, index) {
            return;
        }
        index += 4;
    }
}

fn run_instruction(input: &mut Vec<i64>, index: usize) -> bool {
    let operands = Vec::from(&input[index..index + 4]);
    match operands.as_slice() {
        [1, f, s, t] => invoke_instruction(input, *f, *s, *t, |a, b| a + b),
        [2, f, s, t] => invoke_instruction(input, *f, *s, *t, |a, b| a * b),
        _ => false,
    }
}

fn invoke_instruction<F>(inputs: &mut Vec<i64>, first: i64, second: i64, target: i64, operator: F) -> bool
where
    F: Fn(i64, i64) -> i64,
{
    let first_value = inputs[first as usize];
    let second_value = inputs[second as usize];
    inputs[target as usize] = operator(first_value, second_value);
    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example_input1() {
        assert_eq!(first_code(vec![1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]), 3500)
    }
}
