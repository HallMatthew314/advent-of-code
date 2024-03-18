use std::cmp;
use std::fs;

fn load_day1() -> String {
    fs::read_to_string("day01input.txt").unwrap()
}

fn day1part1() {
    let input = load_day1();
    let answer = input.chars().fold(0, |acc, c| match c {
        '(' => acc + 1,
        ')' => acc - 1,
        _   => acc,
    });
    println!("{}", answer);
}

fn day1part2() {
    let input = load_day1();
    let mut acc = 0i32;
    let mut index = 0usize;
    let mut cs = input.chars();
    while let Some(c) = cs.next() {
        match c {
            '(' => { acc += 1; },
            ')' => { acc -= 1; },
            _   => {},
        }
        index += 1; // first character is instruction 1, not instruction 0
        if acc < 0 {
            println!("{}", index);
            return;
        }
    }
    panic!("invalid input for day1part2");
}

fn load_day2() -> Vec<(u32, u32, u32)> {
    let text = fs::read_to_string("day02input.txt").unwrap();
    text.lines()
        .map(|s| {
            let dims = s.split('x')
                .map(|d| d.parse::<u32>().unwrap())
                .collect::<Vec<u32>>();
            match dims[..] {
                [x, y, z] => (x, y, z),
                _ => panic!("not xpectly three dimensions"),
            }
        })
        .collect()
}

fn day2part1() {
    let data = load_day2();
    let total: u32 = data.iter()
        .map(|(x, y, z)| {
            let a = x*y;
            let b = x*z;
            let c = y*z;
            let slack = cmp::min(cmp::min(a, b), c);
            2 * (a + b + c) + slack
        }).sum();
    println!("{:?}", total);
}

fn day2part2() {
    let data = load_day2();
    let total: u32 = data.iter()
        .map(|(x, y, z)| {
            let extra = x * y * z;
            let a = 2 * (x + y);
            let b = 2 * (x + z);
            let c = 2 * (y + z);
            extra + cmp::min(cmp::min(a, b), c)
        })
        .sum();
    println!("{:?}", total);
}

fn main() {
    day2part2();
}

