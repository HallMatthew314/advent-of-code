use std::env;
use std::fs;

struct D1DialTurn {
    turn_right: bool,
    full_rotations: u32,
    remaining_rotation: u32,
}

impl D1DialTurn {
    fn parse(inp: &str) -> Option<Self> {
        let (lr, num_s) = inp.split_at_checked(1)?;
        let num = u32::from_str_radix(num_s, 10).ok()?;

        let turn_right = match lr {
            "L" => false,
            "R" => true,
            _   => { return None; },
        };
        let full_rotations = num / 100;
        let remaining_rotation = num % 100;

        Some(Self { turn_right, full_rotations, remaining_rotation })
    }
}

struct D1Dial {
    pointing_at: u32,
}

impl D1Dial {
    fn new() -> Self {
        Self { pointing_at: 50 }
    }

    fn turn(&mut self, dt: &D1DialTurn) -> u32 {
        if dt.turn_right {
            self.pointing_at += dt.remaining_rotation;
            if self.pointing_at >= 100 {
                self.pointing_at %= 100;
                dt.full_rotations + 1
            } else {
                dt.full_rotations
            }
        } else {
            // turn left
            let mut zeroes = dt.full_rotations;

            if self.pointing_at == 0 {
                self.pointing_at = 100 - dt.remaining_rotation;
            } else {
                self.pointing_at += 100;
                self.pointing_at -= dt.remaining_rotation;
                if self.pointing_at <= 100 {
                    zeroes += 1;
                }
                self.pointing_at %= 100;
            }
            zeroes
        }
    }
}

fn day1part1(input: &str) -> String {
    let mut dial = D1Dial::new();
    let mut zeroes: u32 = 0;

    for s in input.split_whitespace() {
        let Some(dt) = D1DialTurn::parse(s) else {
            panic!("Invalid turn: {}", s);
        };
        dial.turn(&dt);
        if dial.pointing_at == 0 {
            zeroes += 1;
        }
    }

    format!("D01P1: {}", zeroes)
}

fn day1part2(input: &str) -> String {
    let mut dial = D1Dial::new();
    let mut zeroes: u32 = 0;

    for s in input.split_whitespace() {
        let Some(dt) = D1DialTurn::parse(s) else {
            panic!("Invalid turn: {}", s);
        };
        zeroes += dial.turn(&dt);

    }

    format!("D01P2: {}", zeroes)
}

fn get_whole_file(path: &str) -> String {
    let Ok(bytes) = fs::read(path) else {
        eprintln!("Unable to load file '{}'", path);
        eprintln!("This is probably a typo in the lookup table.");
        std::process::exit(1);
    };
    let Ok(s) = String::from_utf8(bytes) else {
        eprintln!("Loaded the file '{}', but it contains invalid UTF-8.", path);
        std::process::exit(1);
    };
    s
}

fn lookup_solution(name: String) -> Option<(fn(&str) -> String, &'static str)> {
    match name.as_str() {
        "D01P1" => Some((day1part1, "day01_input.txt")),
        "D01P2" => Some((day1part2, "day01_input.txt")),
        //"D02P1" => Some((day2part1, "day02_input.txt")),
        //"D02P2" => Some((day2part2, "day02_input.txt")),
        _       => None,
    }
}

fn main() {
    let Some((f, input_path)) = env::args().filter_map(lookup_solution).next() else {
        eprintln!("No valid solution name provided.");
        eprintln!("Please provide a name as an argument in the form DxxPy");
        std::process::exit(1);
    };
    let input = get_whole_file(input_path);
    println!("{:?}", f(&input));
}

#[cfg(test)]
mod tests {
    use super::*;

    const D1_TEST_INPUT: &'static str ="
L30
R48
L5
R60
L55
L1
L99
R14
L82
";

    #[test]
    fn test_d1p1() {
        let expected = 3;
        assert_eq!(expected, day1part1(D1_TEST_INPUT));
    }

    #[test]
    fn test_d1p2() {
        let expected = 6;
        assert_eq!(expected, day1part2(D1_TEST_INPUT));
    }
}

