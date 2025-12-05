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
            _ => {
                return None;
            }
        };
        let full_rotations = num / 100;
        let remaining_rotation = num % 100;

        Some(Self {
            turn_right,
            full_rotations,
            remaining_rotation,
        })
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

fn d2p1invalid(input: u64) -> bool {
    let input_s = input.to_string();
    let half = input_s.len() / 2;
    &input_s[..half] == &input_s[half..]
}

fn d2p2invalid(input: u64) -> bool {
    let input_s = input.to_string();
    'outer: for substr_len in 1..=(input_s.len() / 2) {
        if input_s.len() % substr_len > 0 {
            continue;
        }

        let (prefix, mut rest) = input_s.split_at(substr_len);
        while !rest.is_empty() {
            if let Some(suffix) = rest.strip_prefix(prefix) {
                rest = suffix;
            } else {
                continue 'outer;
            }
        }

        return true;
    }

    false
}

fn day2generic(input: &str, pred: fn(u64) -> bool) -> u64 {
    let mut bad_sum: u64 = 0;
    for body in input.trim().split(',') {
        let Some((start, end)) = body.split_once('-') else {
            panic!("Invalid range: '{}'", body);
        };

        let Ok(start_n) = u64::from_str_radix(start, 10) else {
            panic!("Couldn't parse '{}' as u64", start);
        };
        let Ok(end_n) = u64::from_str_radix(end, 10) else {
            panic!("Couldn't parse '{}' as u64", end);
        };

        for n in start_n..=end_n {
            if pred(n) {
                bad_sum += n;
            }
        }
    }

    bad_sum
}

fn day2part1(input: &str) -> String {
    format!("D02P1: {}", day2generic(input, d2p1invalid))
}

fn day2part2(input: &str) -> String {
    format!("D02P2: {}", day2generic(input, d2p2invalid))
}

/*
fn d3p1solve_row(digs: &[char]) -> u64 {
    let mut i_tens: usize = 0;
    let mut i_ones: usize = 1;

    for i in 1..digs.len() {
        if digs[i] > digs[i_tens] && i < digs.len() - 1 {
            i_tens = i;
            i_ones = i + 1;
        } else if digs[i] > digs[i_ones] {
            i_ones = i;
        }
    }

    let tens = digs[i_tens].to_digit(10).unwrap() as u64;
    let ones = digs[i_ones].to_digit(10).unwrap() as u64;
    tens * 10 + ones
}*/

fn d3solve_row<const N: usize>(dig_str: &str) -> u64 {
    let digs: Vec<char> = dig_str.chars().collect();
    let mut best = [0; N];
    for i in 1..N {
        best[i] = i;
    }

    for d in 1..digs.len() {
        'inner: for b in 0..best.len() {
            if d <= digs.len() - best.len() + b && d > best[b] && digs[d] > digs[best[b]]
            {
                for i in b..best.len() {
                    best[b + (i - b)] = d + (i - b);
                }
                break 'inner;
            }
        }
    }

    let mut total: u64 = 0;
    for i in 0..best.len() {
        total *= 10;
        let n = digs[best[i]].to_digit(10).unwrap() as u64;
        total += n;
    }
    total
}

fn day3generic<const N: usize>(input: &str) -> u64 {
    input.lines().map(d3solve_row::<N>).sum()
}

fn day3part1(input: &str) -> String {
    format!("D03P1: {}", day3generic::<2>(input))
}

fn day3part2(input: &str) -> String {
    format!("D03P2: {}", day3generic::<12>(input))
}

fn d4parse_map(input: &str) -> Vec<Vec<u8>> {
    input.lines().map(|line| {
        line.chars().map(|c| match c {
            '.' => 0_u8,
            '@' => 1_u8,
            _   => panic!("Invalid map char: '{}'", c),
        }).collect()
    }).collect()
}

// also counts the given cell
fn d4is_accessable(r: usize, c: usize, map: &[Vec<u8>]) -> bool {
    let mut sum = 0;

    'row: for dr in -1_isize..=1_isize {
        let ri = match r.checked_add_signed(dr) {
            Some(x) => x,
            None    => { continue 'row; } // oob
        };

        'col: for dc in -1isize..=1isize {
            let ci = match c.checked_add_signed(dc) {
                Some(x) => x,
                None    => { continue 'col; } // oob
            };

            sum += map.get(ri)
                .and_then(|row| row.get(ci))
                .unwrap_or(&0);
        }
    }

    // leq because the center is also counted
    sum <= 4
}

fn day4part1(input: &str) -> String {
    let map = d4parse_map(input);

    let mut accessable: usize = 0;

    for r in 0..map.len() {
        for c in 0.. map[r].len() {
            if map[r][c] == 1 {
                if d4is_accessable(r, c, &map) {
                    // leq because the counting function also counts the center
                    accessable += 1;
                }
            }
        }
    }

    format!("D04P1: {}", accessable)
}

fn day4part2(input: &str) -> String {
    let mut map = d4parse_map(input);
    let mut accessable: usize = 0;
    let mut to_remove: Vec<(usize, usize)>;

    loop {
        to_remove = vec![];

        for r in 0..map.len() {
            for c in 0..map[r].len() {
                if map[r][c] == 1 {
                    if d4is_accessable(r, c, &map) {
                        to_remove.push((r, c));
                        accessable += 1;
                    }
                }
            }
        }

        if to_remove.is_empty() {
            break;
        }

        for (r, c) in to_remove.iter() {
            map[*r][*c] = 0;
        }
    }

    format!("D04P2: {}", accessable)
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
        "D02P1" => Some((day2part1, "day02_input.txt")),
        "D02P2" => Some((day2part2, "day02_input.txt")),
        "D03P1" => Some((day3part1, "day03_input.txt")),
        "D03P2" => Some((day3part2, "day03_input.txt")),
        "D04P1" => Some((day4part1, "day04_input.txt")),
        "D04P2" => Some((day4part2, "day04_input.txt")),
        _ => None,
    }
}

fn main() {
    match env::args().filter_map(lookup_solution).next() {
        None => {
            eprintln!("No valid solution name provided.");
            eprintln!("Please provide a name as an argument in the form DxxPy");
            std::process::exit(1);
        }
        Some((f, input_path)) => {
            let input = get_whole_file(input_path);
            println!("{:?}", f(&input));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const D1_TEST_INPUT: &'static str = "L68\nL30\nR48\nL5\nR60\nL55\nL1\nL99\nR14\nL82";

    const D2_TEST_INPUT: &'static str = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124";

    const D3_TEST_INPUT: &'static str =
        "987654321111111\n811111111111119\n234234234234278\n818181911112111";

    const D4_TEST_INPUT: &'static str = "..@@.@@@@.\n@@@.@.@.@@\n@@@@@.@.@@\n@.@@@@..@.\n@@.@@@@.@@\n.@@@@@@@.@\n.@.@.@.@@@\n@.@@@.@@@@\n.@@@@@@@@.\n@.@.@@@.@.";

    #[test]
    fn test_d1p1() {
        assert_eq!("D01P1: 3".to_owned(), day1part1(D1_TEST_INPUT));
    }

    #[test]
    fn test_d1p2() {
        assert_eq!("D01P2: 6".to_owned(), day1part2(D1_TEST_INPUT));
    }

    #[test]
    fn test_d2p1() {
        assert_eq!("D02P1: 1227775554".to_owned(), day2part1(D2_TEST_INPUT));
    }

    #[test]
    fn test_d2p2() {
        assert_eq!("D02P2: 4174379265".to_owned(), day2part2(D2_TEST_INPUT));
    }

    #[test]
    fn test_d3p1() {
        assert_eq!("D03P1: 357".to_owned(), day3part1(D3_TEST_INPUT));
    }

    #[test]
    fn test_d3p2() {
        assert_eq!("D03P2: 3121910778619".to_owned(), day3part2(D3_TEST_INPUT));
    }

    #[test]
    fn test_d4p1() {
        assert_eq!("D04P1: 13".to_owned(), day4part1(D4_TEST_INPUT));
    }

    #[test]
    fn test_d4p2() {
        assert_eq!("D04P2: 43".to_owned(), day4part2(D4_TEST_INPUT));
    }
}
