use std::fs;

enum D1Instruction {
    Left(u32),
    Right(u32),
}

fn d1_parse_instruction(inp: &str) -> Option<D1Instruction> {
    let (lr, num_s) = inp.split_at_checked(1)?;
    let num = u32::from_str_radix(num_s, 10).ok()?;
    match lr {
        "L" => Some(D1Instruction::Left(num)),
        "R" => Some(D1Instruction::Right(num)),
        _   => None,
    }
}

fn day1part1(input: &str) -> u32 {
    let mut dial: u32 = 50;
    let mut zeroes: u32 = 0;

    for s in input.split_whitespace() {
        match d1_parse_instruction(s) {
            None => panic!("Invalid instruction: {:?}", s),
            Some(D1Instruction::Left(n)) => {
                let n = n % 100;
                if n > dial {
                    dial += 100;
                }
                dial -= n;
            }
            Some(D1Instruction::Right(n)) => {
                let n = n % 100;
                dial = (dial + n) % 100;
            }
        }
        if dial == 0 {
            zeroes += 1;
        }
    }

    zeroes
}

fn day1part2(input: &str) -> u32 {
    let mut dial: u32 = 50;
    let mut clicks: u32 = 0;

    for s in input.split_whitespace() {
        println!("dial:{}", dial);
        match d1_parse_instruction(s) {
            None => panic!("Invalid instruction"),
            // much dumber solution for now
            Some(D1Instruction::Left(mut n)) => {
                while n >= 100 {
                    println!("click");
                    clicks += 1;
                    n -= 100;
                }

                if n > 0 {
                    if n >= dial {
                        if dial != 0 {
                            println!("click");
                            clicks += 1;
                        }
                        dial += 100;
                    }
                    dial = (dial - n) % 100;
                }
            }
            Some(D1Instruction::Right(mut n)) => {
                while n >= 100 {
                    println!("click");
                    clicks += 1;
                    n -= 100;
                }

                if n > 0 {
                    dial += n;
                    if dial >= 100 {
                    println!("click");
                        clicks += 1;
                        dial -= 100;
                    }
                }
            }
        }
    }

    println!("dial:{}", dial);
    clicks
}

fn get_whole_file(path: &str) -> String {
    let bytes = fs::read(path).unwrap();
    String::from_utf8(bytes).unwrap()
}

fn main() {
    println!("Running D01P2...");
    let input = get_whole_file("day01_input.txt");
    println!("{:?}", day1part2(&input));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_d1p1() {
        let test_inp = "L68
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
        let expected = 3;

        assert_eq!(expected, day1part1(test_inp));
    }

    #[test]
    fn test_d1p2() {
         let test_inp = "L68
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
        let expected = 6;

        assert_eq!(expected, day1part2(test_inp));
    }
}

