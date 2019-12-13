#!/usr/bin/env ruby
# encoding: utf-8

require "test/unit"
require_relative "aoc_2019_ruby.rb"

class TestAOC2019 < Test::Unit::TestCase

  def test_day1_part1
    assert_equal(2, AOC2019.day1_part1([12]))
    assert_equal(2, AOC2019.day1_part1([14]))
    assert_equal(654, AOC2019.day1_part1([1_969]))
    assert_equal(33_583, AOC2019.day1_part1([100_756]))
  end

  def test_day1_part2
    assert_equal(2, AOC2019.day1_part2([14]))
    assert_equal(966, AOC2019.day1_part2([1_969]))
    assert_equal(50_346, AOC2019.day1_part2([100_756]))
  end

  # No examples to test for Day 2

  def test_day3_part1
    assert_equal(6, AOC2019.day3_part1([
      "R8,U5,L5,D3".split(","),
      "U7,R6,D4,L4".split(",")
    ]))
    assert_equal(159, AOC2019.day3_part1([
      "R75,D30,R83,U83,L12,D49,R71,U7,L72".split(","),
      "U62,R66,U55,R34,D71,R55,D58,R83".split(",")
    ]))
    assert_equal(135, AOC2019.day3_part1([
      "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51".split(","),
      "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7".split(",")
    ]))
  end

  def test_day3_part2
    assert_equal(30, AOC2019.day3_part2([
      "R8,U5,L5,D3".split(","),
      "U7,R6,D4,L4".split(",")
    ]))
    assert_equal(610, AOC2019.day3_part2([
      "R75,D30,R83,U83,L12,D49,R71,U7,L72".split(","),
      "U62,R66,U55,R34,D71,R55,D58,R83".split(",")
    ]))
    assert_equal(410, AOC2019.day3_part2([
      "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51".split(","),
      "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7".split(",")
    ]))
  end

  # No examples to test for Day 4 or 5

  def test_day6_part1
    assert_equal(42, AOC2019.day6_part1([
      "COM)B",
      "B)C",
      "C)D",
      "D)E",
      "E)F",
      "B)G",
      "G)H",
      "D)I",
      "E)J",
      "J)K",
      "K)L"
    ]))
  end

  def test_day6_part2
    assert_equal(4, AOC2019.day6_part2([
      "COM)B",
      "B)C",
      "C)D",
      "D)E",
      "E)F",
      "B)G",
      "G)H",
      "D)I",
      "E)J",
      "J)K",
      "K)L",
      "K)YOU",
      "I)SAN"
    ]))
  end

  def test_day7_part1
    assert_equal(43210, AOC2019.day7_part1(
      [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
    ))
    assert_equal(54321, AOC2019.day7_part1(
      [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]
    ))
    assert_equal(65210, AOC2019.day7_part1(
      [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]
    ))
  end

  def test_day7_part2
    assert_equal(139629729, AOC2019.day7_part2(
      [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
    ))
    assert_equal(18216, AOC2019.day7_part2(
      [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]
    ))
  end

  def test_day8_part1
    assert_equal(1, AOC2019.day8_part1("123456789012", 3, 2))
  end

  def test_day8_part2
    assert_equal(" #\n# ", 
      AOC2019.day8_part2("0222112222120000", 2, 2)
    )
  end

  def test_day10
    grid = ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##".split(/\n/)

    assert_equal([[11, 13], 210], AOC2019.day10_part1(grid))
    assert_equal(802, AOC2019.day10_part2(grid))
  end

  def test_day12_part1
    data = [
      "<x=-8, y=-10, z=0>",
      "<x=5, y=5, z=10>",
      "<x=2, y=-7, z=3>",
      "<x=9, y=-8, z=-3>"
    ]

    assert_equal(1940, AOC2019.day12_part1(data, 100))
  end

  def test_day12_part2
    data = [
      "<x=-1, y=0, z=2>",
      "<x=2, y=-10, z=-7>",
      "<x=4, y=-8, z=8>",
      "<x=3, y=5, z=-1>"
    ]

    assert_equal(2772, AOC2019.day12_part2(data))
  end
end

