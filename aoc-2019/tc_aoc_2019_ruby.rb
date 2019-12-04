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

  # No examples to test for Day 4
end

