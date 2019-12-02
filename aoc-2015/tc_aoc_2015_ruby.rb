#!/usr/bin/env ruby
# encoding: utf-8

require "test/unit"
require_relative "aoc_2015_ruby.rb"

class TestAOC2015 < Test::Unit::TestCase

  def test_day1_part1
    assert_equal(0, AOC2015.day1_part1("(())"))
    assert_equal(0, AOC2015.day1_part1("()()"))

    assert_equal(3, AOC2015.day1_part1("((("))
    assert_equal(3, AOC2015.day1_part1("(()(()("))
    assert_equal(3, AOC2015.day1_part1("))((((("))

    assert_equal(-1, AOC2015.day1_part1("())"))
    assert_equal(-1, AOC2015.day1_part1("))("))

    assert_equal(-3, AOC2015.day1_part1(")))"))
    assert_equal(-3, AOC2015.day1_part1(")())())"))
  end

  def test_day1_part2
    assert_equal(1, AOC2015.day1_part2(")"))
    assert_equal(5, AOC2015.day1_part2("()())"))
  end

  def test_day2_part1
    assert_equal(58, AOC2015.day2_part1(["2x3x4"]))
    assert_equal(43, AOC2015.day2_part1(["1x1x10"]))

    assert_equal(101, AOC2015.day2_part1(["2x3x4", "1x1x10"]))
  end

  def test_day2_part2
    assert_equal(34, AOC2015.day2_part2(["2x3x4"]))
    assert_equal(14, AOC2015.day2_part2(["1x1x10"]))

    assert_equal(48, AOC2015.day2_part2(["2x3x4", "1x1x10"]))
  end

  def test_day3_part1
    assert_equal(2, AOC2015.day3_part1(">"))
    assert_equal(4, AOC2015.day3_part1("^>v<"))
    assert_equal(2, AOC2015.day3_part1("^v^v^v^v^v"))
  end

  def test_day3_part2
    assert_equal(3, AOC2015.day3_part2("^v"))
    assert_equal(3, AOC2015.day3_part2("^>v<"))
    assert_equal(11, AOC2015.day3_part2("^v^v^v^v^v"))
  end

  def test_day4_part1
    #assert_equal(609_043, AOC2015.day4_part1("abcdef"))
    #assert_equal(1_048_970, AOC2015.day4_part1("pqrstuv"))
  end

  # No examples available to test Day 4 Part 2

  def test_day5_part1
    assert_equal(1, AOC2015.day5_part1(["ugknbfddgicrmopn"]))
    assert_equal(1, AOC2015.day5_part1(["aaa"]))
    assert_equal(0, AOC2015.day5_part1(["jchzalrnumimnmhp"]))
    assert_equal(0, AOC2015.day5_part1(["haegwjzuvuyypxyu"]))
    assert_equal(0, AOC2015.day5_part1(["dvszwmarrgswjxmb"]))
  end

  def test_day5_part2
    assert_equal(1, AOC2015.day5_part2(["qjhvhtzxzqqjkmpb"]))
    assert_equal(1, AOC2015.day5_part2(["xxyxx"]))
    assert_equal(0, AOC2015.day5_part2(["uurcxstgmygtbstg"]))
    assert_equal(0, AOC2015.day5_part2(["ieodomkazucvgmuy"]))
  end

  def test_day6_part1
    assert_equal(1_000_000, AOC2015.day6_part1(["turn on 0,0 through 999,999"]))
    assert_equal(999_000, AOC2015.day6_part1([
      "turn on 0,0 through 999,999",
      "toggle 0,0 through 999,0"
    ]))
     assert_equal(998_996, AOC2015.day6_part1([
      "turn on 0,0 through 999,999",
      "toggle 0,0 through 999,0",
      "turn off 499,499 through 500,500"
    ]))
  end

  def test_day6_part2
    assert_equal(1_000_000, AOC2015.day6_part2(["turn on 0,0 through 999,999"]))
    assert_equal(1_002_000, AOC2015.day6_part2([
      "turn on 0,0 through 999,999",
      "toggle 0,0 through 999,0"
    ]))
     assert_equal(1_001_996, AOC2015.day6_part2([
      "turn on 0,0 through 999,999",
      "toggle 0,0 through 999,0",
      "turn off 499,499 through 500,500"
    ]))
  end

  # No examples available to test Day 7
end

