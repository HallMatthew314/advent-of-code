#!/usr/bin/env ruby
# encoding: utf-8

require "test/unit"
require_relative "aoc_2018_ruby.rb"

class TestAOC2018 < Test::Unit::TestCase

  def test_day1_part1
    assert_equal(3, AOC2018.day1_part1([1, -2, 3, 1]))
    assert_equal(3, AOC2018.day1_part1([1, 1, 1]))
    assert_equal(0, AOC2018.day1_part1([1, 1, -2]))
    assert_equal(-6, AOC2018.day1_part1([-1, -2, -3]))
  end

  def test_day1_part2
    assert_equal(2, AOC2018.day1_part2([1, -2, 3, 1]))
    assert_equal(0, AOC2018.day1_part2([1, -1]))
    assert_equal(10, AOC2018.day1_part2([3, 3, 4, -2, -4]))
    assert_equal(5, AOC2018.day1_part2([-6, 3, 8, 5, -6]))
    assert_equal(14, AOC2018.day1_part2([7, 7, -2, -7, 04]))
  end

  def test_day2_part1
    ids = [
      "abcdef",
      "bababc",
      "abbcde",
      "abcccd",
      "aabcdd",
      "abcdee",
      "ababab"
    ]

    assert_equal(12, AOC2018.day2_part1(ids))
  end

  def test_day2_part2
    ids = [
      "abcde",
      "fghij",
      "klmno",
      "pqrst",
      "fguij",
      "axcye",
      "wvxyz"
    ]

    assert_equal("fgij", AOC2018.day2_part2(ids.dup))
  end
end

