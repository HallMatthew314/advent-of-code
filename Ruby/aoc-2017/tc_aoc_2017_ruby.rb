#!/usr/bin/env ruby
# encoding: utf-8

require "test/unit"

require_relative "aoc_2017_ruby.rb"

class TestAOC2017 < Test::Unit::TestCase

  def test_day1_part1
    assert_equal(3, AOC2017.day1_part1("1122"))
    assert_equal(4, AOC2017.day1_part1("1111"))
    assert_equal(0, AOC2017.day1_part1("1234"))
    assert_equal(9, AOC2017.day1_part1("91212129"))
  end

  def test_day1_part2
    assert_equal(6, AOC2017.day1_part2("1212"))
    assert_equal(0, AOC2017.day1_part2("1221"))
    assert_equal(4, AOC2017.day1_part2("123425"))
    assert_equal(12, AOC2017.day1_part2("123123"))
    assert_equal(4, AOC2017.day1_part2("12131415"))
  end

  def test_day3_part1
    assert_equal(0, AOC2017.day3_part1(1))
    assert_equal(3, AOC2017.day3_part1(12))
    assert_equal(2, AOC2017.day3_part1(23))
    assert_equal(31, AOC2017.day3_part1(1024))
  end

  def test_day4_part1
    passes = [
      "aa bb cc dd ee",
      "aa bb cc dd aa",
      "aa bb cc dd aaa"
    ]

    assert_equal(2, AOC2017.day4_part1(passes))
  end

  def test_day4_part2
    passes = [
      "abcde fghij",
      "abcde xyz ecdab",
      "a ab abc abd abf abj",
      "iiii oiii ooii oooi oooo",
      "oiii ioii iioi iiio"
    ]

    assert_equal(3, AOC2017.day4_part2(passes))
  end

  def test_day5
    jumps = [0, 3, 0, 1, -3]

    assert_equal(5, AOC2017.day5_part1(jumps.dup))
    assert_equal(10, AOC2017.day5_part2(jumps.dup))
  end

  def test_day6
    blocks = [0, 2, 7, 0]

    assert_equal(5, AOC2017.day6_part1(blocks.dup))
    assert_equal(4, AOC2017.day6_part2(blocks.dup))
  end

  def test_day7
    nodes = [
      "pbga (66)",
      "xhth (57)",
      "ebii (61)",
      "havc (66)",
      "ktlj (57)",
      "fwft (72) -> ktlj, cntj, xhth",
      "qoyq (66)",
      "padx (45) -> pbga, havc, qoyq",
      "tknk (41) -> ugml, padx, fwft",
      "jptl (61)",
      "ugml (68) -> gyxo, ebii, jptl",
      "gyxo (61)",
      "cntj (57)"
    ]

    assert_equal("tknk", AOC2017.day7_part1(nodes.dup))
  end
end

