#!/usr/bin/env ruby
# encoding: utf-8

require "test/unit"
require_relative "aoc_2016_ruby.rb"

class TestAOC2016 < Test::Unit::TestCase

  def test_day1_part1
    assert_equal(5, AOC2016.day1_part1("R2, L3".split(", ")))
    assert_equal(2, AOC2016.day1_part1("R2, R2, R2".split(", ")))
    assert_equal(12, AOC2016.day1_part1("R5, L5, R5, R3".split(", ")))
  end

  def test_day1_part2
    assert_equal(4, AOC2016.day1_part2("R8, R4, R4, R8".split(", ")))
  end

  def test_day2_part1
    assert_equal("1985", AOC2016.day2_part1([
      "ULL",
      "RRDDD",
      "LURDL",
      "UUUUD"
    ]))
  end

  def test_day2_part2
    assert_equal("5DB3", AOC2016.day2_part2([
      "ULL",
      "RRDDD",
      "LURDL",
      "UUUUD"
    ]))
  end
end

