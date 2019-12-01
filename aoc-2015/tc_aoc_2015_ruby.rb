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
end

