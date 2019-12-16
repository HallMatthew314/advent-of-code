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
end

