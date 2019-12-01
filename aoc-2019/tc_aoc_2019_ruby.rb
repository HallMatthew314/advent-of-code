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
end

