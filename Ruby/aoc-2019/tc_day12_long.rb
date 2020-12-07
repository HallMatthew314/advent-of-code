#!/usr/bin/env ruby
# encoding: utf-8

require "test/unit"
require_relative "aoc_2019_ruby.rb"

class TestDay12Long < Test::Unit::TestCase

  def test_day12
    #s = [
    #  "<x=-8, y=-10, z=0>",
    #  "<x=5, y=5, z=10>",
    #  "<x=2, y=-7, z=3>",
    #  "<x=9, y=-8, z=-3>"
    #]
    #assert_equal(4686774924, AOC2019.day12_part2(s))
    data = File.readlines("day12_input.txt").map { |l| l.strip }
    assert_equal(551272644867044, AOC2019.day12_part2(data))
  end
end

