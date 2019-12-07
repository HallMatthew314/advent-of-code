#!/usr/bin/env ruby
# encoding: utf-8

require "test/unit"
require_relative "intcode_computer5.rb"

class TestIntcodeComputer5 < Test::Unit::TestCase

  def test_day5_part2
    #code = File.read("day5_input.txt").strip.split(",").map { |o| o.to_i }
    code = [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8]
    com = IntcodeComputer5.new(code)
    assert_equal(1, com.run([8]).first)
  end
end

