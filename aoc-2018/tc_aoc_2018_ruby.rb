#!/usr/bin/env ruby
# encoding: utf-8

require "test/unit"
require_relative "aoc_2018_ruby.rb"

class TestAOC2018 < Test::Unit::TestCase

  def test_day1
    assert_equal(0, AOC2018.day1("(())"))
    assert_equal(0, AOC2018.day1("()()"))
  end
end

