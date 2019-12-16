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

  def test_day3
    assert_equal(0, AOC2016.day3_part1(["5 10 25"]))
    assert_equal(6, AOC2016.day3_part2([
      "101 301 501",
      "102 302 502",
      "103 303 503",
      "201 401 601",
      "202 402 602",
      "203 403 603"
    ]))
  end

  def test_day4
    rooms = [
      "aaaaa-bbb-z-y-x-123[abxyz]",
      "a-b-c-d-e-f-g-h-987[abcde]",
      "not-a-real-room-404[oarel]",
      "totally-real-room-200[decoy]"
    ]

    assert_equal(1514, AOC2016.day4_part1(rooms))
  end

  # Day 5 takes too long to have tests.

  def test_day6
    lines = [
      "eedadn",
      "drvtee",
      "eandsr",
      "raavrd",
      "atevrs",
      "tsrnev",
      "sdttsa",
      "rasrtv",
      "nssdts",
      "ntnada",
      "svetve",
      "tesnvt",
      "vntsnd",
      "vrdear",
      "dvrsen",
      "enarar"
    ]

    assert_equal("easter", AOC2016.day6_part1(lines))
    assert_equal("advent", AOC2016.day6_part2(lines))
  end

  def test_day7_part1
    addrs = [
      "abba[mnop]qrst",
      "abcd[bddb]xyyx",
      "aaaa[qwer]tyui",
      "ioxxoj[asdfgh]zxcvbn"
    ]

    assert_equal(2, AOC2016.day7_part1(addrs))
  end

  def test_day7_part2
    addrs = [
      "aba[bab]xyz", # yes
      "xyx[xyx]xyx", # no
      "aaa[kek]eke", # yes
      "zazbz[bzb]cdb" # yes
    ]

    assert_equal(3, AOC2016.day7_part2(addrs))
  end
end

