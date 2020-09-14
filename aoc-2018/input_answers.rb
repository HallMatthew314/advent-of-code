#!/usr/bin/env ruby
# encoding: utf-8

require_relative "aoc_2018_ruby.rb"

def day1(path)
  deltas = File.readlines(path || "day1_input.txt").map(&:to_i)
  puts "Part 1: #{AOC2018.day1_part1(deltas.dup)}"
  puts "Part 2: #{AOC2018.day1_part2(deltas.dup)}"
end

def day2(path)
  ids = File.readlines(path || "day2_input.txt").map(&:strip)
  puts "Part 1: #{AOC2018.day2_part1(ids)}"
  puts "Part 2: #{AOC2018.day2_part2(ids)}"
end

def day3(path)
  claims = File.readlines(path || "day3_input.txt").map(&:strip)
  puts "Part 1: #{AOC2018.day3_part1(claims)}"
  puts "Part 2: #{AOC2018.day3_part2(claims)}"
end

case ARGV[0].to_i
when 1 then day1(ARGV[1])
when 2 then day2(ARGV[1])
when 3 then day3(ARGV[1])
else puts "Please supply a day to run: ./input_answers.rb <DAY NUMBER> (INPUT PATH)"
end

