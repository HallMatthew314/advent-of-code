#!/usr/bin/env ruby
# encoding: utf-8

require_relative "aoc_2019_ruby.rb"

def day1(path)
  masses = File.readlines(path || "day1_input.txt").map { |l| l.strip }
  puts "Part 1: #{AOC2019.day1_part1(masses)}"
  puts "Part 2: #{AOC2019.day1_part2(masses)}"
end

def day2(path)
  c = File.read(path || "day2_input.txt").strip.split(",").map { |o| o.to_i }
  puts "Part 1: #{AOC2019.day2_part1(c.dup, 12, 2)}"
  puts "Part 2: #{AOC2019.day2_part2(c.dup, 19690720)}"
end

case ARGV[0].to_i
when 1 then day1(ARGV[1])
when 2 then day2(ARGV[1])
#when 3 then day3(ARGV[1])
else puts "Please supply a day to run: ./input_answers.rb <DAY NUMBER> (INPUT PATH)"
end

