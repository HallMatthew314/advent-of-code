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

def day3(path)
  wires = File.readlines(path || "day3_input.txt")
    .map { |l| l.strip.split(",") }

  puts "Part 1: #{AOC2019.day3_part1(wires)}"
  puts "Part 2: #{AOC2019.day3_part2(wires)}"
end

def day4(path)
  range = File.read(path || "day4_input.txt").strip
  puts "Part 1: #{AOC2019.day4_part1(range)}"
  puts "Part 2: #{AOC2019.day4_part2(range)}"
end

def day5(path)
  c = File.read(path || "day5_input.txt").strip.split(",").map { |o| o.to_i }
  puts "Part 1: #{AOC2019.day5_part1(c.dup, 1)}"
  puts "Part 2: #{AOC2019.day5_part2(c.dup, 5)}"
end

def day6(path)
  orbit_map = File.readlines(path || "day6_input.txt").map { |l| l.strip }
  puts "Part 1: #{AOC2019.day6_part1(orbit_map)}"
  puts "Part 2: #{AOC2019.day6_part2(orbit_map)}"
end

def day7(path)
  c = File.read(path || "day7_input.txt").strip.split(",").map { |o| o.to_i }
  puts "Part 1: #{AOC2019.day7_part1(c)}"
  puts "Part 2: #{AOC2019.day7_part2(c)}"
end

case ARGV[0].to_i
when 1 then day1(ARGV[1])
when 2 then day2(ARGV[1])
when 3 then day3(ARGV[1])
when 4 then day4(ARGV[1])
when 5 then day5(ARGV[1])
when 6 then day6(ARGV[1])
when 7 then day7(ARGV[1])
else puts "Please supply a day to run: ./input_answers.rb <DAY NUMBER> (INPUT PATH)"
end

