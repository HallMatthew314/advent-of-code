#!/usr/bin/env ruby
# encoding: utf-8

require_relative "aoc_2015_ruby.rb"

def day1(path)
  directions = File.read(path || "day1_input.txt").strip
  puts "Part 1: #{AOC2015.day1_part1(directions)}"
  puts "Part 2: #{AOC2015.day1_part2(directions)}"
end

def day2(path)
  dimensions = File.readlines(path || "day2_input.txt").map { |l| l.strip }
  puts "Part 1: #{AOC2015.day2_part1(dimensions)}"
  puts "Part 2: #{AOC2015.day2_part2(dimensions)}"
end

def day3(path)
  directions = File.read(path || "day3_input.txt").strip
  puts "Part 1: #{AOC2015.day3_part1(directions)}"
  puts "Part 2: #{AOC2015.day3_part2(directions)}"
end

def day4(path)
  key = File.read(path || "day4_input.txt").strip
  puts "Part 1: #{AOC2015.day4_part1(key)}"
  puts "Part 2: #{AOC2015.day4_part2(key)}"
end

def day5(path)
  strings = File.readlines(path || "day5_input.txt").map { |l| l.strip }
  puts "Part 1: #{AOC2015.day5_part1(strings)}"
  puts "Part 2: #{AOC2015.day5_part2(strings)}"
end

def day6(path)
  instructions = File.readlines(path || "day6_input.txt").map { |l| l.strip }
  puts "Part 1: #{AOC2015.day6_part1(instructions)}"
  puts "Part 2: #{AOC2015.day6_part2(instructions)}"
end

def day7(path)
  circuit = File.readlines(path || "day7_input.txt").map { |l| l.strip }
  puts "Part 1: #{AOC2015.day7_part1(circuit)}"
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

