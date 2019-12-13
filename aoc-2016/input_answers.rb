#!/usr/bin/env ruby
# encoding: utf-8

require_relative "aoc_2016_ruby.rb"

def day1(path)
  directions = File.read(path || "day1_input.txt").split(", ")
  puts "Part 1: #{AOC2016.day1_part1(directions)}"
  puts "Part 2: #{AOC2016.day1_part2(directions)}"
end

def day2(path)
  code = File.readlines(path || "day2_input.txt").map { |l| l.strip }
  puts "Part 1: #{AOC2016.day2_part1(code)}"
  puts "Part 2: #{AOC2016.day2_part2(code)}"
end

def day3(path)
  triangles = File.readlines(path || "day3_input.txt").map { |l| l.strip }
  puts "Part 1: #{AOC2016.day3_part1(triangles.dup)}"
  puts "Part 2: #{AOC2016.day3_part2(triangles.dup)}"
end

# Methods below this comment are copied from the 2019 file

def day4(path)
  range = File.read(path || "day4_input.txt").strip
  puts "Part 1: #{AOC2016.day4_part1(range)}"
  puts "Part 2: #{AOC2016.day4_part2(range)}"
end

case ARGV[0].to_i
when 1 then day1(ARGV[1])
when 2 then day2(ARGV[1])
when 3 then day3(ARGV[1])
#when 4 then day4(ARGV[1])
else puts "Please supply a day to run: ./input_answers.rb <DAY NUMBER> (INPUT PATH)"
end

