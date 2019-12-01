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

def day3
  directions = File.read(path || "day3_input.txt").strip
  puts "Part 1: #{AOC2015.day3_part1(directions)}"
  puts "Part 2: #{AOC2015.day3_part2(directions)}"
end

case ARGV[0].to_i
when 1 then day1(ARGV[1])
when 2 then day2(ARGV[1])
when 3 then day3(ARGV[1])
else puts "Please supply a day to run: ./input_answers.rb <DAY NUMBER> (INPUT PATH)"
end
