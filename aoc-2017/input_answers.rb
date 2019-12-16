#!/usr/bin/env ruby
# encoding: utf-8

require_relative "aoc_2017_ruby.rb"

def day1(path)
  captcha = File.read(path || "day1_input.txt").strip
  puts "Part 1: #{AOC2017.day1_part1(captcha)}"
  puts "Part 2: #{AOC2017.day1_part2(captcha)}"
end

def day2(path)
  rows = File.readlines(path || "day2_input.txt").map { |l| l.strip }
  puts "Part 1: #{AOC2017.day2_part1(rows)}"
  puts "Part 2: #{AOC2017.day2_part2(rows)}"
end

def day3(path)
  square = File.read(path || "day3_input.txt").to_i
  puts "Part 1: #{AOC2017.day3_part1(square)}"
  puts "Part 2: #{AOC2017.day3_part2(square)}"
end

case ARGV[0].to_i
when 1 then day1(ARGV[1])
when 2 then day2(ARGV[1])
when 3 then day3(ARGV[1])
else puts "Please supply a day to run: ./input_answers.rb <DAY NUMBER> (INPUT PATH)"
end

