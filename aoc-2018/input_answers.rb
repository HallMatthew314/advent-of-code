#!/usr/bin/env ruby
# encoding: utf-8

require_relative "aoc_2018_ruby.rb"

def day1(path)
  deltas = File.readlines(path || "day1_input.txt").map { |i| i.to_i }
  puts "Part 1: #{AOC2018.day1_part1(deltas.dup)}"
  puts "Part 2: #{AOC2018.day1_part2(deltas.dup)}"
end

case ARGV[0].to_i
when 1 then day1(ARGV[1])
else puts "Please supply a day to run: ./input_answers.rb <DAY NUMBER> (INPUT PATH)"
end

