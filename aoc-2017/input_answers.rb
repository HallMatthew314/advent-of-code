#!/usr/bin/env ruby
# encoding: utf-8

require_relative "aoc_2017_ruby.rb"

def day1(path)
  captcha = File.read(path || "day1_input.txt").strip
  puts "Part 1: #{AOC2017.day1_part1(captcha)}"
  puts "Part 2: #{AOC2017.day1_part2(captcha)}"
end

case ARGV[0].to_i
when 1 then day1(ARGV[1])
else puts "Please supply a day to run: ./input_answers.rb <DAY NUMBER> (INPUT PATH)"
end

