# encoding: utf-8

module AOC2015

  module_function

  def day1_part1(directions)
    directions.chars.map { |d| d == "(" ? 1 : -1 }.sum
  end

  def day1_part2(directions)
    floor = 0
    index = 0

    until floor == -1
      floor += directions[index] == "(" ? 1 : -1
      index += 1
    end

    return index
  end
end

