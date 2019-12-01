# encoding: utf-8

module AOC2018

  module_function

  def day1(directions)
    raise ArgumentError unless /^[()]*$/ =~ directions
    directions.chars.map { |d| d == "(" ? 1 : -1 }.sum
  end
end

