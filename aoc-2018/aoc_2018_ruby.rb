# encoding: utf-8

module AOC2018

  module_function

  def day1_part1(deltas)
    deltas.sum
  end

  def day1_part2(deltas)
    history = {}
    sum = 0
    index = 0

    until history[sum]
      history[sum] = true
      sum += deltas[index]
      index = (index + 1) % deltas.size
    end

    sum
  end
end

