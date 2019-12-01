# encoding: utf-8

module AOC2019

  module_function

  def day1_part1(mods)
    mods.map { |m| m.to_i / 3 - 2 }.sum
  end

  def day1_part2(mods)
    fuel = ->(m) do
      f = m / 3 - 2
      f > 0 ? f + fuel.call(f) : 0
    end

    mods.map { |m| fuel.call(m.to_i) }.sum
  end
end

