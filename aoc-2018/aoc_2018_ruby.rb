# encoding: utf-8

require "matrix"

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

  def day2_part1(ids)
    two_or_three = lambda do |id|
      counts = Hash.new(0)
      id.chars.each { |c| counts[c] += 1 }

      Vector.elements([
        counts.values.any?(2) ? 1 : 0,
        counts.values.any?(3) ? 1 : 0
      ])
    end

    # Array#reduce(:+) is used here because Array#sum
    # does not work with Vector objects
    (ids.map &two_or_three).reduce(:+).reduce(:*)
  end

  def day2_part2(ids)
    difference = lambda do |a, b|
      return [0] if a[1..] == b[1..] && a[0] != a[1]

      indices = []
      (0...a.size).each do |i|
        unless a[i] == b[i]
          indices.push(i)
        end
      end

      indices
    end

    ids.sort!

    (-1...ids.size - 1).each do |index|
      diffs = difference.call(ids[index], ids[index + 1])
      if diffs.size == 1
        retval = ids[index].chars
        retval.delete_at(diffs.first)
        return retval.join
      end
    end
  end
end

# 
# pbykrmjmizwhxlqnwasfgtycdv

