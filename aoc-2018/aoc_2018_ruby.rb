# encoding: utf-8

require "matrix"

module AOC2018

  Rect = Struct.new(:x, :y, :width, :height) do
    def top
      y
    end

    def bottom
      y + height - 1
    end

    def left
      x
    end

    def right
      x + width - 1
    end

    def overlap?(other)
      return false if left > other.right || right < other.left
      return false if bottom > other.top || top < other.bottom
      true
    end
  end

  Claim = Struct.new(:id, :rectangle) do
    def overlap?(other)
      rectangle.overlap?(other.rectangle)
    end
  end

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

  def day3_part1(claims)
    fabric = Hash.new(0)
    p_claim = /(\d+),(\d+): (\d+)x(\d+)$/

    claims.each do |claim|
      xpos, ypos, w, h = claim.match(p_claim).captures.map(&:to_i)

      (ypos...ypos + h).each do |y|
        (xpos...xpos + w).each { |x| fabric[[x, y]] += 1 }
      end
    end

    fabric.values.count { |v| v > 1 }
  end

  def day3_part2(claims)

    p_claim = /^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$/

    claims.map! do |c|
      i, x, y, w, h = c.match(p_claim).captures.map(&:to_i)
      Claim.new(i, Rect.new(x, y, w, h))
    end

    until claims.empty?
      claim = claims.pop
      old_size = claims.size

      claims.reject! { |c| claim.overlap?(c) }

      return claim.id if old_size == claims.size
    end

    raise "They all overlap."
  end
end

