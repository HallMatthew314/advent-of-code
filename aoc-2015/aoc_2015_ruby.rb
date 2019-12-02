# encoding: utf-8

require "digest"
require "matrix"

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

  def day2_part1(dims)
    p = /^(\d+)x(\d+)x(\d+)$/
    total = 0

    dims.each do |d|
      measures = p.match(d).captures.map { |c| c.to_i }
      # HACK: There isn't a map_index method, so this is
      # an approximation of what it might be.
      indices = measures.each_index.to_a
      areas = indices.map { |i| measures[i] * measures[i - 1] }
      total += areas.sum * 2 + areas.min
    end

    return total
  end

  def day2_part2(dims)
    p = /^(\d+)x(\d+)x(\d+)$/
    total = 0

    dims.each do |d|
      measures = p.match(d).captures.map { |c| c.to_i }
      # Wrap
      total += (measures.sum - measures.max) * 2
      # Bow
      total += measures.reduce(:*)
    end

    return total
  end

  def day3_part1(directions)
    x = 0
    y = 0
    locations = [[x, y]]

    directions.chars.each do |d|
      case d
      when "^" then y -= 1
      when "v" then y += 1
      when "<" then x -= 1
      when ">" then x += 1
      else raise "Uh oh, Spaghetti-Os"
      end

      locations.push([x, y])
    end
    locations.uniq.size
  end

  # REVIEW: There's probably a better way to do this.
  def day3_part2(directions)
    s_x = 0
    s_y = 0
    r_x = 0
    r_y = 0
    robot_turn = false

    locations = [[0, 0]]

    directions.chars.each do |d|
      if robot_turn
        case d
        when "^" then r_y -= 1
        when "v" then r_y += 1
        when "<" then r_x -= 1
        when ">" then r_x += 1
        else raise "Uh oh, Spaghetti-Os"
        end

        locations.push([r_x, r_y])
      else
        case d
        when "^" then s_y -= 1
        when "v" then s_y += 1
        when "<" then s_x -= 1
        when ">" then s_x += 1
        else raise "Uh oh, Spaghetti-Os"
        end

        locations.push([s_x, s_y])
      end

      robot_turn = !robot_turn
    end
    locations.uniq.size
  end

  def day4_part1(key)
    h = "xxxxx"
    n = 0
    h = Digest::MD5.hexdigest("#{key}#{n += 1}") until h[0..4] == "00000"
    return n
  end

  def day4_part2(key)
    h = "xxxxxx"
    n = 0
    h = Digest::MD5.hexdigest("#{key}#{n += 1}") until h[0..5] == "000000"
    return n
  end

  def day5_part1(strings)
    nice_score = ->(s) do
      return 0 if s =~ /ab|cd|pq|xy/
      (s =~ /(.)\1/) && (s.scan(/[aeiou]/).size > 2) ? 1 : 0
    end

    strings.map { |s| nice_score.call(s) }.sum
  end

  def day5_part2(strings)
    nice_score = ->(s) do
      (s =~ /(..).*\1/) && (s =~ /(.).\1/) ? 1 : 0
    end

    strings.map { |s| nice_score.call(s) }.sum
  end

  def day6_part1(instructions)
    actions = {}
    actions["toggle"] = ->(b) { !b }
    actions["turn off"] = ->(b) { false }
    actions["turn on"] = ->(b) { true }

    # Initialise the matrix.
    rows = []
    1_000.times { rows.push([false] * 1_000) }
    lights = Matrix.rows(rows)

    # Loop the instructions.
    instructions.each do |step|
      action = actions[step.match(/^(toggle|turn on|turn off)/)[1]]
      coords = step.match(/(\d+),(\d+) through (\d+),(\d+)$/)
      xs = coords[1].to_i
      xe = coords[3].to_i
      ys = coords[2].to_i
      ye = coords[4].to_i

      (ys..ye).each do |y|
        (xs..xe).each { |x| lights[y, x] = action.call(lights[y, x]) }
      end
    end

    # Count the number of lights turned on.
    lights.map { |c| c ? 1 : 0 }.sum
  end

  def day6_part2(instructions)
    actions = {}
    actions["toggle"] = ->(i) { i + 2 }
    actions["turn off"] = ->(i) { i > 0 ? i - 1 : 0 }
    actions["turn on"] = ->(i) { i + 1 }

    # Initialise the matrix.
    rows = []
    1_000.times { rows.push([0] * 1_000) }
    lights = Matrix.rows(rows)

    # Loop the instructions.
    instructions.each do |step|
      action = actions[step.match(/^(toggle|turn on|turn off)/)[1]]
      coords = step.match(/(\d+),(\d+) through (\d+),(\d+)$/)
      xs = coords[1].to_i
      xe = coords[3].to_i
      ys = coords[2].to_i
      ye = coords[4].to_i

      (ys..ye).each do |y|
        (xs..xe).each { |x| lights[y, x] = action.call(lights[y, x]) }
      end
    end

    # Count the number of lights turned on.
    lights.sum
  end
end

