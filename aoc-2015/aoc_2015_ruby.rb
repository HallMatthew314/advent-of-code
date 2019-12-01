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

end

