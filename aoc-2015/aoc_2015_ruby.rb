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
    y = 1
    santa = [0, 0]
    locations = [[0, 0]]

    directions.chars.each do |d|
      case d
      when "^" then santa[y] -= 1
      when "v" then santa[y] += 1
      when "<" then santa[x] -= 1
      when ">" then santa[x] += 1
      else raise "Uh oh, Spaghetti-Os"
      end

      locations.push(santa.dup)
    end
    locations.uniq.size
  end

  def day3_part2(directions)
    # 'Constants' for readble indexing.
    x = 0
    y = 1
    santa = [0, 0]
    robot = [0, 0]
    robot_turn = false

    locations = [[0, 0]]

    directions.chars.each do |d|
      if robot_turn
        case d
        when "^" then robot[y] -= 1
        when "v" then robot[y] += 1
        when "<" then robot[x] -= 1
        when ">" then robot[x] += 1
        else raise "Uh oh, Spaghetti-Os"
        end

        locations.push(robot.dup)
      else
        case d
        when "^" then santa[y] -= 1
        when "v" then santa[y] += 1
        when "<" then santa[x] -= 1
        when ">" then santa[x] += 1
        else raise "Uh oh, Spaghetti-Os"
        end

        locations.push(santa.dup)
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
    nice = ->(s) do
      return false if s =~ /ab|cd|pq|xy/
      (s =~ /(.)\1/) && (s.scan(/[aeiou]/).size > 2)
    end

    strings.filter { |s| nice.call(s) }.size
  end

  def day5_part2(strings)
    nice = ->(s) do
      (s =~ /(..).*\1/) && (s =~ /(.).\1/)
    end

    strings.filter { |s| nice.call(s) }.size
  end

  def day6_part1(instructions)
    toggle = ->(l) { l == 1 ? 0 : 1 }

    # Initialise the matrix.
    rows = []
    1_000.times { rows.push([0] * 1_000) }
    lights = Matrix.rows(rows)

    # Loop the instructions.
    instructions.each do |step|
      action = step.match(/^(toggle|turn on|turn off)/)[1]
      coords = step.match(/(\d+),(\d+) through (\d+),(\d+)$/)
      
      x_range = coords[1].to_i..coords[3].to_i
      y_range = coords[2].to_i..coords[4].to_i

      case action
      when "toggle"
        y_range.each do |y|
          x_range.each { |x| lights[y, x] = toggle.call(lights[y, x]) }
        end

      when "turn on"
        lights[x_range, y_range] = 1

      when "turn off"
        lights[x_range, y_range] = 0

      else
        raise "Invalid instruction"
      end

    end

    # Count the number of lights turned on.
    lights.sum
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

      x_range = coords[1].to_i..coords[3].to_i
      y_range = coords[2].to_i..coords[4].to_i

      y_range.each do |y|
        x_range.each { |x| lights[y, x] = action.call(lights[y, x]) }
      end
    end

    # Sum the brightness of each light.
    lights.sum
  end

  def day7_part1(circuit)
    gates = {}
    gates["AND"] = ->(l, r) { l & r }
    gates["LSHIFT"] = ->(l, r) { l << r }
    gates["NOT"] = ->(_, r) { ~r }
    gates["OR"] = ->(l, r) { l | r }
    gates["RSHIFT"] = ->(l, r) { l >> r }

    wires = {}
    circuit.map { |conn| conn.match(/^(.+) -> ([a-z]+)$/).captures }
      .each { |c| wires[c[1]] = c[0] }

    find = ->(w) do
      return nil if w.nil?
      return w.to_i if w.is_a?(Integer) || w =~ /^[0-9]+$/

      val = wires[w]
      # Int literal
      if val.is_a?(Integer) || val =~ /^[0-9]+$/
        wires[w] = val.to_i

      # Direct from another wire
      elsif val =~ /^[a-z]+$/
        wires[w] = find.call(val)

      # Gate
      else
        left, gate, right = val.match(/^(.+ )?(.+) (.+)$/).captures
        left = find.call(left&.strip)
        right = find.call(right)
        wires[w] = gates[gate].call(left, right)
      end
      return wires[w]
    end

    find.call("a")
  end

  def day7_part2(circuit)
    gates = {}
    gates["AND"] = ->(l, r) { l & r }
    gates["LSHIFT"] = ->(l, r) { l << r }
    gates["NOT"] = ->(_, r) { ~r }
    gates["OR"] = ->(l, r) { l | r }
    gates["RSHIFT"] = ->(l, r) { l >> r }

    wires = {}
    circuit.map { |conn| conn.match(/^(.+) -> ([a-z]+)$/).captures }
      .each { |c| wires[c[1]] = c[0] }

    # Override wire `b` with value of wire `a` in the last question.
    wires["b"] = day7_part1(circuit) 

    find = ->(w) do
      return nil if w.nil?
      return w.to_i if w.is_a?(Integer) || w =~ /^[0-9]+$/

      val = wires[w]
      # Int literal
      if val.is_a?(Integer) || val =~ /^[0-9]+$/
        wires[w] = val.to_i

      # Direct from another wire
      elsif val =~ /^[a-z]+$/
        wires[w] = find.call(val)

      # Gate
      else
        left, gate, right = val.match(/^(.+ )?(.+) (.+)$/).captures
        left = find.call(left&.strip)
        right = find.call(right)
        wires[w] = gates[gate].call(left, right)
      end
      return wires[w]
    end

    find.call("a")
  end
end

