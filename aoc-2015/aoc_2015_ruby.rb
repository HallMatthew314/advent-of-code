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

  # Currently doesn't work
  # Try using a binary tree
  def day7_part1(circuit)
    wires = {}
    gates = {}
    p_gate_test = /AND|LSHIFT|NOT|OR|RSHIFT/
    # Captures: left, gate, right, destination
    p_gate = /^(.+ )?([A-Z]+) (.+) -> (.+)$/
    # Captures: source, destination
    p_no_gate = /^(.+) -> (.+)$/

    # Tests if an input is a wire or literal.
    # Returns either the literal or the gate's value.
    decode = ->(i) { i =~ /^[0-9]+$/ ? i.to_i : wires[i] }

    gates["AND"] = ->(l, r) { decode.call(l.strip) & decode.call(r) }
    gates["LSHIFT"] = ->(l, r) { decode.call(l.strip) << r.to_i }
    gates["NOT"] = ->(_, r) { ~decode.call(r) }
    gates["OR"] = ->(l, r) { decode.call(l.strip) | decode.call(r) }
    gates["RSHIFT"] = ->(l, r) { decode.call(l.strip) >> r.to_i }

    circuit.each do |connection|
      if connection =~ p_gate_test
        # there is a gate
        data = connection.match(p_gate)
        wires[data[4]] = gates[data[2]].call(data[1], data[3])
      else
        data = connection.match(p_no_gate)
        wires[data[2]] = decode.call(data[1])
      end
    end

    wires["a"]
  end
end

