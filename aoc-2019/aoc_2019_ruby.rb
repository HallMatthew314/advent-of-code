# encoding: utf-8

require_relative "intcode_computer5.rb"

module AOC2019

  module_function

  def day1_part1(masses)
    masses.map { |m| m.to_i / 3 - 2 }.sum
  end

  def day1_part2(masses)
    fuel = ->(m) do
      f = m / 3 - 2
      f > 0 ? f + fuel.call(f) : 0
    end

    masses.map { |m| fuel.call(m.to_i) }.sum
  end

  def day2_part1(code, in1, in2)
    index = 0

    code[1] = in1
    code[2] = in2

    loop do
      o1 = code[index + 1]
      o2 = code[index + 2]
      write = code[index + 3]

      case code[index]
      when 1
        code[write] = code[o1] + code[o2]

      when 2
        code[write] = code[o1] * code[o2]

      when 99
        return code[0]

      else 
        raise "Invalid opcode: #{code[index]}"
      end

      index += 4
    end
  end

  # OPTIMIZE: Brute-force solutions are bad [Citation Needed].
  def day2_part2(code, target)
    (0..99).each do |in1|
      (0..99).each do |in2|
        return [in1, in2].join if day2_part1(code.dup, in1, in2) == target
      end
    end

    return "No valid inputs."
  end

  # HACK
  def day3_part1(wires)
    path = /^([UDLR])(\d+)$/

    wire1, wire2 = wires
    points1 = [[0, 0]]
    points2 = [[0, 0]]

    wire1.each do |w|
      data = w.match(path).captures

      data[1].to_i.times do
        point = points1.last.dup

        case data[0]
        when "U"
          point[1] += 1

        when "D"
          point[1] -= 1

        when "L"
          point[0] -= 1

        when "R"
          point[0] += 1

        else
          raise "Bad direction"
        end

        points1.push(point)
      end
    end

    wire2.each do |w|
      data = w.match(path).captures

      data[1].to_i.times do
        point = points2.last.dup

        case data[0]
        when "U"
          point[1] += 1

        when "D"
          point[1] -= 1

        when "L"
          point[0] -= 1

        when "R"
          point[0] += 1

        else
          raise "Bad direction"
        end

        points2.push(point)
      end
    end

    intersections = (points1.uniq & points2.uniq)
    intersections.delete([0, 0])

    manhattan = ->(p) { p[0].abs + p[1].abs }

    (intersections.map &manhattan).min
  end

  # HACK
  def day3_part2(wires)
    path = /^([UDLR])(\d+)$/

    wire1, wire2 = wires
    points1 = [[0, 0]]
    points2 = [[0, 0]]

    wire1.each do |w|
      data = w.match(path).captures

      data[1].to_i.times do
        point = points1.last.dup

        case data[0]
        when "U"
          point[1] += 1

        when "D"
          point[1] -= 1

        when "L"
          point[0] -= 1

        when "R"
          point[0] += 1

        else
          raise "Bad direction"
        end

        points1.push(point)
      end
    end

    wire2.each do |w|
      data = w.match(path).captures

      data[1].to_i.times do
        point = points2.last.dup

        case data[0]
        when "U"
          point[1] += 1

        when "D"
          point[1] -= 1

        when "L"
          point[0] -= 1

        when "R"
          point[0] += 1

        else
          raise "Bad direction"
        end

        points2.push(point)
      end
    end

    intersections = (points1 & points2).reject { |p| p == [0,0] }

    intersections.map do |i|
      points1.find_index(i) + points2.find_index(i)
    end.min
  end

  def day4_part1(range)
    code_valid = ->(c) do
      c =~ /(.)\1/ && c.chars == c.chars.sort
    end

    candidates = range.scan(/\d+/).map { |i| i.to_i }
    candidates = (candidates[0]..candidates[1]).to_a.map { |c| c.to_s }

    candidates.filter { |c| code_valid.call(c) }.size
  end

  def day4_part2(range)
    code_valid = ->(c) do
      c.chars == c.chars.sort &&
      ("0".."9").to_a.map { |i| c.chars.count(i) }.any?(2)
    end

    candidates = range.scan(/\d+/).map { |i| i.to_i }
    candidates = (candidates[0]..candidates[1]).to_a.map { |c| c.to_s }

    candidates.filter { |c| code_valid.call(c) }.size
  end

  # HACK
  def day5_part1(code, input)
    index = 0
    outputs = []

    loop do
      # Decode
      p1 = if (code[index] / 100) % 10 == 1
             code[index + 1]
           else
             code[code[index + 1]]
           end

      p2 = if (code[index] / 1_000) % 10 == 1
             code[index + 2]
           else
             code[code[index + 2]]
           end

      p3 = if code[index] / 10_000 == 1
             code[index + 3]
           else
             code[code[index + 3]]
           end

      opcode = code[index] % 100

      # Execute
      case opcode
      when 1
        code[code[index + 3]] = p1 + p2
        index += 4

      when 2
        code[code[index + 3]] = p1 * p2
        index += 4

      when 3
        # Get input here
        code[code[index + 1]] = input
        index += 2

      when 4
        outputs << code[code[index + 1]]
        index += 2

      when 99
        return outputs.last

      else 
        raise "Invalid opcode: #{opcode}"
      end
    end
  end

  # HACK
  def day5_part2(code, input)
    index = 0
    outputs = []

    loop do
      return outputs if code[index] % 100 == 99

      # Decode
      p1 = if (code[index] / 100) % 10 == 1
             code[index + 1]
           else
             code[code[index + 1]]
           end

      p2 = if (code[index] / 1_000) % 10 == 1
             code[index + 2]
           else
             code[code[index + 2]]
           end

      p3 = if code[index] / 10_000 == 1
             code[index + 3]
           else
             code[code[index + 3]]
           end

      opcode = code[index] % 100

      # Execute
      case opcode
      when 1
        code[code[index + 3]] = p1 + p2
        index += 4

      when 2
        code[code[index + 3]] = p1 * p2
        index += 4

      when 3
        # Get input here
        code[code[index + 1]] = input
        index += 2

      when 4
        outputs << code[code[index + 1]]
        index += 2

      when 5
        index = p1 == 0 ? index + 3 : p2

      when 6
        index = p1 == 0 ? p2 : index + 3

      when 7
        code[code[index + 3]] = p1 < p2 ? 1 :0
        index += 4

      when 8
        code[code[index + 3]] = p1 == p2 ? 1 : 0
        index += 4

      when 99
        return outputs.last

      else 
        raise "Invalid opcode: #{opcode}"
      end
    end
  end

  def day6_part1(orbit_map)
    orbits = {"COM" => nil}
    # A)B - B orbits A - use B as hash index
    orbit_map.map { |o| o.match(/(.+)\)(.+)/).captures }
      .each { |c| orbits[c[1]] = c[0] }

    inderect_orbits = ->(body) do
      return 0 if body == "COM"

      walker = orbits[body]
      i = 0
      until walker == "COM"
        walker = orbits[walker]
        i += 1
      end

      return i
    end

    orbits.keys.map { |o| inderect_orbits.call(o) }.sum +orbits.size - 1
  end

  def day6_part2(orbit_map)
    orbits = {"COM" => nil}
    orbit_map.map { |o| o.match(/(.+)\)(.+)/).captures }
      .each { |c| orbits[c[1]] = c[0] }

    orbit_chain = ->(body) do
      c = []
      walker = body
      c.push(walker)

      c.push(walker = orbits[walker]) until walker == "COM"

      return c
    end

    chain_you = orbit_chain.call("YOU")
    chain_san = orbit_chain.call("SAN")

    # Move up chain_you untill we find the common parent
    walker = "YOU"
    walker = orbits[walker] until chain_san.include?(walker)
    common_parent = walker

    chain_san.pop until chain_san.last == common_parent
    chain_you.pop until chain_you.last == common_parent

    chain_you.size + chain_san.size - 4
  end

  def day7_part1(code)
    signals = {}
    computer = IntcodeComputer5.new(code)

    (0..4).to_a.permutation do |perm|
      signal = 0

      perm.each do |p|
        computer.reset
        computer.run([p, signal])
        signal = computer.fetch_output
      end

      signals[perm] = signal
    end
    signals.values.max
  end

  def day7_part2(code)
    signals = {}
    computer = IntcodeComputer5.new(code)

    (5..9).to_a.permutation do |p|
      

      # IGNORE BELOW THIS COMMENT
      input_signal = 0

      # REVIEW: Maybe a loop could be used here?
      # Thruster A
      input_signal = computer.run([p[0], input_signal]).first

      # Thruster B
      input_signal = computer.run([p[1], input_signal]).first

      # Thruster C
      input_signal = computer.run([p[2], input_signal]).first

      # Thruster D
      input_signal = computer.run([p[3], input_signal]).first

      # Thruster E, store result
      signals[p] = computer.run([p[4], input_signal]).first
    end
    signals.values.max
  end
end

