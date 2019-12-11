# encoding: utf-8

require "matrix"
require_relative "intcode_computer.rb"

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
    computer = IntcodeComputer.new(code)

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

    (5..9).to_a.permutation do |perm|
      computers = []
      5.times { computers << IntcodeComputer.new(code) }
      perm.each_index { |i| computers[i].send_input(perm[i]) }
      computers.first.send_input(0)

      c = 0
      until computers.last.state == "DONE"
        if computers[c].state == "IDLE"
          sig = computers[c - 1].fetch_output
          computers[c].send_input(sig) unless sig.nil?
        end

        computers[c].step
        c = (c + 1) % 5
      end

      signals[perm] = computers.last.fetch_output
    end
    signals.values.max
  end

  def day8_part1(image, width=25, height=6)
    layer_size = width * height

    layers = []

    (0...image.size).step(layer_size) do |i|
      layers << image[i, layer_size]
    end

    target_layer = layers.sort do |l, m| 
      l.count("0") <=> m.count("0")
    end.first

    target_layer.count("1") * target_layer.count("2")
  end

  def day8_part2(image, width=25, height=6)
    layer_size = width * height

    layers = []
    (0...image.size).step(layer_size) do |i|
      layers << image[i, layer_size]
    end

    final = layers.shift

    layers.each do |l|
      l.chars.each_index { |i| final[i] = l[i] if final[i] == "2" }
    end

    str = ""
    (0...layer_size).step(width) { |i| str << "#{final[i, width]}\n" }
    str.gsub(/\d/, {"0" => " ", "1" => "#"}).chomp
  end

  def day9_part1(code)
    com = IntcodeComputer.new(code)
    com.send_input(1)
    com.run
    com.fetch_output
  end

  def day9_part2(code)
    com = IntcodeComputer.new(code)
    com.send_input(2)
    com.run
    com.fetch_output
  end

  def day10_part1(grid)
    width = grid[0].size
    height = grid.size
    points = []

    # Extract locations of asteroids.
    (0...height).each do |y|
      (0...width).each { |x| points.push([x, y]) if grid[y][x] == "#" }
    end

    count_lines = ->(base) do
      # Names less, equal, greater refer to x values.
      points_less = points.filter { |p| p[0] < base[0] }
      points_equal = points.filter { |p| p[0] == base[0] }
      points_greater = points.filter { |p| p[0] > base[0] }

      lines = ->(p) do
        a, b = base[0] > p[0] ? [base, p] : [p, base]

        m = (a[1] - b[1]).to_r / (a[0] - b[0]).abs
        c = a[1] - (m * a[0])

        [m, c]
      end

      count_less = (points_less.map &lines).uniq.size
      count_greater = (points_greater.map &lines).uniq.size
      count_equal = 0
      count_equal += 1 if points_equal.any? { |p| p[1] < base[1] }
      count_equal += 1 if points_equal.any? { |p| p[1] > base[1] }

      [base, count_less + count_greater + count_equal]
    end

    (points.map &count_lines).max { |a, b| a[1] <=> b[1] }
  end

  def day10_part2(grid)
    origin = day10_part1(grid)[0].dup
    width = grid[0].size
    height = grid.size

    radius = ->(a) do
      Math.sqrt((a[0] - origin[0])**2 + (a[1] - origin[1])**2)
    end

    angle = ->(a) do
      Math.atan2(-(a[1] - origin[1]), a[0] - origin[0])
    end

    asteroids = []
    (0...height).each do |y|
      (0...width).each { |x| asteroids.push([x, y]) if grid[y][x] == "#" }
    end

    asteroids -= [origin]

    angles = {}

    # Make sure there is an array for every angle.
    # There is likely a better way to do this but
    # I'm tired of this challenge.
    angles[Math::PI / 2] = []
    asteroids.each { |a| angles[angle.call(a)] = [] }

    asteroids.each { |a| angles[angle.call(a)].push(a.dup) }

    angles.values.each do |v|
      v.sort! { |a, b| radius.call(a) <=> radius.call(b) }
    end

    keys = angles.keys.dup.sort
    # I thought this should be PI/2, but it works.
    start_offset = keys.index(Math::PI / 2)
    i = start_offset

    destroyed = []
    until destroyed.size == 200
      k = keys[i]
      a = angles[k].shift
      destroyed.push(a) unless a.nil?
      i = (i - 1) % keys.size
    end

    t = destroyed.last
    t[0] * 100 + t[1]
  end

  def day11_part1(code)
    directions = {
      :east => 0,
      :north => 1,
      :west => 2,
      :south => 3
    }

    panels = {}
    panels.default = -1
    r_location = [0, 0]
    r_dir = directions[:north]
    com = IntcodeComputer.new(code)

    turn = ->(t) { r_dir = (r_dir + (t.zero? ? -1 : 1)) % 4 }
    move = -> do
      case r_dir
      when directions[:east]
        r_location[0] += 1
      when directions[:north]
        r_location[1] -= 1
      when directions[:west]
        r_location[0] -= 1
      when directions[:south]
        r_location[1] += 1
      else
        raise "Bad direction"
      end
    end

    until com.done?
      # Is the panel white?
      c = panels[r_location] > 0 ? 1 : 0
      com.send_input(c)
      com.run
      raise "CRASHED: #{com.error_log}" if com.crash?

      p = com.fetch_output
      t = com.fetch_output

      panels[r_location] = p
      turn.call(t)
      move.call
    end

    panels.size
  end

  def day11_part2(code)
    directions = {
      :east => 0,
      :north => 1,
      :west => 2,
      :south => 3
    }

    r_location = [0, 0]
    r_dir = directions[:north]
    com = IntcodeComputer.new(code)
    panels = {}
    panels.default = -1
    panels[r_location] = 1

    turn = ->(t) { r_dir = (r_dir + (t.zero? ? -1 : 1)) % 4 }
    move = -> do
      case r_dir
      when directions[:east]
        r_location[0] += 1
      when directions[:north]
        r_location[1] -= 1
      when directions[:west]
        r_location[0] -= 1
      when directions[:south]
        r_location[1] += 1
      else
        raise "Bad direction"
      end
    end

    until com.done?
      # Is the panel white?
      c = panels[r_location] > 0 ? 1 : 0
      com.send_input(c)
      com.run
      raise "CRASHED: #{com.error_log}" if com.crash?

      p = com.fetch_output
      t = com.fetch_output

      panels[r_location.dup] = p
      turn.call(t)
      move.call
    end

    min_x = panels.keys.min { |a, b| a[0] <=> b[0] }[0]
    min_y = panels.keys.min { |a, b| a[1] <=> b[1] }[1]
    max_x = panels.keys.max { |a, b| a[0] <=> b[0] }[0]
    max_y = panels.keys.max { |a, b| a[1] <=> b[1] }[1]

    translate = ->(p) { [ p[1] - min_y, p[0] - min_x ] }

    hull = Matrix.build(max_y - min_y + 1, max_x - min_x + 1) { 0 }
    puts "#{hull.row_size} x #{hull.column_size}"
    panels.each_pair do |point, color|
      p = translate.call(point)
      hull[p[0], p[1]] = color
    end

    rows = []
    hull.row_vectors.each { |r| rows << r.to_a.reverse.join }
    rows.join("\n").gsub(/\d/, {"0" => ".", "1" => "#"})
  end
end

