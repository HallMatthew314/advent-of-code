# encoding: utf-8

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
end

