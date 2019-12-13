# encoding: utf-8

require "matrix"

module AOC2016

  module_function

  def day1_part1(path)
    d = 1
    location = [0, 0]

    directions = [
      [1,0],
      [1,1],
      [-1,0],
      [-1,1]
    ]

    path.each do |p|
      d = (p[0] == "L" ? d + 1 : d - 1) % 4

      pn, xy = directions[d]

      location[xy] += pn * p.scan(/\d+/)[0].to_i
    end

    location.map{ |l| l.abs }.sum
  end

  def day1_part2(path)
    d = 1
    points = [[0, 0]]

    directions = [
      [1,0],
      [1,1],
      [-1,0],
      [-1,1]
    ]

    path.each do |p|
      d = (p[0] == "L" ? d + 1 : d - 1) % 4

      pn, xy = directions[d]

      p.scan(/\d+/)[0].to_i.times do
        l = points.last.dup
        l[xy] += pn
        points.push(l)

        return points.last.map{ |l| l.abs }.sum unless points == points.uniq
      end
    end

    raise "No location visited twice"
  end

  def day2_part1(code)
    digit_for_point = {
      [-1, -1] => 1,
      [0, -1] => 2,
      [1, -1] => 3,
      [-1, 0] => 4,
      [0, 0] => 5,
      [1, 0] => 6,
      [-1, 1] => 7,
      [0, 1] => 8,
      [1, 1] => 9,
    }

    decode = ->(letters, start) do
      point = start
      letters.chars.each do |l|
        case l
        when "L"
          point[0] -= 1 unless point[0] == -1

        when "R"
          point[0] += 1 unless point[0] == 1

        when "U"
          point[1] -= 1 unless point[1] == -1

        when "D"
          point[1] += 1 unless point[1] == 1

        else
          raise "Bad letter: #{l}"
        end
      end
      # Explicit return needed to return two things.
      return point, digit_for_point[point]
    end

    final_code = []
    p = [0, 0]
    code.each do |letters|
      p, digit = decode.call(letters, p)
      final_code << digit
    end

    final_code.join
  end

  def day2_part2(code)
    digit_for_point = {
      [0, -2] => 1,
      [-1, -1] => 2,
      [0, -1] => 3,
      [1, -1] => 4,
      [-2, 0] => 5,
      [-1, 0] => 6,
      [0, 0] => 7,
      [1, 0] => 8,
      [2, 0] => 9,
      [-1, 1] => "A",
      [0, 1] => "B",
      [1, 1] => "C",
      [0, 2] => "D",
    }

    decode = ->(letters, start) do
      point = start
      letters.chars.each do |l|
        case l
        when "L"
          point[0] -= 1 unless digit_for_point[[point[0] - 1, point[1]]].nil?

        when "R"
          point[0] += 1 unless digit_for_point[[point[0] + 1, point[1]]].nil?

        when "U"
          point[1] -= 1 unless digit_for_point[[point[0], point[1] - 1]].nil?

        when "D"
          point[1] += 1 unless digit_for_point[[point[0], point[1] + 1]].nil?

        else
          raise "Bad letter: #{l}"
        end
      end
      return point, digit_for_point[point]
    end

    final_code = []
    p = [-2, 0]
    code.each do |letters|
      p, digit = decode.call(letters, p)
      final_code << digit
    end

    final_code.join
  end

  def day3_part1(triangles)
    triangles.map! { |t| t.scan(/\d+/).map { |i| i.to_i }.sort }

    valid = ->(t) { t[0] + t[1] > t[2] }

    (triangles.filter &valid).size
  end

  def day3_part2(triangles)
    triangles.map! { |t| t.scan(/\d+/).map { |i| i.to_i } }
    valid = ->(t) { t[0] + t[1] > t[2] }

    count = triangles.size
    mat = Matrix.rows(triangles).t
    real_triangles = []

    mat.row_vectors.each do |row|
      (0...mat.column_size).step(3) do |i|
        real_triangles.push(row[i..i + 2].sort)
      end
    end

    (real_triangles.filter &valid).size
  end

  def day4_part1(rooms)
    p_room = /(\d+)\[(.{5})\]/

    chksum = ->(str) do
      tally = Hash.new(0)
      str.chars.each { |c| tally[c] += 1 }

      counts = {}
      tally.values.each { |k| counts[k] = [] }
      tally.each { |k, v| counts[v].push(k) }

      counts.each_value { |v| v.sort! }
      counts.to_a.sort { |a, b| b[0] <=> a[0] }
        .map { |c| c[1] }.flatten.first(5).join
    end

    id_total = 0

    rooms.each do |room|
      name = room.split("-")
      id, sum = name.pop.match(p_room).captures

      id_total += id.to_i if sum == chksum.call(name.join)
    end

    id_total
  end

  def day4_part2(rooms)
    new_letter = ->(l, offset) do
      l == "-" ? " " : ((l.ord - 97 + offset) % 26 + 97).chr
    end

    decrypt = ->(room) do
      name = room.split("-")
      o = name.pop.scan(/\d+/).first.to_i

      [name.join("-").chars.map { |c| new_letter.call(c, o) }.join, o]
    end

    (rooms.map &decrypt).filter { |r| r[0] =~ /north|pole|object/ }
  end
end

