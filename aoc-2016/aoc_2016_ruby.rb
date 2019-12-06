# encoding: utf-8

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

    decode = ->(letters) do
      point = [0, 0]
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
      digit_for_point[point]
    end

    code.map { |letters| decode.call(letters) }.join
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

    decode = ->(letters) do
      point = [-2, 0]
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
      digit_for_point[point]
    end

    code.map { |letters| decode.call(letters) }.join
  end
end

