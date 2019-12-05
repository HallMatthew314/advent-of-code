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
end

