# encoding: utf-8

module AOC2017

  module_function

  def day1_part1(captcha)
    total = 0

    (-1...captcha.size - 1).each do |i|
      total += captcha[i].to_i if captcha[i] == captcha[i + 1]
    end

    total
  end

  def day1_part2(captcha)
    total = 0
    half = captcha.size / 2

    (0...half).each do |i|
      total += captcha[i].to_i * 2 if captcha[i] == captcha[i + half]
    end

    total
  end

  def day2_part1(rows)
    chkrow = ->(r) do
      ints = r.scan(/\d+/).map { |i| i.to_i }
      ints.max - ints.min
    end

    (rows.map &chkrow).sum
  end

  def day2_part2(rows)
    # HACK: Probably a nicer way to do this.
    divrow = ->(r) do
      ints = r.scan(/\d+/).map { |i| i.to_i }.sort
      until ints.empty?
        l = ints.shift
        ints.each { |i| return i / l if i % l == 0 }
      end
    end

    (rows.map &divrow).sum
  end

  def day3_part1(square)
    return 0 if square < 2
    manhattan = ->(p) { p[0].abs + p[1].abs }

    p = [0, 0]
    points = {[0, 0] => 1}
    i = 1
    n = 2

    while i < square
      # right one
      i += 1
      p[0] += 1
      points[p.dup] = i

      # up n-1
      (n - 1).times do
        i += 1
        p[1] -= 1
        points[p.dup] = i
      end

      # left n
      n.times do
        i += 1
        p[0] -= 1
        points[p.dup] = i
      end

      # down n
      n.times do
        i += 1
        p[1] += 1
        points[p.dup] = i
      end

      # right n
      n.times do
        i += 1
        p[0] += 1
        points[p.dup] = i
      end

      n += 2
    end

    manhattan.call(points.key(square))
  end

  # HACK: it might be better to keep a hash of Vectors
  def day3_part2(input)
    p = [0, 0]
    i = 1
    n = 2
    points = {[0, 0] => 1}

    calc_value = ->(p) do
      sum = 0
      p = p.dup

      p[0] += 1
      sum += points[p] if points[p]
      p[1] -= 1
      sum += points[p] if points[p]

      2.times do
        p[0] -= 1
        sum += points[p] if points[p]
      end

      2.times do
        p[1] += 1
        sum += points[p] if points[p]
      end 

      2.times do
        p[0] += 1
        sum += points[p] if points[p]
      end

      sum
    end

    loop do
      # Right one
      p[0] += 1
      points[p.dup] = calc_value.call(p)
      return points[p] if points[p] > input

      # Up n-1
      (n - 1).times do
        p[1] -= 1
        points[p.dup] = calc_value.call(p)
        return points[p] if points[p] > input
      end

      # Left n
      n.times do
        p[0] -= 1
        points[p.dup] = calc_value.call(p)
        return points[p] if points[p] > input
      end

      # Down n
      n.times do
        p[1] += 1
        points[p.dup] = calc_value.call(p)
        return points[p] if points[p] > input
      end

      # Right n
      n.times do
        p[0] += 1
        points[p.dup] = calc_value.call(p)
        return pointsp[p] if points[p] > input
      end

      n += 2
    end
  end
end

