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
end

