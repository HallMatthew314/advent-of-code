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
end

