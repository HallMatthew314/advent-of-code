#!/usr/bin/env ruby
# encoding: utf-8

require "test/unit"
require_relative "intcode_computer.rb"

class TestIntcodeComputer5 < Test::Unit::TestCase

  def test_day5_part2
    code = [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8]
    com = IntcodeComputer5.new(code)
    com.send_input(8)
    com.run
    assert_equal(1, com.fetch_output)
  end

  def test_steps_only
    code = [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8]
    com = IntcodeComputer5.new(code)
    com.send_input(8)

    com.step until com.state == "DONE"
    assert_equal(1, com.fetch_output)
  end

  def test_idle_state
    code = [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8]
    com = IntcodeComputer5.new(code)
    com.run
    assert_equal("IDLE", com.state)

    com.step
    assert_equal("IDLE", com.state)

    com.run
    assert_equal("IDLE", com.state)

    com.send_input(8)
    com.step
    assert_equal("RUNNING", com.state)

    com.run
    assert_equal("DONE", com.state)
    assert_equal(1, com.fetch_output)
  end

  def test_day9_example1
    code = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
    com = IntcodeComputer5.new(code)
    com.run
    assert_equal(code, com.view_output)
  end

  def test_day9_example2
    code = [1102,34915192,34915192,7,4,7,99,0]
    com = IntcodeComputer5.new(code)
    com.run
    assert(Math.log10(com.fetch_output).floor == 15)
  end

  def test_day9_example3
    code = [104,1125899906842624,99]
    com = IntcodeComputer5.new(code)
    com.run
    assert_equal(1125899906842624, com.fetch_output)
  end
end

