#!/usr/bin/env ruby
# encoding: utf-8

require "test/unit"
require_relative "intcode_computer5.rb"

class TestIntcodeComputer5 < Test::Unit::TestCase

  def test_day5_part2
    code = [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8]
    com = IntcodeComputer5.new(code)
    com.send_input(8)
    com.run
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
end

