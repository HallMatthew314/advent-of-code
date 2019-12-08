# encoding: utf-8

# Intcode Computer class following specifications of Day 5.
class IntcodeComputer5

  # @state values:
  # READY - Program has not started.
  # RUNNING - Program is running.
  # IDLE - Program is waiting for input (queue is empty).
  # CRASH - Program encountered invalid opcode.
  # DONE - Program reached halt opcode.

  def initialize(program)
    unless program.is_a?(Array)
      raise ArgumentError, "Program was not an Array"
    end

    @program = program.dup

    reset
  end

  def reset
    @memory = @program.dup
    @cycle = 0
    @mem_ptr = 0
    @state = "READY"
    @input_queue = []
    @output_queue = []
    @error_log = nil
  end

  def fetch_output
    @output_queue.shift
  end

  # TODO: Allow send Arrays of integers
  def send_input(i)
    raise ArgumentError, "Inputs must be integers" unless i.is_a?(Integer)
    @input_queue << i
  end

  def run(inputs=[])
    raise ArgumentError, "Input was not an Array" unless inputs.is_a?(Array)
    inputs.each { |i| send_input(i) }

    @state = "RUNNING"
    step while @state == "RUNNING"

    return @state
  end

  def step
    @cycle += 1

    # HLT
    if @memory[@mem_ptr] % 100 == 99
      @state = "DONE"
      return
    end

    # Decode
    # Parameter 1
    p1 = if (@memory[@mem_ptr] / 100) % 10 == 1
           @memory[@mem_ptr + 1]
         else
           @memory[@memory[@mem_ptr + 1]]
         end

    # Parameter 2
    p2 = if (@memory[@mem_ptr] / 1_000) % 10 == 1
           @memory[@mem_ptr + 2]
         else
           @memory[@memory[@mem_ptr + 2]]
         end

    # Parameter 3
    p3 = if @memory[@mem_ptr] / 10_000 == 1
            @memory[@mem_ptr + 3]
         else
            @memory[@memory[@mem_ptr + 3]]
         end

    # Opcode
    opcode = @memory[@mem_ptr] % 100

    # Execute
    case opcode

    # ADD
    when 1
      @memory[@memory[@mem_ptr + 3]] = p1 + p2
      @mem_ptr += 4

    # MUL
    when 2
      @memory[@memory[@mem_ptr + 3]] = p1 * p2
      @mem_ptr += 4

    # INP
    when 3
      if @input_queue.empty?
        @state = "IDLE"
      else
        @memory[@memory[@mem_ptr + 1]] = @input_queue.shift
        @mem_ptr += 2
        @state = "RUNNING"
      end

    # OUT
    when 4
      @output_queue << @memory[@memory[@mem_ptr + 1]]
      @mem_ptr += 2

    # JEZ
    when 5
      @mem_ptr = p1 == 0 ? @mem_ptr + 3 : p2

    # JNZ
    when 6
      @mem_ptr = p1 == 0 ? p2 : @mem_ptr + 3

    # LST
    when 7
      @memory[@memory[@mem_ptr + 3]] = p1 < p2 ? 1 :0
      @mem_ptr += 4

    # EQL
    when 8
      @memory[@memory[@mem_ptr + 3]] = p1 == p2 ? 1 : 0
      @mem_ptr += 4

    else
      @state = "CRASH"
      @error_log = "Invalid opcode: #{opcode} - " \
        "INST_PTR: #{@mem_ptr}:#{@memory[@mem_ptr]} - " \
        "CYCLE #{cycle}"
    end
  end

  def state
    @state
  end

  def error_log
    @error_log
  end
end

