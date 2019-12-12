# encoding: utf-8

# To be updated in accordance with growing specifications.
class IntcodeComputer

  attr_reader :state, :error_log

  STATE = {
    :ready => "READY",
    :running => "RUNNING",
    :idle => "IDLE",
    :crash => "CRASH",
    :done => "DONE"
  }

  def initialize(program)
    self.program=(program)
    reset
  end

  def reset
    @memory = {}
    @memory.default = 0
    @program.each_index { |i| @memory[i] = @program[i] }
    @cycle = 0
    @mem_ptr = 0
    @rel_base = 0
    @state = STATE[:ready]
    @input_queue = []
    @output_queue = []
    @error_log = nil
  end

  def fetch_output
    @output_queue.shift
  end

  def send_input(i)
    case
    when i.is_a?(Integer)
      @input_queue << i

    when i.is_a?(Array)
      unless i.all? { |e| e.is_a?(Integer) }
        raise ArgumentError, "Non-Integer element given"
      end
      i.each { |e| @input_queue.push(e) }

    else
      raise ArgumentError, "Inputs must be Integers or Arrays of Integers"
    end
  end

  alias input send_input

  def run(inputs=[])
    raise ArgumentError, "Input was not an Array" unless inputs.is_a?(Array)
    inputs.each { |i| send_input(i) }

    @state = STATE[:running]
    step while @state == STATE[:running]

    return @state
  end

  def step
    @cycle += 1

    # HLT
    if @memory[@mem_ptr] % 100 == 99
      @state = STATE[:done]
      return
    end

    # Decode
    # Parameter 1
    unless @memory[@mem_ptr + 1].nil?
      p1 = decode_parameter(@mem_ptr + 1, @memory[@mem_ptr] / 100 % 10)
    end
    # Parameter 2
    unless @memory[@mem_ptr + 2].nil?
      p2 = decode_parameter(@mem_ptr + 2, @memory[@mem_ptr] / 1_000 % 10)
    end

    # Parameter 3
    unless @memory[@mem_ptr + 3].nil?
      p3 = decode_parameter(@mem_ptr + 3, @memory[@mem_ptr] / 10_000)
    end

    return if @state == STATE[:crash]

    # Opcode
    opcode = @memory[@mem_ptr] % 100

    # Execute
    case opcode

    # ADD
    when 1
      @memory[p3] = @memory[p1] + @memory[p2]
      @mem_ptr += 4

    # MUL
    when 2
      @memory[p3] = @memory[p1] * @memory[p2]
      @mem_ptr += 4

    # INP
    when 3
      if @input_queue.empty?
        @state = STATE[:idle]
      else
        @memory[p1] = @input_queue.shift
        @mem_ptr += 2
        @state = STATE[:running]
      end

    # OUT
    when 4
      @output_queue << @memory[p1]
      @mem_ptr += 2

    # JEZ
    when 5
      @mem_ptr = @memory[p1] == 0 ? @mem_ptr + 3 : @memory[p2]

    # JNZ
    when 6
      @mem_ptr = @memory[p1] == 0 ? @memory[p2] : @mem_ptr + 3

    # LST
    when 7
      @memory[p3] = @memory[p1] < @memory[p2] ? 1 : 0
      @mem_ptr += 4

    # EQL
    when 8
      @memory[p3] = @memory[p1] == @memory[p2] ? 1 : 0
      @mem_ptr += 4

    # ARB
    when 9
      @rel_base += @memory[p1]
      @mem_ptr += 2

    else
      @state = STATE[:crash]
      @error_log = "Invalid opcode: #{opcode} - " \
        "INST_PTR: #{@mem_ptr}:#{@memory[@mem_ptr]} - " \
        "CYCLE #{@cycle}"
    end
  end

  def ready?
    @state == STATE[:ready]
  end

  def running?
    @state == STATE[:running]
  end

  def idle?
    @state == STATE[:idle]
  end

  def crash?
    @state == STATE[:crash]
  end

  def done?
    @state == STATE[:done]
  end

  def view_output
    @output_queue.dup
  end

  def program=(program)
    if @state == STATE[:idle] || @state == STATE[:running]
      raise "Program cannot be changed while computation is in progress."
    end

    unless program.is_a?(Array)
      raise ArgumentError, "Program was not an Array"
    end

    unless program.all? { |i| i.is_a?(Integer) }
      raise ArgumentError, "Program Array contained non-integer elements"
    end

    @program = program.dup
  end

  private

  def decode_parameter(p, m)
    case m
    when 0 then @memory[p]
    when 1 then p
    when 2 then @rel_base + @memory[p]
    else
      @state = STATE[:crash]
      @error_log = "Invalid parameter mode: #{m} - " \
        "INST_PTR: #{@mem_ptr}:#{@memory[@mem_ptr]} - " \
        "CYCLE #{@cycle}"
    end
  end
end

