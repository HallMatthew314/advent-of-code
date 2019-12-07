# encoding: utf-8

# Intcode Computer class following specifications of Day 5.
class IntcodeComputer5

  def initialize(program)
    @program = program
    unless @program.is_a?(Array)
      raise ArgumentError, "Program was not an Array"
    end
  end

  def run(input_queue=[])
    raise ArgumentError, "Input was not an Array" unless input_queue.is_a?(Array)

    code = @program.dup
    outputs = []
    inst_ptr = 0
    cycle = 0

    loop do
      cycle += 1
      # HLT
      return outputs if code[inst_ptr] % 100 == 99

      # Decode
      # Parameter 1
      p1 = if (code[inst_ptr] / 100) % 10 == 1
             code[inst_ptr + 1]
           else
             code[code[inst_ptr + 1]]
           end
      # Parameter 2
      p2 = if (code[inst_ptr] / 1_000) % 10 == 1
             code[inst_ptr + 2]
           else
             code[code[inst_ptr + 2]]
           end
      # Parameter 3
      p3 = if code[inst_ptr] / 10_000 == 1
             code[inst_ptr + 3]
           else
             code[code[inst_ptr + 3]]
           end
      # Opcode
      opcode = code[inst_ptr] % 100

      # Execute
      case opcode
      # ADD
      when 1
        code[code[inst_ptr + 3]] = p1 + p2
        inst_ptr += 4
      # MUL
      when 2
        code[code[inst_ptr + 3]] = p1 * p2
        inst_ptr += 4
      # INP
      when 3
        raise "Ran out of inputs" if input_queue.empty?
        code[code[inst_ptr + 1]] = input_queue.shift
        inst_ptr += 2
      # OUT
      when 4
        outputs << code[code[inst_ptr + 1]]
        inst_ptr += 2
      # JEZ
      when 5
        inst_ptr = p1 == 0 ? inst_ptr + 3 : p2
      # JNZ
      when 6
        inst_ptr = p1 == 0 ? p2 : inst_ptr + 3
      # LST
      when 7
        code[code[inst_ptr + 3]] = p1 < p2 ? 1 :0
        inst_ptr += 4
      # EQL
      when 8
        code[code[inst_ptr + 3]] = p1 == p2 ? 1 : 0
        inst_ptr += 4

      else 
        raise "Invalid opcode: #{opcode} - " \
              "INST_PTR: #{inst_ptr}:#{code[inst_ptr]} - " \
              "CYCLE #{cycle}"
      end
    end
  end
end
