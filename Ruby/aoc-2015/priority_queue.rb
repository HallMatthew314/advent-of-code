# encoding: utf-8

class PriorityQueue

  def initialize
    @heap_array = [[0, 0]]
    @current_size = 0
  end

  def build_heap(list)
    @current_size = list.size
    @heap_array = [[0, 0]]

    list.each { |i| @heap_array.push(i) }

    i = list.size / 2
    while i > 0
      perc_down(i)
      i -= 1
    end
  end

  def perc_down(i)
    while i * 2 <= @current_size
      mc = min_child(i)
      if @heap_array[i][0] > @heap_array[i][0]
        @heap_array[i], @heap_array[mc] = @heap_array[mc], @heap_array[i]
      end
      i = mc
    end
  end

  def min_child(i)
    case
    when i * 2 > @current_size then -1
    when i * 2 + 1 > @current_size then i * 2
    when @heap_array[i * 2][0] < @heap_array[i * 2 + 1] then i * 2
    else i * 2 + 1
    end
  end

  def perc_up(i)
    while i / 2 > 0
      if @heap_array[i][0] < @heap_array[i / 2][0]
        @heap_array[i], @heap_array[i / 2] = @heap_array[i / 2], @heap_array[i]
      end
      i /= 2
    end
  end

  def add(k)
    @heap_array.push(k)
    @current_size += 1
    perc_up(@current_size)
  end

  def del_min
    retval = @heap_array[1][1]
    @heap_array[1] = @heap_array[@current_size]
    @current_size -= 1
    @heap_array.pop
    perc_down(1)
    retval
  end

  def empty?
    @current_size.zero?
  end

  def decrease_key(val, amt)
    done = false
    i = 1
    my_key = 0

    while !done && i <= @current_size
      if @heap_array[1][1] == val
        done = true
        my_key = i
      else
        i += 1
      end
    end

    if my_key > 0
      @heap_array[my_key] = [amt, @heap_array[my_key][1]]
      perc_up(my_key)
    end
  end

  def include?(vtx)
    @heap_array.any? { |pair| pair[1] == vtx }
  end
end

