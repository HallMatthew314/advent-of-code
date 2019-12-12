# encoding: utf-8

class Graph
  class Vertex

    attr_accessor :color, :dist, :pred, :discovery, :finish, :disc
    attr_reader :id

    def initialize(key)
      @id = key
      @connections = {}
      @color = 'white'
      @dist = 100000000000
    end

    def add_neighbor(nbr, weight=0)
      @connections[nbr] = weight
    end

    def connections
      @connections.keys.dup
    end

    def weight(nbr)
      @connections[nbr]
    end
  end
end

