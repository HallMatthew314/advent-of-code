# encoding: utf-8

require_relative "graph_vertex.rb"

class Graph

  attr_accessor :vertices

  def initialize
    @vertices = {}
    @num_vertices = 0
  end

  def add_vertex(key)
    @num_vertices += 1
    new_v = Vertex.new(key)
    @vertices[key] = new_v
    new_v
  end

  def get_vertex(key)
    @vertices[key]
  end

  def include?(key)
    @vertices.include?(key)
  end

  def add_edge(f, t, cost=0)
    nv = add_vertex(f) unless include?(f)
    nv = add_vertex(t) unless include?(t)

    @vertices[f].add_neighbor(@vertices[t], cost)
  end
end

