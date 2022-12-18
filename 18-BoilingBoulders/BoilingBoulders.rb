#!/usr/bin/env ruby

# Boiling Boulders
# https://adventofcode.com/2022/day/18

require "set"

class Cube
    attr_reader :x
    attr_reader :y
    attr_reader :z

    def initialize(x, y, z)
        @x = x
        @y = y
        @z = z
    end

    def neighbors
        [
            Cube.new(@x-1, @y, @z), Cube.new(@x+1, @y, @z),
            Cube.new(@x, @y-1, @z), Cube.new(@x, @y+1, @z),
            Cube.new(@x, @y, @z-1), Cube.new(@x, @y, @z+1)
        ]
    end

    def bounded_neighbors(x_bounds, y_bounds, z_bounds)
        neighbors.select { |c|
            x_bounds.include?(c.x) && y_bounds.include?(c.y) && z_bounds.include?(c.z) 
        }
    end

    def eql?(other)
        self.class == other.class && @x == other.x && @y == other.y && @z == other.z
    end

    def ==(other)
        self.eql?(other)
    end

    def hash
        [self.class, @x, @y, @z].hash
    end

    def to_s
        "(#{@x},#{@y},#{@z})"
    end
end

def parse_cube(str)
    Cube.new *(str.split(",").map { |s| s.to_i })
end

def surface_area(cubes)
    cubes.reduce(0) { |sum, cube| 
        adjacent = 6 - cube.neighbors.count { |n| cubes.include? n }
        sum + adjacent
    }
end

def bounding_ranges(cubes)
    first = cubes.first()
    min_x = max_x = first.x
    min_y = max_y = first.y
    min_z = max_z = first.z
    cubes.each do |c|
        min_x = c.x if c.x < min_x
        max_x = c.x if c.x > max_x
        min_y = c.y if c.y < min_y
        max_y = c.y if c.y > max_y
        min_z = c.z if c.z < min_z
        max_z = c.z if c.z > max_z
    end
    [((min_x - 1)..(max_x + 1)), ((min_y - 1)..(max_y + 1)), ((min_z - 1)..(max_z + 1))]
end

def external_surface_area(cubes)
    external_surfaces = 0
    (x_range, y_range, z_range) = bounding_ranges(cubes)

    open_cells = [Cube.new(x_range.first, y_range.first, z_range.first)]
    closed_cells = open_cells.to_set

    while open_cells.any?
        cell = open_cells.pop
        cell.bounded_neighbors(x_range, y_range, z_range).each do |neighbor|
            next if closed_cells.include? neighbor
            if cubes.include? neighbor
                external_surfaces += 1
            else
                closed_cells << neighbor
                open_cells << neighbor
            end
        end
    end
    external_surfaces
end

cubes = File.read("input.txt").split.map { |line| parse_cube(line) }.to_set
puts "Part 1: #{surface_area cubes}"
puts "Part 2: #{external_surface_area cubes}"
