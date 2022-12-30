#!/usr/bin/env python3

# Converts the input for Advent of Code 2022 Day 5 by removing the starting configuration.
# The resulting file will only have the list of moves.
# Note that this means the code won't work with arbitrary input without updating the source.
# https://adventofcode.com/2022/day/5

input_file = open("input.txt")
lines = [line.strip() for line in input_file.readlines()]
input_file.close()

moves_idx = lines.index('')

output_file = open("input-moves.txt", "w")
for line in lines[moves_idx+1:]:
    output_file.write(line)
    output_file.write('\n')
