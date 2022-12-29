#!/usr/bin/env python3

# Converts the input for Advent of Code 2022 Day 25 into a SAIL list.
# https://adventofcode.com/2022/day/25

output_file = open("input.sail", "w")
input_file = open("input.txt")

output_file.write('{\n')
for line in input_file.readlines():
    output_file.write(format(f'  "{line.strip()}",\n'))
output_file.write('}\n')
