#!/usr/bin/env python3

# Converts the input for Advent of Code 2022 Day 2 into 6502 assembler directives.
# https://adventofcode.com/2022/day/2

def format(rps):
    return f"\t!byte '{rps[0]}', '{rps[2]}'\n"

input_file = open("input.txt")
games = [line for line in input_file.readlines()]
input_file.close()

output_file = open("input.asm", "w")
output_file.write("strategy_data\n")
for rps in games:
    output_file.write(format(rps))
output_file.write("\t!byte 0\n")
