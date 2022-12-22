#!/usr/bin/env python3

# Monkey Map
# https://adventofcode.com/2022/day/22

import re
from typing import Tuple

class MonkeyMap:

    right = 0
    down = 1
    left = 2
    up = 3

    def __init__(self, map: dict):
        self.map = map
        self.pos = self.min_x_point(map, 1)
        self.facing = self.right
        self.offsets = [(1, 0), (0, 1), (-1, 0), (0, -1)]

    def password(self):
        return 1_000 * self.pos[1] + 4 * self.pos[0] + self.facing

    def move(self, distance: int):
        offset = self.offsets[self.facing]
        while distance > 0:
            pos, cell = self.next_pos(offset)
            if cell == '#':
                return
            self.pos = pos
            distance -= 1
    
    def next_pos(self, offset: Tuple[int, int]):
        pos = (self.pos[0] + offset[0], self.pos[1] + offset[1])
        cell = self.map.get(pos)
        if cell is None:
            if self.facing == self.right:
                pos = self.min_x_point(map, pos[1])
            elif self.facing == self.left:
                pos = self.max_x_point(map, pos[1])
            elif self.facing == self.down:
                pos = self.min_y_point(map, pos[0])
            else:
                pos = self.max_y_point(map, pos[0])
            cell = self.map.get(pos)
            assert cell is not None
        return pos, cell

    def turn(self, direction: str):
        if direction == '':
            return
        offset = 1 if direction == 'R' else -1
        self.facing = (self.facing + offset) % 4

    @staticmethod
    def min_x_point(map: dict, y: int):
        return (min(p[0] for p in map.keys() if p[1] == y), y)

    @staticmethod
    def max_x_point(map: dict, y: int):
        return (max(p[0] for p in map.keys() if p[1] == y), y)

    @staticmethod
    def min_y_point(map: dict, x: int):
        return (x, min(p[1] for p in map.keys() if p[0] == x))

    @staticmethod
    def max_y_point(map: dict, x: int):
        return (x, max(p[1] for p in map.keys() if p[0] == x))

def read_file():
    file = open('input.txt')
    return list(file.readlines())

def parse(lines: list):
    map = parse_map(lines[:-2])
    return map, lines[-1:][0]

def parse_map(lines: list):
    return { (x+1, y+1): c for y, l in enumerate(lines) for x, c in enumerate(l) if c == '.' or c == '#' }

def parse_moves(line: str):
    matches = re.findall(r"([0-9]+)([RL])?", line)
    return [(int(d), c) for d, c in matches]

def debug_print(msg: str, should_print: bool = False):
    if should_print:
        print(msg)

map, move_str = parse(read_file())
monkey_map = MonkeyMap(map)
moves = parse_moves(move_str)

for move in moves:
    monkey_map.move(move[0])
    monkey_map.turn(move[1])
    debug_print(f'After {move}, now at {monkey_map.pos} facing {monkey_map.facing}')

print(f'Part 1: {monkey_map.password()}')