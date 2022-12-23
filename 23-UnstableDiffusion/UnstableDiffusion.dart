#!/usr/bin/env dart run

// Unstable Diffusion
// https://adventofcode.com/2022/day/23

import 'dart:collection';
import 'dart:io';

enum Direction {
    north,
    south,
    west,
    east
}

class Point {
    final int x;
    final int y;

    Point(this.x, this.y);

    @override
    bool operator==(other) {
        if (other is! Point) {
            return false;
        }
        final otherPoint = (other as Point);
        return x == otherPoint.x && y == otherPoint.y;
    }

    @override
    int get hashCode => (x.hashCode * 9973) ^ y.hashCode;

    @override
    String toString() {
        return '(${this.x},${this.y})';
    }
}

class Grove {
    Set<Point> elves;
    
    final allNeighbors = [
        Point(-1, -1), Point(0, -1), Point(1, -1),
        Point(-1, 1), Point(0, 1), Point(1, 1),
        Point(-1, 0), Point(1, 0)
    ];

    final neighbors = {
        Direction.north: [Point(0, -1), Point(-1, -1), Point(1, -1)],
        Direction.south: [Point(0, 1), Point(-1, 1), Point(1, 1)],
        Direction.west:  [Point(-1, 0), Point(-1, -1), Point(-1, 1)],
        Direction.east:  [Point(1, 0), Point(1, -1), Point(1, 1)]
    };

    var startDirection = 0;

    Grove(this.elves);

    Point pointAtOffset(Point p, Point o) {
        return Point(p.x + o.x, p.y + o.y);
    }

    bool isElfAtOffset(Point elf, Point offset) {
        return elves.contains(pointAtOffset(elf, offset));
    }

    bool isElfNearby(Point elf) {
        for (final offset in this.allNeighbors) {
            if (this.isElfAtOffset(elf, offset)) {
                return true;
            }
        }
        return false;
    }

    Point? nextMove(Point elf) {
        for (final d in Direction.values) {
            final dir = Direction.values[(d.index + startDirection) % Direction.values.length];
            final offsetInDirections = neighbors[dir];
            if (offsetInDirections is! List<Point>) continue;
            if (!offsetInDirections.any((offset) => this.isElfAtOffset(elf, offset))) {
                return pointAtOffset(elf, offsetInDirections[0]);
            }
        }
        return null;
    }

    void moveElf(Point fromPoint, Point toPoint) {
        elves.remove(fromPoint);
        elves.add(toPoint);
    }

    // Runs one round of moves and returns true if any elf moved.
    bool runRound() {
        var didMove = false;
        var possibleMoves = HashMap<Point, Point>();
        var moveCounts = HashMap<Point, int>();
        for (final elf in this.elves) {
            if (!this.isElfNearby(elf)) {
                continue;
            }
            
            final move = this.nextMove(elf);
            if (move is! Point) continue;
            possibleMoves[elf] = move;
            moveCounts[move] = (moveCounts[move] ?? 0) + 1;
        }

        possibleMoves.forEach((elf, move) {
            if (moveCounts[move] == 1) {
                didMove = true;
                this.moveElf(elf, move);
            }
        });

        startDirection = (startDirection + 1) % Direction.values.length;
        return didMove;
    }

    int emptyTiles() {
        final b = this.bounds();
        final size = (b[1].x - b[0].x + 1) * (b[1].y - b[0].y + 1);
        return size - elves.length;
    }

    List<Point> bounds() {
        var minX = elves.first.x;
        var minY = elves.first.y;
        var maxX = minX;
        var maxY = minY;
        for (final p in elves) {
            if (p.x < minX) minX = p.x;
            else if (p.x > maxX) maxX = p.x;
            if (p.y < minY) minY = p.y;
            else if (p.y > maxY) maxY = p.y;
        }
        return [Point(minX, minY), Point(maxX, maxY)];
    }

    void prettyPrint() {
        final bounds = this.bounds();
        for (var y = bounds[0].y; y <= bounds[1].y; y++) {
            var line = '';
            for (var x = bounds[0].x; x <= bounds[1].x; x++) {
                line += elves.contains(Point(x, y)) ? '#' : '.';
            }
            debugPrint(line);
        }
    }
}

int solvePart1() {
    var grove = Grove(parseInput());
    grove.prettyPrint();
     
    for (var i = 0; i < 10; i++) {
        grove.runRound();
        debugPrint('\nAfter round $i');
        grove.prettyPrint();
    }

    return grove.emptyTiles();
}

int solvePart2() {
    var grove = Grove(parseInput());
    var moveNum = 1;
    while (grove.runRound()) {
        moveNum++;
    }
    return moveNum;
}

Set<Point> parseInput() {
    final lines = File('input.txt').readAsLinesSync();
    var map = Set<Point>();
    for (int y = 0; y < lines.length; y++) {
        final line = lines[y];
        for (int x = 0; x < line.length; x++) { 
            if (line[x] == '#') {
                map.add(Point(x, y));
            }
        }
    }; 
    return map;
}

void debugPrint(String msg, {bool show = false}) {
    if (show) print(msg);
}

main() {
    final part1 = solvePart1();
    print('Part 1: $part1');

    final part2 = solvePart2();
    print('Part 2: $part2');
}