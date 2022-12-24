// Blizzard Basin
// https://adventofcode.com/2022/day/24

use std::collections::HashMap;
use std::collections::HashSet;
use std::collections::VecDeque;
use std::convert::TryInto;

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Point(i32, i32);

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
struct Blizzard {
    pos: Point,
    ch: char,
    offset: Point
}

#[derive(Debug)]
struct Valley {
    width: i32,
    height: i32,
    start: Point,
    end: Point,
    walls: HashSet<Point>,
    blizzards: Vec<Blizzard>,
}

impl Valley {
    fn pretty_print(&self) {
        let mut blizzard_points = HashMap::new();
        for b in &self.blizzards {
            blizzard_points.insert(b.pos, b.ch);
        }
        for y in 0 .. self.height {
            for x in 0 .. self.width {
                let p = Point(x, y);
                if self.walls.contains(&p) {
                    print!("#")
                } else if let Some(ch) = blizzard_points.get(&p) {
                    print!("{}", ch)
                } else {
                    print!(".")
                }
            }
            println!("")
        }
    }

    // Returns the minimum end time to traverse the valley from the start point to the end point.
    fn find_shortest_path_time(&self, start: Point, end: Point, start_time: i32) -> i32 {
        let mut visited: HashSet<(i32, Point)> = HashSet::new();
        let mut open_list = VecDeque::from([(start_time, start)]);
        while !open_list.is_empty() {
            let (mut time, cell) = open_list.pop_front().unwrap();
            time += 1;
            let map = self.map_at_time(time);
            for next_cell in self.neighbors(cell, map) {
                if !visited.contains(&(time, next_cell)) {
                    if next_cell == end {
                        return time
                    }
                    visited.insert((time, next_cell));
                    open_list.push_back((time, next_cell))
                }
            }
        }
        -1
    }

    // Returns a set of points that are occupied by walls or blizzards at the given time.
    fn map_at_time(&self, time: i32) -> HashSet<Point> {
        let b_width = self.width - 2;
        let b_height = self.height - 2;
        let mut points: HashSet<Point> = self.walls.clone();
        for b in &self.blizzards {
            let x = modulo(b.pos.0 - 1 + b.offset.0 * time, b_width) + 1;
            let y = modulo(b.pos.1 - 1 + b.offset.1 * time, b_height) + 1;
            assert!(x > 0 && x < self.width - 1, "x out of range");
            assert!(y > 0 && y < self.height - 1, "y out of range");
            points.insert(Point(x, y));
        }
        points
    }

    // Returns a list of points that neighbor the giveen cell and are not blocked.
    fn neighbors(&self, cell: Point, map: HashSet<Point>) -> Vec<Point> {
        let mut moves: Vec<Point> = Vec::new();
        for (dx, dy) in [(0, 1), (1, 0), (-1, 0), (0, -1), (0, 0)] {
            let x = cell.0 + dx;
            let y = cell.1 + dy;
            if x < 0 || x >= self.width || y < 0 || y >= self.height {
                continue
            }
            let p = Point(x, y);
            if !map.contains(&p) {
                moves.push(p.clone())
            }
        }
        moves
    }
}

fn modulo(x: i32, y: i32) -> i32 {
    ((x % y) + y) % y
}

fn make_valley(lines: Vec<String>) -> Valley {
    let width = lines[0].len().try_into().unwrap();
    let height = lines.len().try_into().unwrap();
    let start = Point(1, 0);
    let end = Point(width - 2, height - 1);
    Valley {
        width,
        height,
        start,
        end,
        walls: make_walls(width, height, start, end),
        blizzards: make_blizzards(lines),
    }
}

fn make_walls(width: i32, height: i32, start: Point, end: Point) -> HashSet<Point> {
    let mut walls = HashSet::new();
    for x in 0 .. width {
        if x != start.0 {
            walls.insert(Point(x, 0));
        }
        if x != end.0 {
            walls.insert(Point(x, height - 1));
        }
    }
    for y in 0 .. height {
        walls.insert(Point(0, y));
        walls.insert(Point(width - 1, y));
    }
    walls
}

fn make_blizzards(lines: Vec<String>) -> Vec<Blizzard> {
    let offsets = HashMap::from([
        ('^', Point(0, -1)), ('v', Point(0, 1)), ('>', Point(1, 0)), ('<', Point(-1, 0))
    ]);
    let mut blizzards = Vec::new();
    for (y, row) in lines.iter().enumerate() {
        for (x, ch) in row.chars().enumerate() {
            if let Some(offset) = offsets.get(&ch) {
                let pos = Point(x.try_into().unwrap(), y.try_into().unwrap());
                let b = Blizzard {
                    pos,
                    ch,
                    offset: *offset
                };
                blizzards.push(b)
            }
        }
    }
    blizzards
}

fn read_input() -> Vec<String> {
    std::fs::read_to_string("input.txt")
        .unwrap()
        .lines()
        .map(str::to_string)
        .collect()
}

fn main() {
    let lines = read_input();
    let valley = make_valley(lines);

    let part1 = valley.find_shortest_path_time(valley.start, valley.end, 0);
    println!("Part 1: {}", part1);
}
