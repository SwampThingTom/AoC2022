#!/usr/bin/env node

// Beacon Exclusion Zone
// https://adventofcode.com/2022/day/15

const fs = require('fs');
const path = require('path');

const DEBUG = false;
const debugLog = ((text) => { if (DEBUG) console.log(text); });

// Part 1: Count the cells in the given row that can not have beacons.
function countCellsWithoutBeacons(sensors, beacons, row) {
    const covered_ranges = sortRanges(findSensorIntersectionsForRow(sensors, row));
    const covered_count = sumRangeCounts(covered_ranges);
    const target_beacons = beacons.filter((b) => b.by == row);
    return covered_count - target_beacons.length;
}

// Sums the number of unique coordinates for an array of ranges.
// Ranges must be sorted by their start points.
function sumRangeCounts(ranges) {
    if (ranges.length == 0) {
        console.log('No ranges to count. Are you using the correct row value for the input?');
        return 0;
    }

    let count = 0;
    let [start, end] = ranges[0];

    for (let i = 1; i < ranges.length; i++) {
        const [range_start, range_end] = ranges[i]; 
        if (range_start > end) {
            count += end - start + 1;
            start = range_start;
            end = range_end;
        } else if (range_end > end) {
            end = range_end;
        }
    }

    count += end - start + 1;
    return count;
}

// Part 2: Find the single beacon that is not covered by the sensors.
function findTuningFrequency(sensors) {
    const MAX_ROW = 4_000_000
    for (let row = 0; row <= MAX_ROW; row++) {
        const covered_ranges = sortRanges(findSensorIntersectionsForRow(sensors, row));
        let col = findUncoveredColumn(covered_ranges)
        if (col != null) {
            return col * MAX_ROW + row;
        }
    }
    return null;
}

// Find the first column that isn't covered in the ranges.
// Ranges must be sorted by their start points.
function findUncoveredColumn(ranges, max_row) {
    let [_, end] = ranges[0];
    for (let i = 1; i < ranges.length; i++) {
        const [range_start, range_end] = ranges[i]; 
        if (range_start > end) {
            return range_start - 1;
        } else if (range_end > end) {
            end = range_end;
        }
        if (end > max_row)
            return null;
    }
    return null;
}

// Returns an array of ranges that are covered by the sensors on the given row.
// A range is an array where item 0 is the starting x coordinate and item 1 is the ending x coordinate.
function findSensorIntersectionsForRow(sensors, row) {
    return sensors
        .filter((s) => Math.abs(row - s.sy) < s.distance)
        .map((s) => { 
            let dy = Math.abs(row - s.sy);
            return [s.sx - (s.distance - dy), s.sx + (s.distance - dy)];
        });
}

// Sorts an array of ranges by their starting coordinate.
function sortRanges(ranges) {
    return ranges.sort((r1, r2) => { 
        if (r1[0] == r2[0]) return 0;
        return (r1[0] < r2[0]) ? -1 : 1;
    });
}

// Returns the manhattan distance between (x1, y1) and (x2, y2).
function distance(x1, y1, x2, y2) {
    return Math.abs(x1 - x2) + Math.abs(y1 - y2);
}

// Returns a sensor object for an input string.
function parseSensor(s) {
    const m = /x=(-?\d+), y=(-?\d+).*?x=(-?\d+), y=(-?\d+)/g.exec(s);
    const sx = parseInt(m[1]); const sy = parseInt(m[2]);
    const bx = parseInt(m[3]); const by = parseInt(m[4]);
    return {sx: sx, sy: sy, bx: bx, by: by, distance: distance(sx, sy, bx, by)};
}

// Returns a map of unique beacons found in an array of sensors.
function uniqueBeacons(sensors) {
    // Can't store JS objects in a `Set`.
    // So we'll store them in a `Map` to get unique values
    // and then convert the values back to an array.
    const beacons = new Map();
    sensors.forEach((s) => {
        const beacon = { bx: s.bx, by: s.by };
        beacons.set(JSON.stringify(beacon), beacon);
    });
    return Array.from(beacons.values());
}

const sensors = fs
    .readFileSync(path.join(__dirname, 'input.txt'), 'utf8')
    .toString()
    .trim()
    .split(("\n"))
    .map(parseSensor);

const beacons = uniqueBeacons(sensors);

debugLog(sensors);
debugLog(beacons);

const part1 = countCellsWithoutBeacons(sensors, beacons, 2_000_000);
console.log(`Part 1: ${part1}`);

const part2 = findTuningFrequency(sensors);
console.log(`Part 2: ${part2}`);
