#!/usr/bin/env groovy

// Pyroclastic Flow
// https://adventofcode.com/2022/day/17

import groovy.transform.EqualsAndHashCode
import groovy.transform.ToString

class PyroclasticFlow {

    static final DIR_LEFT = '<'
    static final DIR_RIGHT = '>'
    static final DIR_DOWN = 'v'

    @EqualsAndHashCode
    class Point {
        int x
        int y

        Point(int x, int y) {
            this.x = x
            this.y = y
        }

        Point moved(String dir) {
            switch (dir) {
                case DIR_LEFT:  return new Point(x-1, y)
                case DIR_RIGHT: return new Point(x+1, y)
                case DIR_DOWN:  return new Point(x, y-1)
            }
            return null
        }

        String toString() { "($x,$y)" }
    }

    class Rock {
        int shape
        Point pos

        def blocks(Point pos = null) {
            if (pos == null) { pos = this.pos }
            switch (shape) {
                case 0:
                    return [
                        new Point(pos.x, pos.y), new Point(pos.x+1, pos.y), 
                        new Point(pos.x+2, pos.y), new Point(pos.x+3, pos.y)
                    ]
                case 1:
                    return [
                        new Point(pos.x+1, pos.y), new Point(pos.x+1, pos.y+2),
                        new Point(pos.x, pos.y+1), new Point(pos.x+1, pos.y+1),
                        new Point(pos.x+2, pos.y+1)
                    ]
                case 2:
                    return [
                        new Point(pos.x, pos.y), new Point(pos.x+1, pos.y), 
                        new Point(pos.x+2, pos.y), new Point(pos.x+2, pos.y+1), 
                        new Point(pos.x+2, pos.y+2)
                    ]
                case 3:
                    return [
                        new Point(pos.x, pos.y), new Point(pos.x, pos.y+1),
                        new Point(pos.x, pos.y+2), new Point(pos.x, pos.y+3)
                    ]
                case 4:
                    return [
                        new Point(pos.x, pos.y), new Point(pos.x+1, pos.y),
                        new Point(pos.x, pos.y+1), new Point(pos.x+1, pos.y+1)
                    ]
                default:
                    return []
            }
        }

        void move(String dir) {
            pos = pos.moved dir
        }

        Rock(int shape, Point pos) {
            this.shape = shape
            this.pos = pos
        }
    }

    class Chamber {
        Set<Point> blocks = []
        int height() { (blocks.max { it.y }?.y ?: 0) + 1 }
        Rock rock = new Rock(0, new Point(2, 3))
        long numRocks = 0

        void addRock() {
            blocks.addAll rock.blocks()
            numRocks++
            rock = new Rock((int)(numRocks % 5), new Point(2, height()+3))
        }

        boolean canFit(String dir) {
            Point newPos = rock.pos.moved dir
            for (Point block : rock.blocks(newPos)) {
                if (blocks.contains block) {
                    return false
                }
                if (block.y < 0 || block.x < 0 || block.x > 6) {
                    return false
                }
            }
            true
        }

        List<Integer> relativeColumnHeights() {
            def height = this.height()
            (0..6).collect { x -> height - (blocks.findAll { it.x == x }.max { it.y }?.y ?: 0) }
        }
    }

    @EqualsAndHashCode
    class GameState {
        int moveIndex
        int rockShape
        List<Integer> columnHeights
    }

    @EqualsAndHashCode
    @ToString
    class GameResults {
        long numRocks
        int height
    }

    def gameHash(int moveIndex, Chamber chamber) {
        def state = new GameState(
            moveIndex: moveIndex,
            rockShape: chamber.rock.shape,
            columnHeights: chamber.relativeColumnHeights())
        state.hashCode()
    }

    def readInput() {
        File file = new File("input.txt")
        file.text.trim()
    }

    void debugPrint(String str) {
        if (false) { println str }
    }

    def solve(long numRocksToAdd, boolean detectCycles = false) {
        def moves = readInput()
        def moveIndex = 0
        def chamber = new Chamber()

        def foundCycle = !detectCycles
        def cyclesHeight = 0
        def gamesSeen = new HashMap<Integer, GameResults>()

        while (chamber.numRocks < numRocksToAdd) {
            def dir = moves[moveIndex++]
            moveIndex %= moves.length()
            
            if (chamber.canFit(dir)) {
                chamber.rock.move dir
                debugPrint "Moved $dir"
            }
            if (chamber.canFit(DIR_DOWN)) {
                chamber.rock.move DIR_DOWN
                debugPrint "Moved $DIR_DOWN"
            } else {
                chamber.addRock()
                debugPrint "Added rock $chamber.numRocks. New height is ${chamber.height()}"
            }

            if (!foundCycle) {
                def hash = gameHash(moveIndex, chamber)
                def prevResults = gamesSeen[hash]
                if (prevResults != null) {
                    foundCycle = true
                    
                    def rocksLeft = numRocksToAdd - chamber.numRocks
                    def rocksPerCycle = chamber.numRocks - prevResults.numRocks
                    def heightPerCycle = chamber.height() - prevResults.height
                    def numCycles = rocksLeft.intdiv(rocksPerCycle)
                    debugPrint "Skipping $numCycles cycles with $rocksPerCycle rocks and $heightPerCycle height."
                    
                    chamber.numRocks += numCycles * rocksPerCycle
                    cyclesHeight = numCycles * heightPerCycle
                } else {
                    gamesSeen[hash] = new GameResults(numRocks: chamber.numRocks, height: chamber.height())
                }
            }            
        }
        chamber.height() + cyclesHeight
    }

    static void main(String[] args) {
        def tetris = new PyroclasticFlow()
        println "Part 1: ${tetris.solve(2022)}"
        println "Part 2: ${tetris.solve(1_000_000_000_000, true)}"
    }
}
