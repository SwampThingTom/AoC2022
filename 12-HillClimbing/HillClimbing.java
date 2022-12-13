// Hill Climbing Algorithm
// https://adventofcode.com/2022/day/12

import java.io.FileReader;
import java.lang.Math;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;

class HillClimbing {

    class GridCell {
        private int row;
        private int col;

        public GridCell(int row, int col) {
            this.row = row;
            this.col = col;
        }

        public int getRow() { return row; }
        public int getCol() { return col; }

        public GridCell above() { return new GridCell(row-1, col); }
        public GridCell below() { return new GridCell(row+1, col); }
        public GridCell left()  { return new GridCell(row, col-1); }
        public GridCell right() { return new GridCell(row, col+1); }

        public List<GridCell> neighbors() {
            List<GridCell> neighbors = new ArrayList<>();
            neighbors.add(above());
            neighbors.add(below());
            neighbors.add(left());
            neighbors.add(right());
            return neighbors;
        }

        public String toString() {
            return "(" + row + "," + col + ")";
        }

        @Override
        public boolean equals(Object o) {
            if (o == this) {
                return true;
            }
            if (!(o instanceof GridCell)) {
                return false;
            }
            GridCell other = (GridCell)o;
            return this.row == other.row && this.col == other.col;
        }

        @Override
        public int hashCode() {
            return row * 31 + col * 17;
        }
    }

    class GridMap {
        private int[][] map;
        private int height = 0;
        private int width = 0;
        private GridCell start;
        private GridCell end;

        public GridMap(ArrayList<String> lines) {
            if (lines.isEmpty()) {
                return;
            }
            this.height = lines.size();
            this.width = lines.get(0).length();
            this.map = new int[height][width];
            for (int row = 0; row < height; row++) {
                for (int col = 0; col < width; col++) {
                    char cellChar = lines.get(row).charAt(col);
                    char cellElevation = cellChar;
                    if (cellChar == 'S') {
                        cellElevation = 'a';
                        start = new GridCell(row, col);
                    } else if (cellChar == 'E') {
                        cellElevation = 'z';
                        end = new GridCell(row, col);
                    }
                    this.map[row][col] = cellElevation;
                }
            }
        }

        public int getHeight() { return height; }
        public int getWidth() { return width; }
        public GridCell getStart() { return start; }
        public GridCell getEnd() { return end; }

        public boolean isValid(GridCell cell) {
            return cell.getRow() >= 0 && cell.getRow() < height &&
                   cell.getCol() >= 0 && cell.getCol() < width;
        }
        
        public int elevationAt(GridCell cell) {
            if (!isValid(cell)) {
                return -1;
            }
            return map[cell.getRow()][cell.getCol()];
        }

        public List<GridCell> getNeighbors(GridCell cell) {
            List<GridCell> neighbors = new ArrayList<>();
            int maxElevationForNeighbor = elevationAt(cell) + 1;
            for (GridCell neighborCell : cell.neighbors()) {
                if (isValid(neighborCell) && elevationAt(neighborCell) <= maxElevationForNeighbor) {
                    neighbors.add(neighborCell);
                }
            }
            return neighbors;
        }

        public List<GridCell> getPossibleStartCells() {
            List<GridCell> startCells = new ArrayList<>();
            for (int row = 0; row < height; row++) {
                for (int col = 0; col < width; col++) {
                    if (map[row][col] == 'a') {
                        startCells.add(new GridCell(row, col));
                    }
                }
            }
            return startCells;
        }

        public void print() {
            for (int row = 0; row < getHeight(); row++) {
                for (int col = 0; col < getWidth(); col++) {
                    System.out.print(String.format("%d ", elevationAt(new GridCell(row, col))));
                }
                System.out.println();
            }
        }
    }

    class PathNode implements Comparable<PathNode> {
        private GridCell cell;
        private PathNode previous = null;
        private int cost = Integer.MAX_VALUE;

        public PathNode(GridCell cell) {
            this.cell = cell;
        }

        public GridCell getCell() { return cell; }
        public PathNode getPrevious() { return previous; }
        public int getCost() { return cost; }

        public void setPrevious(PathNode node) { previous = node; }
        public void setCost(int newCost) { cost = newCost; }

        public String toString() {
            return "{PathNode " + cell + " Cost: " + getCost() + "}";
        }

        @Override
        public int compareTo(PathNode other) {
            if (this.getCost() > other.getCost()) { return 1; }
            if (this.getCost() < other.getCost()) { return -1; }
            return 0;
        }

        @Override
        public boolean equals(Object o) {
            if (o == this) {
                return true;
            }
            if (!(o instanceof PathNode)) {
                return false;
            }
            PathNode other = (PathNode)o;
            return this.cell == other.cell;
        }

        @Override
        public int hashCode() {
            return cell.hashCode();
        }
    }

    class PathNodeMap {
        private int height = 0;
        private int width = 0;
        private PathNode[][] nodes;
        private Set<PathNode> unexplored;

        public PathNodeMap(GridMap map) {
            height = map.getHeight();
            width = map.getWidth();
            nodes = new PathNode[height][width];
            unexplored = new HashSet<>();

            for (int row = 0; row < height; row++) {
                for (int col = 0; col < width; col++) {
                    PathNode node = new PathNode(new GridCell(row, col));
                    nodes[row][col] = node;
                    unexplored.add(node);
                }
            }
        }

        public PathNode nodeAt(GridCell cell) {
            return nodes[cell.getRow()][cell.getCol()];
        }

        public boolean hasUnexplored() {
            return !unexplored.isEmpty();
        }

        public boolean isUnexploredCell(GridCell cell) {
            return unexplored.contains(nodeAt(cell));
        }

        public PathNode getLowestCostUnexplored() {
            PathNode node = Collections.min(unexplored);
            unexplored.remove(node);
            return node;
        }

        public void print() {
            for (int row = 0; row < height; row++) {
                for (int col = 0; col < width; col++) {
                    System.out.print(String.format("%d ", nodes[row][col].getCost()));
                }
                System.out.println();
            }
        }
    }

    int findLengthOfPath(GridMap map, GridCell start, GridCell end) {
        PathNodeMap pathNodeMap = new PathNodeMap(map);
        pathNodeMap.nodeAt(start).setCost(0);

        while (pathNodeMap.hasUnexplored()) {
            PathNode node = pathNodeMap.getLowestCostUnexplored();

            if (node.getCell().equals(end)) {
                return node.getCost();
            }

            List<GridCell> neighbors = map.getNeighbors(node.getCell());
            for (GridCell neighborCell : neighbors) {
                if (!pathNodeMap.isUnexploredCell(neighborCell)) {
                    continue;
                }

                PathNode neighborNode = pathNodeMap.nodeAt(neighborCell);
                int cost = node.getCost() + 1;
                if (cost < neighborNode.getCost()) {
                    neighborNode.setCost(cost);
                    neighborNode.setPrevious(node);
                }
            }
        }

        return -1;
    }

    int findLengthOfPath(GridMap map) {
        return findLengthOfPath(map, map.getStart(), map.getEnd());
    }

    int findShortestPathFromBottom(GridMap map) {
        int shortestPath = Integer.MAX_VALUE;
        List<GridCell> possibleStartCells = map.getPossibleStartCells();
        for (GridCell startCell : possibleStartCells) {
            int pathLength = findLengthOfPath(map, startCell, map.getEnd());
            if (pathLength >= 0 && pathLength < shortestPath) {
                shortestPath = pathLength;
            }
        }
        return shortestPath;
    } 

    ArrayList<String> readInput() {
        ArrayList<String> lines = new ArrayList<>();
        try (Scanner file = new Scanner((new FileReader("input.txt")))) {
            while (file.hasNext()) {
                lines.add(file.nextLine());
            }
        }
        catch (Exception e) {
            System.err.println(e);
        }
        return lines;
    }

    void solve() {
        ArrayList<String> input = readInput();
        GridMap map = new GridMap(input);

        int lengthOfPath = findLengthOfPath(map);
        System.out.println("Part 1: " + lengthOfPath);

        int shortestPath = findShortestPathFromBottom(map);
        System.out.println("Part 2: " + shortestPath);
    }

    public static void main(String[] args) {
        HillClimbing aoc = new HillClimbing();
        aoc.solve();
    }
}