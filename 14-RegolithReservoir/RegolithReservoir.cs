// Regolith Reservoir
// https://adventofcode.com/2022/day/14

using System;
using System.Collections.Generic;
using System.IO;

class RegolithReservoir
{
  struct Point
  {
    public int x { get; }
    public int y { get; }

    public Point(int x, int y)
    {
      this.x = x;
      this.y = y;
    }

    public override string ToString() => string.Format("{0},{1}", x, y);
  }

  class Grid
  {
    public HashSet<Point> points { get; }
    public int minX { get; }
    public int maxX { get; }
    public int maxY { get; }

    private Point startPoint = new Point(500, 0);

    public Grid(HashSet<Point> points, int minX, int maxX, int maxY)
    {
      this.points = points;
      this.minX = minX;
      this.maxX = maxX;
      this.maxY = maxY;
    }

    public int CountSand(bool hasFloor)
    {
      var count = 0;
      while (AddSand(startPoint, hasFloor))
        count++;
      return hasFloor ? count+1 : count;
    }

    private bool AddSand(Point point, bool hasFloor)
    {
      var nextPoints = new [] { 
        new Point(point.x, point.y + 1),
        new Point(point.x - 1, point.y + 1),
        new Point(point.x + 1, point.y + 1)
      };

      foreach (var next in nextPoints)
      {
        if (!hasFloor && (next.x < minX || next.x > maxX || next.y > maxY))
          return false;

        if (!points.Contains(next) && (!hasFloor || next.y < maxY + 2))
          return AddSand(next, hasFloor);
      }

      if (point.Equals(startPoint))
        return false;

      points.Add(point);
      return true;
    }

    public void Print()
    {
      for (int y = 0; y <= maxY; y++)
      {
        for (int x = minX; x <= maxX; x++)
        {
          var point = new Point(x, y);
          char cell = points.Contains(point) ? '#' : '.';
          Console.Write(cell);
        }
        Console.WriteLine();
      }
    }
  }

  Point ParsePoint(string pointString)
  {
    string[] xy = pointString.Split(',');
    var point = new Point(Int32.Parse(xy[0]), Int32.Parse(xy[1]));
    return point;
  }

  Grid MakeGrid()
  {
    var points = new HashSet<Point>();
    var minX = 500;
    var maxX = 500;
    var maxY = 0;

    var lines = File.ReadLines("input.txt");
    foreach (var line in lines)
    {
      string[] structures = line.Split(" -> ", StringSplitOptions.RemoveEmptyEntries);
      for (int i = 0; i < structures.Length - 1; i++)
      {
        var start = ParsePoint(structures[i]);
        var end = ParsePoint(structures[i+1]);

        var startX = (start.x <= end.x) ? start.x : end.x;
        var endX = (start.x <= end.x) ? end.x : start.x;
        if (startX <= minX) minX = startX;
        if (endX >= maxX) maxX = endX;
        for (int x = startX; x <= endX; x++)
        {
          var startY = (start.y <= end.y) ? start.y : end.y;
          var endY = (start.y <= end.y) ? end.y : start.y;
          if (endY > maxY) maxY = endY;
          for (int y = startY; y <= endY; y++)
          {
            var point = new Point(x, y);
            points.Add(point);
          }
        }
      }
    }

    return new Grid(points, minX, maxX, maxY);
  }

  void Solve()
  {
    var grid = MakeGrid();
    Console.WriteLine("Part 1: {0}", grid.CountSand(false));

    grid = MakeGrid();
    Console.WriteLine("Part 2: {0}", grid.CountSand(true));
  }

  static void Main(string[] args)
  {
    new RegolithReservoir().Solve();
  }
}
