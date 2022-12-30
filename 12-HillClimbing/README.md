# Advent of Code 2022
## [Day 12](https://adventofcode.com/2022/day/12) - Hill Climbing Algorithm

### Programming Language 

[Java](https://en.wikipedia.org/wiki/Java_(programming_language)) was originally developed in 1995 by James Gosling at Sun Microsystems.
Java was widely marketed as "Write Once, Run Anywhere" because it compiles to bytecode that runs on a virtual machine with implementations on a large number of platforms.

The syntax is based on C / C++ but greatly simplified.
It also provides run-time inspection of objects similar to Smalltalk.

### Why I Chose It

I started playing with Java in the late 1990s, near the end of my time with IBM.
I used it occasionally throughout the early 2000s but it wasn't a common language for government contracts.
I was using it the most somewhere between 2005-2007.
During that time I was using it for most of my personal projects, and even found ways to use it professionally.

Once I started developing for iOS, I stopped using Java for personal projects in favor of Objective-C and C#.
It wasn't until I joined Appian, whose core client-server platform is written in Java, that I started using it again.
Although it isn't my primary language at Appian, I continue to make contributions to our Java code as necessary.

### How It Went

My main hangup today was trying to decide whether to use Dijkstra's pathfinding algorithm or A*.
For the pathfinding puzzle last year, I initially implemented it using Dijkstra in Python but it was too slow on the second part of the puzzle, so I switched to A* for both parts.
So this morning I started by implementing A* before realizing that it would almost certainly not be more efficient for this particular problem.
A* shines when there are many paths to the solution but some paths are more expensive than others.
In today's puzzle, many paths were blocked off due to the difference in altitude leading to a smaller set of possible solutions that twisted their way around the map.
So I switched back to Dijkstra and solved part 1 quickly.

I also solved part 2 quickly but it isn't particularly efficient.
It should be easy enough to enhance my pathfinding function to prune trees once they reach a count that is larger than the current best path.
Memoizing known paths would likely be an even better optimization (something easy to do in Python).

I'm far from a Java expert despite how long I've been using it.
It's just not the language I'm most interested in using.
But it's a solid language with support for a variety of data types and collections which makes it great for using on Advent of Code puzzles.

A few minor things that tripped me up (and made me laugh) only because they are different from the two languages I use the most now (Swift and Python):
1) I typed `self` instead of `this` more times than I'd care to admit.
2) I typed `nil` instead of `null` just as many times as I used `self`.
3) I have come to dislike languages that require ";" as statement terminators / separators.
4) I used `==` instead of `.equals` once which led to the only real bug I had to fix.
I could not understand why it wasn't detecting that it had found the end node.
smh

### Tools

[OpenJDK](https://openjdk.org/)

### Running

To automatically fetch your input file and run the program, enter:

```
make run
```
