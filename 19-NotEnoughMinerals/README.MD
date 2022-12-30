# Advent of Code 2022
## [Day 19](https://adventofcode.com/2022/day/19) - Not Enough Minerals

### Programming Language 

[Swift](https://en.wikipedia.org/wiki/Swift_(programming_language)) was designed by Chris Lattner at Apple and released in 2014.
Apple was looking for a modern language to replace Objective-C while remaining compatible with its runtime.

Chris Lattner says that the language drew inspiration "from Objective-C, Rust, Haskell, Ruby, Python, C#, CLU, and far too many others to list".
It maintains Objective-C features such as dynamic dispatch, the ability to extend classes with categories, and named function arguments.
But it provides a cleaner syntax that looks familiar to Java and C# developers.

Apple open-sourced its development in December 2015.
It is widely used on all Apple platforms.
But it is also used on Linux, including Linux servers and Raspberry Pi.
It's available on Windows but support for Windows APIs is still lagging.
The Browser Company is helping lead the effort to complete support for Windows.
They plan to release their Arc web browser, written entirely in Swift, on Windows.

### Why I Chose It

I fell in love with Swift as soon as I started using the beta released after WWDC 2014.
It felt like what I thought Objective-C should have been from the beginning, a type-safe, OO language with dynamic dispatch, and a syntax that is accessible to modern programmers.
It also borrows a number of ideas that I loved in C#, such as type inference and structs that are immutable value types.
And it supports functional programming features.
Functions are first-class citizens, closures have a much cleaner syntax than Objective-C's "blocks", and the language has built-in support for higher-order functions.

Swift remains my favorite programming language for both work and side projects.

### How It Went

I'm glad that I saved Swift for today, despite the fact that I technically started using it before I learned Ruby (which I used for yesterday's puzzle).
Today's puzzle looked very easy when I first read it.
But I quickly realized that it required some clever tree-pruning in order to be fast enough to solve.

After switching back and forth between depth-first search and breadth-first search and trying a few options to prune the search tree, I finally broke down and looked at Reddit.
I found a post discussing [heuristics that could be used to prune the search tree](https://www.reddit.com/r/adventofcode/comments/zpy5rm/2022_day_19_what_are_your_insights_and/) and implemented two of them.
First, I implemented a function to determine whether it was even worth trying to build certain types of robots based on available resources and time left.
Second, I made it so that it decides on a particular robot to build and then fast-forwards to when it is complete.

It still takes 6 minutes to solve part 2 so it could use some additional optimization.
But it solves the problem and got me 2 stars, so I'm good with it for now.

### Tools

[Swift](https://www.swift.org/)

### Running

To automatically fetch your input file and run the program, enter:

```
make run
```
