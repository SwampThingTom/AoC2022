# Advent of Code 2022
## [Day 20](https://adventofcode.com/2022/day/20) - Grove Positioning System

### Programming Language 

[TypeScript](https://en.wikipedia.org/wiki/TypeScript) is an open-source programming language developed by Microsoft and released in 2012.
It is a superset of JavaScript that adds static typing.
Anders Hejlsberg, designer of C# and Turbo Pascal, was actively involved in its design.

### Why I Chose It

As much as JavaScript has improved over the years, the lack of static typing continues to be a source of bugs and programmer frustration.
After rewriting Appian's front-end in ReactJS, one of my coworkers (and key architect for that rewrite) started evangelizing TypeScript.
I quickly became a fan and started using TypeScript as much as possible in our React codebase.

### How It Went

I was a little frustrated that there were two important details that weren't provided in the puzzle description or sample inputs.
The first is that the actual data contains duplicates.
Not a big deal, I discovered that quickly and came up with a solution.
The bigger one is that you need to remove an item from the list before moving it so that it isn't counted when you determine its new location.
That caused me no end of frustration since everything worked fine on the sample input.

However, despite the poor puzzle description, TypeScript was a great language to do this in.
As soon as I saw the problem I knew it would be fun to write a circular linked list in TypeScript.
And it completes both parts in under 300 ms.

I also used this as a chance to play with [Deno](https://deno.land).
It's fantastic and I highly recommend using it.
It has built-in support for TypeScript and can be easily installed on MacOS via homebrew.
It was much faster to get started than setting up an npm environment to use TypeScript with node.

### Tools

- [TypeScript](https://www.typescriptlang.org/)
- [Deno](https://deno.land/)

### Running

To automatically fetch your input file and run the program, enter:

```
make run
```
