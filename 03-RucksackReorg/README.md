# Advent of Code 2022
## [Day 3](https://adventofcode.com/2022/day/3) - Rucksack Reorganization

### Programming Language 

[Pascal](https://en.wikipedia.org/wiki/Pascal_(programming_language)) was designed by Niklaus Wirth in 1970.
It was very popular in the 1970s and 80s both in colleges and for writing commercial software.

Apple developed an object-oriented variant called [Object Pascal](https://en.wikipedia.org/wiki/Object_Pascal).
It was the company's language of choice until 1991 when it was displaced by C++.

### Why I Chose It

Pascal was my introduction to structured programming languages, strong typing, data structures, and pointers.
It quickly became my favorite language my senior year of high school.
As an intern at IBM around 1988, I fixed bugs in a Pascal program that processed sonar data on submarines.

### How It Went

Sets in Pascal are fairly limited compared to sets in modern programming languages.
They are implemented as bitmaps and their API makes it easy to do common operations like intersection, union, and contains.
But standard Pascal doesn't provide an easy way to do things like count the number of items in a set or get an item from a set.
After spending some time googling, "pascal get item from set" and "pascal get iterator for set", I hacked my own version by using a `For` loop and returning the first item.

Other than that, today's problem was very straightforward to solve in Pascal.
I'm a little disappointed that the problem didn't require using a linked list since that's one of the main things I associate with Pascal.

As much as I loved Pascal at the time, and still appreciate its strong typing and expressiveness, it's quite verbose and the standard library is limited compared to modern languages.

### Tools

[Free Pascal](https://freepascal.org)

### Running

To automatically fetch your input file and run the program, enter:

```
make run
```
