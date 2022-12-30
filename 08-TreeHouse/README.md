# Advent of Code 2022
## [Day 8](https://adventofcode.com/2022/day/8) - Treetop Tree House

### Programming Language 

[BASH](https://en.wikipedia.org/wiki/Bash_(Unix_shell)) was originally developed by Brian Fox for the Free Software Foundation (FSF) in 1989.
The name, Bourne-Again Shell, is a play on the fact that BASH was a follow-on to the Bourne Shell.
It remains by far the most popular shell on Linux.

### Why I Chose It

After graduating and taking a job with IBM, I was working with AIX on RS/6000 workstations.
The default shell on AIX has always been Korn shell (ksh).
That's where I first did a lot with shell scripting.
Although ksh isn't compatible with bash, all of the UNIX shell scripting languages are fairly similar.

Later, when I had gotten into the Apple ecosystem, BASH became my default scripting language, since it was the default shell on MacOS until Catalina.
This is the first language I've used in AoC so far that I still use today, both personally and professionally.

### How It Went

For part 1, just iterate over each row and column in both directions, adding trees that can be seen to an associative array.
Easy peasy.

With the ability to easily iterate over the grid in any direction from part 1, part 2 wasn't bad either.
BASH's use of pipes to return values from functions makes the syntax of calling the function that calculates the scenic value for a tree a bit odd but since I'm used to it, it was easy to code.

Comparing scripting languages, BASH's syntax is terrible compared to Rexx.
Looking at the code, I'd be hard pressed to find an easy way to explain what it's doing to someone who isn't already familiar with BASH.
But BASH has real arrays and associative arrays which makes it much easier to model problems like this one (if you are familiar with the syntax).

Overall neither language is ideal for AoC puzzles.
For the kinds of problems I use scripting languages for, I prefer Rexx's syntax but Bash is ubiquitous, has better data structures, and, at this point, I'm familiar enough with the syntax that I can write it pretty quickly.

### Tools

[GNU BASH](https://tiswww.case.edu/php/chet/bash/bashtop.html)

### Running

To automatically fetch your input file and run the program, enter:

```
make run
```
