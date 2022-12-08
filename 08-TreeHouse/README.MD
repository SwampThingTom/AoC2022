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

For part 1, just iterated over each row and column in both directions, adding the tree to an associative array of trees that have been seen.
Easy peasy.

### Tools

[GNU Bash](https://tiswww.case.edu/php/chet/bash/bashtop.html)

### Running

```
./TreeHouse.sh
```
