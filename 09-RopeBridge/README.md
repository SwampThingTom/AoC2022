# Advent of Code 2022
## [Day 9](https://adventofcode.com/2022/day/9) - Rope Bridge

### Programming Language 

[Ada](https://en.wikipedia.org/wiki/Ada_(programming_language)) was designed by a DoD working group tasked with solving the problem of projects being written in a plethora of new computer languages, all with their own propensity for producing bugs.
It was initially released in 1983 as MIL-STD-1815.

Between 1991 and 1997 (coincidentally, almost the exact years I was with IBM's Federal Systems Division), Ada was officially mandated for all DoD contracts.
That said, exceptions were frequently granted.

### Why I Chose It

I was introduced to Ada in a senior computer science class on software project management in 1990.
I loved how Ada used a Pascal-like syntax and strong typing while providing access to very low-level capabilities, like bit manipulation and concurrency, that I associated with C.
I became a bit of an Ada fan-boy and thought it was the language of the future.

Over the next decade, some of the worst professional code I've ever written was in Ada.
I can't blame the language for my own deficiencies but it made me realize that no language is bullet-proof.
And that the large amount of boilerplate that had to be written to express simple concepts did not protect you from bugs.

By 2000 I was past disillusionment but continued to have to maintain poorly written Ada code.
I haven't used it since 2007 or 2008 when I removed it from my resume.
That said, I have heard that more recent versions of Ada have addressed some of its problems.

### How It Went

That's a lot of code just to store a coordinate in a set.
And writing the hash function was painful because I kept getting integer overflows.
Also having to specify the function name both in the declaration AND in the `end` statement is no fun.
But I got part 1 working without too much trouble.

Part 2 required a smaller refactor than I had expected at first glance.
Just kept an array of knots and update all of them instead of just a head and tail.

Overall I think this was a good problem to do in Ada.
Honestly the hardest part was having to implement a hash function for my set type.
Beyond that it was just slow to implement because I had to remember (google) syntax for a language I haven't used in over 15 years.

### Tools

[GNAT (GNU Ada)](https://www.gnu.org/software/gnat/)

### Running

To automatically fetch your input file and run the program, enter:

```
make run
```
