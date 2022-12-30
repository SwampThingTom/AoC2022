# Advent of Code 2022
## [Day 25](https://adventofcode.com/2022/day/25) - Full of Hot Air

### Programming Language 

[SAIL](https://en.wikipedia.org/wiki/Appian_Corporation#SAIL_(Self-Assembling_Interface_Layer)) is a low-code framework for building enterprise apps using Appian.
Although SAIL apps are mostly created using a drag-and-drop interface designer, SAIL is backed by a functional expression language.

### Why I Chose It

I've been working at Appian since 2014, mostly focusing on our front-end software that renders SAIL across web and mobile devices.
Although I don't write SAIL every day, we take dog-fooding very seriously.
Our interface designer used to create SAIL apps is written in SAIL.
And we run most of our business processes, including tracking the engineering release cycle, using apps written in SAIL.

### How It Went

Although Advent of Code puzzles are not at all the type of problem our customers purchase Appian to solve, today's puzzle was pretty straightforward to solve with our expression language.
My solution looks similar to what I'd write in any other functional programming language.
It converts each snafu value to decimal by iterating over the characters, converting them to their decimal digit, and then using reduce to multiply the accumulated value by 5 and add the new digit value.
After summing all of the decimal values, it converts it back to snafu using a recursive function that divides the current value by 5, converts that digit to its snafu value, and concatenates it with the result so far.

Because Appian's interface designer is GUI-based and not intended to be written in text files (in some ways similar to SmallTalk environments like Pharo), I manually copied the source of each "rule" (function) into a single text file, [aoc_fullOfHotAir.sail](aoc_fullOfHotAir.sail).

I also wrote a script, `scripts/convert_input.py`, that converts the puzzle input from `input.txt` into a SAIL list that can be copied and pasted into an expression rule.
It saves the converted input as `input.sail`.

### Tools

[Appian SAIL](https://community.appian.com/)
