# Advent of Code 2022
## [Day 7](https://adventofcode.com/2022/day/7) - No Space Left On Device

### Programming Language 

[Rexx](https://en.wikipedia.org/wiki/Rexx) (Restructured Extended Executor) was designed by IBM Fellow Mike Cowlishaw as an 'own-time' project at IBM in the early 1980s.
It was intended to be a scripting language with a simplified version of PL/I syntax.
It shipped as an IBM product in 1982 and since then IBM has included it in most of its operating systems.
It was later ported to many other operating systems and was even a popular scripting language on the Commodore Amiga.

### Why I Chose It

During my first internship at IBM I worked for their IT department where Rexx was the automation language of choice.
After graduating college and starting to work full-time at IBM, I bought my first IBM PC with their brand new OS/2 operating system.
Rexx was a familiar friend and my go-to scripting language on it.

I stopped using it when I stopped purchasing IBM computers and finally embraced the Microsoft ecosystem.

### How It Went

I'd forgotten that the only real data structure in Rexx is what they call "compound variables" (aka "stem variables").
This lets you treat `myvar` (the "stem") as a compound structure that can have fields indexed using a dot followed by a "tail": `myvar.tail`.
You can simulate arrays by using numeric tails and tracking the number of items in `myarray.0`.
Anything more complex than that requires significant effort (or someone much more experienced with Rexx than I am these days).

I started out trying to model the file system using nested compound variables.
That failed miserably.
I ended up keeping two parallel "arrays": one containing the full path to each directory and the second containing the size of the files in that directory (including all subdirectories).
When it finds a file in the input, it adds its size to the size of the current directory and then recursively applies it to each parent directory.
The parsing itself was super simple, both because today's input was in an easily parsed format and because Rexx has great support for tokenizing and parsing lines of text.

I remember liking Rexx a lot back in the day.
But the lack of better data structure support and the inability to pass variables by reference makes it a bit painful to use.

### Tools

[Regina Rexx](https://regina-rexx.sourceforge.io/)

### Running

To automatically fetch your input file and run the program, enter:

```
make run
```
