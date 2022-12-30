# Advent of Code 2022
## [Day 5](https://adventofcode.com/2022/day/5) - Supply Stacks

### Programming Language 

[Lisp](https://en.wikipedia.org/wiki/Lisp_(programming_language)) was originally developed by John McCarthy at MIT in 1958.
It is the second-oldest high-level programming language still in common use, behind only Fortran which is a year older.

It has traditionally been a favorite programming language for artificial intelligence research.
It is also popular because of its use as the scripting language for the Emacs editor.
A dialect of Lisp known as ZIL (Zork Implementation Language) was created by Infocom and used to create their popular text adventures in the 1980s.

### Why I Chose It

I think I was aware of Lisp in high school but I don't believe I ever did any programming in it until I took a "Survey of Programming Languages" class in college.
After that I used it in a few other college classes, including an independent study focused on natural language processing using [CLOS](https://en.wikipedia.org/wiki/Common_Lisp_Object_System).

The fact that Lisp source code is itself a set of lists that can be operated on has always blown my mind.
It was also my introduction to functional programming principles.
And I love how easy it is to express recursive functions.
That said, it never interested me enough to make it part of my regular toolbox.
I'm pretty sure this is the first time I've programmed in LISP since college, other than some practice exercises I did to prepare for using it in AoC.

### How It Went

Parsing strings in Common LISP is nobody's idea of fun.
After struggling to build up some functions to parse the starting stacks portion of the input, I gave up and took the advice of one of my coworkers to manually create the array of stacks and only parse the list of moves.
Even that took quite a bit of googling since `split`, `split-string`, `split-sequence`, and some other suggestions I found for splitting a sentence into word tokens are unavailable (as near as I can tell) in Steel Bank Common Lisp.
Finally found `words` in the SBCL docs as the solution I was looking for.
Even that required some experimentation though.
I made the incorrect assumption that the result would be a list of just words (non-whitespace) but instead the list contained both the words and the whitespace.
Frustrating.

The actual puzzle solution itself was mostly straightforward.
It went slower than I would have liked just because I'm out of practice with Lisp programming.
But I'm happy with my solution.

I may go back later and try to get the parsing of the stacks working.
Maybe.

### Tools

[SBCL](https://www.sbcl.org/manual/index.html) (Steel Bank Common Lisp)

### Running

Because I didn't implement parsing of the initial stack configuration, the code as is won't work for any input but mine.
Or, I guess, any input that doesn't happen to have the exact same starting stack configuration as mine.
So in order to use custom input with a different stack configuration, you'll have to edit the `initial-stacks` function.

Once you've done that, you can fetch your input file and run the program, by entering:

```
make run
```
