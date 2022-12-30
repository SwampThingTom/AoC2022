# Advent of Code 2022
## [Day 6](https://adventofcode.com/2022/day/6) - Tuning Trouble

### Programming Language 

[Smalltalk](https://en.wikipedia.org/wiki/Smalltalk) was developed by Alan Kay and others at Xerox PARC and first appeared in 1972.
The first general release was in 1980 as Smalltalk-80.
In 1988, ANSI Smalltalk standardized the language.

Smalltalk was influenced by Algol, the first object-oriented programming language, and itself heavily influenced later object-oriented languages, including Java, Ruby, CLOS (Common Lisp Object System), and, especially, Objective-C.
Objective-C took almost everything about Smalltalk -- syntax, message passing, dynamic dispatch -- and glued it onto C.

Unlike most of those languages to follow, Smalltalk is a "pure" object-oriented language where everything is an object and there are no primitive types (at least from the perspective of the developer).
This includes classes which themselves are objects of a root `Metaclass` class.
Even blocks of code are objects allowing for a Lisp-like functional ability to pass code as data.

### Why I Chose It

I was introduced to Smalltalk in a college "Survey of Programming Languages" class around 1988.
It looked at half a dozen or so languages based on different programming paradigms.
The ones that stuck with me are Lisp, Prolog, Forth, and Smalltalk.

Smalltalk was the first object-oriented programming language I had seen.
And even though I didn't really "get" object-oriented programming until a few years later, I was fascinated by the language.
I loved the idea of methods as "messages" that are sent to objects.
And I found the named parameters to be very readable.

At the time I had no idea that Smalltalk was the basis for another language called Objective-C that, decades later, would become my primary programming language.
More on that later.

In my opinion, the main downside to Smalltalk is that it is mostly used within a combined GUI development and operating environment, such as Pharo and Squeak.
Smalltalk adherents tend to be fanatical about using them but I found them cumbersome to learn as a newcomer.
I avoided that by using GNU Smalltalk which can be run from a command line.
However it is not actively maintained, doesn't have an ARM distribution, and has pretty poor performance running under Rosetta 2 on an Apple Silicon Mac.
The other thing that hurts performance is that it uses garbage collection.

### How It Went

Super simple although it's disappointing that the problem did not require any OO functionality.
My experience with Objective-C made it comfortable to program in, despite the number of decades that have passed since I last touched Smalltalk.

### Tools

[GNU Smalltalk](https://www.gnu.org/software/smalltalk/)

### Running

To automatically fetch your input file and run the program, enter:

```
make run
```
