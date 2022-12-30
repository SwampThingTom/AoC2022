# Advent of Code 2022
## [Day 11](https://adventofcode.com/2022/day/11) - Monkey in the Middle

### Programming Language 

[Tcl](https://en.wikipedia.org/wiki/Tcl) ("tickle") was created in 1988 by John Ousterhout at University of California Berkeley.
He claims it was "born out of frustration".
It's a scripting language designed to be easy to learn and use.
The language run-time is compact enough to be used in embedded systems.

It's particularly popular in conjunction with Tk, a GUI toolkit that makes it easy to prototype user interfaces.
The combination is frequently referred to as Tcl/Tk.

### Why I Chose It

In the mid-1990s while at IBM, we were spiking writing real-time embedded software that would be running in a device that had not yet been made.
One of my teammates had heard of Tcl/Tk and recommended we consider using that to prototype how the device would respond to user input and call our library.
Tcl/Tk was the perfect UI prototyping tool at the time and we were amazed at how quickly we could create a working prototype that could call our software written in C.

### How It Went

Not well, haha.
Although it was a Sunday I had limited time to devote to it because I was out of town and driving back home that day.
That said, I didn't get anywhere near as far as I would have liked for the time that I did spend on it.
I struggled with things that to me felt like they should be simple, like, how do I return an array from a function (`proc`)?

This was the first language I've done so far that didn't come back to me quickly.
After finally getting the code to parse the input and print an array of `monster` dictionaries, I decided I had spent enough time on it.

I'll come back to this on a future day.
Whether I will continue trying to do it in Tcl or choose another language, we shall see.

**Updated Dec 26 2022:**
Getting both parts working turned out to not be so bad once I came back to it.
Once I realized that I can't think of variables as references to objects (memory) but instead as keys in a scoped dictionary, it became easier to proceed.
Nothing is passed by reference and it isn't possible to have multiple references to the same underlying value.
This is conceptually simple and helps newcomers avoid problems with setting a variable's value and being surprised that it also changed the value of a different variable referencing the same memory.
But it makes it more difficult for those of us who want to write functions that can update referenced variables.

Tcl's scoping rules are also a bit odd compared to other programming languages.
The programmer has to explicitly map local variables to variables in outer scopes, and even keep track of the nesting level.

One very cool thing, though, is the ability to evaluate strings as expressions.
This made it easy to inject the operations used to reduce worry for each part.

### Tools

[Tcl](https://www.tcl-lang.org/)

### Running

To automatically fetch your input file and run the program, enter:

```
make run
```
