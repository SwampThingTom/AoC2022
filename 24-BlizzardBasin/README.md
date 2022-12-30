# Advent of Code 2022
## [Day 24](https://adventofcode.com/2022/day/24) - Blizzard Basin

### Programming Language 

[Rust](https://en.wikipedia.org/wiki/Rust_(programming_language)) was designed by Graydon Hoare at Mozilla and released in 2015.
It is intended for high-performant systems programming, where C++ has traditionally been used.
It's most unique feature is that it enforces memory safety without using garbage collection (like Java and Python) or reference counting (like Swift and C++ smart pointers).
It does this by statically tracking ownership of all memory references and throwing compiler errors if code attempts to reference memory that isn't owned.

### Why I Chose It

Rust has gotten a lot of press recently for being a high-performant language with better memory safety than C++.
Over the summer of this year (2022), I decided to start playing with it by attempting to port my [Wordle Solver](https://github.com/SwampThingTom/wordle-solver) from Swift to Rust.
I didn't get very far, mainly due to other things taking priority rather than lack of interest.
That said, it did give me an appreciation for how picky the Rust compiler is and how it forces you to think about memory references in a way that other languages don't.
It also gave me an appreciation for how good the compiler error messages are at telling you why the code won't compile and providing reasonable hints about how to fix it.

### How It Went

I really should have approached today's puzzle using TDD.
I had so many off-by-one errors.
And I was trying to use `%` to mean modulo instead of remainder like a total n00b.
*sigh*

For the most part, the language didn't cause any real stumbling blocks.
I had tons of compiler errors but they pretty much always gave correct hints for how to fix the problem.
My code is not particularly performant compared to what a Rust expert would write.
Mainly because I pretty much solved all memory ownership problems by making clones of data rather than trying to use references and satisfy the borrow checker.

In order to make part 2 more performant, I added memoization of the map states.
That worked very well but made me miss how easy it is to do that in Python.

I'm sure I could have solved this in either Python or Swift much faster than I did in Rust and with close to the same performance.
But I strongly suspect that if I used Rust more, I could write far more performant code quickly and safely.

Overall, I'm intrigued.
From the little I've seen so far, I think it's an improvement over C++ for writing high-performant code.
I'm still torn on whether Rust's [borrow checker](https://doc.rust-lang.org/1.8.0/book/references-and-borrowing.html) is an improvement over Swift's [Automatic Reference Counting (ARC)](https://docs.swift.org/swift-book/LanguageGuide/AutomaticReferenceCounting.html).
Both use the compiler to generate memory safe code.
ARC handles it almost entirely automatically, requiring the developer merely to provide hints about whether a memory reference should be weak (unowned) or strong (owned).
Rust handles it by throwing compiler errors that require the developer to make changes, even in cases where technically there isn't a problem with multiple owners.
But Rust does handle the case of multiple owners of a mutable object accessing it concurrently in a safer way than Swift.

### Tools

[Rust](https://www.rust-lang.org/)

### Running

To automatically fetch your input file and run the program, enter:

```
make run
```
