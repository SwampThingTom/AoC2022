# Advent of Code 2022
## [Day 21](https://adventofcode.com/2022/day/21) - Monkey Math

### Programming Language 

[Kotlin](https://en.wikipedia.org/wiki/Kotlin_(programming_language)) was developed by JetBrains and officially released in 2016.
It's a JVM language with a cleaner, "modern" syntax and support for both OO and functional programming styles.
In 2019, Google made it the preferred language for Android development.

It is currently the most popular language for targetting Android, with Google claiming that 70% of the top 1,000 apps in the Play Store are written in Kotlin.
It can also be compiled to JavaScript for use in web applications, and has some server-side development support as well.

### Why I Chose It

I've only done a very little bit with Kotlin.
We considered using Kotlin Native for a cross-platform mobile feature at Appian.
And, of course, almost all of our Android development is now in Kotlin.
But I haven't written a full app in Kotlin myself.

I like what I have seen.
It provides a lot of the features that I like about C# and Swift: type inference, optionals, value types ("data classes"), higher order functions, and, of course, no semicolons required.
I miss Swift's `guard` statements though.

### How It Went

Pretty easy one today.
I was originally thinking of using a sealed class to hold the value of a node (either an integer or an operation).
But it turned out to be easy enough (and fast enough) to just treat the nodes as arrays that either hold a single value or three strings representing the operation and operands.

This might have been a good day to use Scala since I still haven't finished Day 16 using that language.
Oh, well.

### Tools

[Kotlin](https://kotlinlang.org/)

For this puzzle, I used their command-line compiler, installed via Homebrew, rather than one of JetBrains IDEs.
I like writing all of my AoC solutions in VSCode!

### Running

To automatically fetch your input file and run the program, enter:

```
make run
```
