# Advent of Code 2022
## [Day 17](https://adventofcode.com/2022/day/17) - Pyrocastic Flow

### Programming Language 

[Groovy](https://www.wikipedia.org/wiki/Apache_Groovy) was created in 2003 by James Strachan.
It is a Java-like scripting language with a simplified syntax.

### Why I Chose It

Appian uses Groovy with Gradle for our Jenkins CI/CD jobs.
I don't work in that area very often.
But I've made a few contributions since I've been at Appian.

### How It Went

Another JVM language.
Today's went very smoothly and Groovy was easy to get back into.
Had a minor setback when my first draft wasn't detecting that some points were already in the set of points filled by rocks.
Figured I'd have to implement equals and hashCode but decided to Google it.
Google turned up Groovy's `@EqualsAndHashCode` annotation.
I am in love.

After finishing part 1 pretty quickly, I realized that there was no way I was going to get this to be performant enough to do part 2 as is.
A quick look at reddit showed that people were using dynamic programming to find cycles.
Started working on it but wasn't able to complete it before having to do other things.

Update: I finished part 2.
Even with cycle detection, it still takes 30 seconds to run.
But it works.

Overall, I was happy with Groovy.
I'm a little suprised it isn't used more outside of CI/CD and testing.
It provides some nice improvements over Java, including type inference, dynamic dispatch, treating all types as objects, an `==` operator that actually checks for equality, and a more concise syntax.

### Tools

[Groovy](https://groovy-lang.org/)

### Running

To automatically fetch your input file and run the program, enter:

```
make run
```
