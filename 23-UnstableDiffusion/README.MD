# Advent of Code 2022
## [Day 23](https://adventofcode.com/2022/day/23) - Unstable Diffusion

### Programming Language 

[Dart](https://en.wikipedia.org/wiki/Dart_(programming_language)) was designed at Google by Lars Bak, who also led the development of Chrome's V8 JavaScript engine, and Lasper Kund.
It was initially released in 2013.
The original plan was to include a Dart VM in Chrome which led to criticism of Google for fragmenting web development.
That plan was dropped and Google instead focused on compiling Dart to JavaScript for use on the web.

In 2017, Google released Flutter, an open-source framework for creating cross-platform applications, using Dart as the runtime and preferred language.
Originally intended as an alternative to React/React Native for cross-platform web and mobile apps, it now supports native Linux, MacOS, and Windows apps.

### Why I Chose It

I had read a bit about Flutter and Dart around the time that Appian was considering React Native.
But it wasn't until I purchased Robert Nystrom's "[Crafting Interpreters](https://craftinginterpreters.com/)" in 2021 that I had a chance to use it.
He wrote a short but effective [test runner](https://github.com/munificent/craftinginterpreters/blob/master/tool/bin/test.dart) in Dart for testing the language he creates in that book.
He also works on the team responsible for Dart at Google.

When I started playing around with creating my own language using the ideas from his book, I adapted his Dart code for my own test runner.
I found it pretty easy to use, even as a noob.

### How It Went

After more than week of using languages that don't require semicolons, the Dart compiler had to remind me frequently that I needed them today.
Other than that, today's puzzle was pretty easy and I had no trouble implementing it in Dart.
The only oddity I found is the funky syntax for optional parameters.
Had to google that and was surprised that they have to be wrapped in {}.

At some point I probably should write a small Flutter mobile app.
Would be interesting to compare the experience to React Native and native mobile development.

### Tools

[Dart](https://dart.dev/)

### Running

To automatically fetch your input file and run the program, enter:

```
make run
```
