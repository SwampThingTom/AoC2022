# Advent of Code 2022
## [Day 18](https://adventofcode.com/2022/day/18) - Boiling Boulders

### Programming Language 

[Ruby](https://en.wikipedia.org/wiki/Ruby_(programming_language)) was designed by Yukihiro "Matz" Matsumoto in 1995.
He wanted an object-oriented scripting language and wasn't impressed by Perl or Python.
The language was heavily influenced by Lisp and Smalltalk, as well as Perl, Python, Eiffel, Ada, BASIC, and Java.
It's popularity soared in 2005 with the emergence of Ruby on Rails, a web application framework based on Ruby.

### Why I Chose It

Ruby is a popular language for development tools on OS X / MacOS.
CocoaPods, a widely-used dependency management tool for iOS and MacOS programs, and FastLane, a build and deployment tool for iOS and Android, are both written in Ruby.

Appian's iOS app supports a variety of configurations and custom branding options.
When I joined we were using a set of difficult to maintain Bash scripts to configure what gets built.
One of my teammates had heard of Fastlane and proposed using it instead.
The combination of Fastlane and a set of custom Ruby scripts has proven to be much more robust and easier to maintain than the old Bash scripts.

### How It Went

Originally I had planned to use Swift today.
But once I saw that the problem looked quite easy, I decided to switch up and use Ruby instead.
I'm much more proficient in Swift and want to save it for a harder problem.

Using a class to represent cubes is completely overkill.
The problem can easily be solved in Ruby just by treating cubes as an array containing its x, y, and z values.
But one of my goals with this exercise is to show off language capabilities as much as possible.
And Ruby is an object-oriented language at heart.

### Tools

[Ruby](https://www.ruby-lang.org/)

### Running

To automatically fetch your input file and run the program, enter:

```
make run
```
