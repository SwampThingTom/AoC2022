# Advent of Code 2022
## [Day 13](https://adventofcode.com/2022/day/13) - Distress Signal

### Programming Language 

[Objective-C](https://en.wikipedia.org/wiki/Objective-C) was created by Tom Love and Brad Cox in 1984.
They were huge fans of Smalltalk who recognized the importance of compatibility with existing C code.

In 1988, Steve Jobs's NeXT licensed Objective-C and extended gcc to support it.
After Steve Jobs returned to Apple in 1996 and brought NeXT with him, Objective-C became the foundational language for the brand new Mac OS X.
As a result, it became the de facto programming language for all Apple hardware that followed (iPhone, iPad, Apple TV, and Apple Watch) through 2014 when Swift was introduced.

### Why I Chose It

In 2007 I made the decision to leave government contracting for commercial software development.
At the time I was mostly interested in breaking into the burgeoning Java software market, both because I liked the language and it seemed to be ubiquitous.

For quite a bit of the previous decade I had been playing around with cell phones.
I was doing some Windows CE development at work and had a top-end Nokia phone that I wrote VERY small games for.
I found development with both to be tedious.
And app discovery was painful and dangerous since there was no trusted source for apps.

That same year, Steve Jobs introduced the iPhone.
I bought the first model and was immediately hooked.

Then Apple announced that they would start letting developers write apps for the iPhone and make them available from an officially-supported App Store.
I immediately bought a Mac Mini, the first Apple computer I'd ever purchased.
I learned Objective-C by porting an Othello program that I'd written in C in college.
And I got [Morocco](https://apps.apple.com/us/app/morocco/id284946595) into the iOS App Store soon after launch.

Not long after getting Morocco in the App Store, I had an interview with a startup called Reality Mobile.
They had a mobile video streaming solution before YouTube was everywhere.
Although they understood that iOS 2.0 had limitations that prevented them from running the Palm or Windows CE versions of their software, they wanted to have iOS experience in-house to see what was possible and be ready to have an app as soon as possible.

Reality Mobile didn't survive but my decision to learn Objective-C and publish an app in the App Store fulfilled my desire to break into commercial software development.
I appreciate all of the good people at Reality Mobile who helped me make that transition.
After leaving them, I was hired by Appian as an iOS developer where I remain today, although I do less mobile development than I used to.

Swift has replaced Objective-C as Apple's (and my) preferred language.
But there is still a lot of Objective-C code out there.

### How It Went

Pretty straightforward.
Objective-C's string parsing APIs are a bit verbose but easy to use.
And its dynamic typing lets me store both `NSNumber` and `NSArray` objects together in an array and quickly be able to query each to determine whether a list item is a number or sub-list.

### Tools

[Clang](https://clang.llvm.org/)

### Running

To automatically fetch your input file and run the program, enter:

```
make run
```
