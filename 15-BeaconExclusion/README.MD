# Advent of Code 2022
## [Day 15](https://adventofcode.com/2022/day/15) - Beacon Exclusion Zone

### Programming Language 

[JavaScript](https://en.wikipedia.org/wiki/JavaScript) was initially designed by Brendan Eich at Netscape in 1995.
During the early years of the World Wide Web, Netscape recognized the need for having a programming language that could be run in a web browser to provide dynamic user behavior without having to reach back to a server.
Originally called LiveScript, Netscape released it as JavaScript in order to take advantage of the popularity of Java.
The language itself has nothing to do with Java other than both borrowing a C-like syntax.

While building Internet Explorer, Microsoft reverse-engineered JavaScript's interpreter and released their own JScript language based on it in 1996.
For the next decade, Netscape (which became Mozilla/Firefox), Microsoft, and (later) Google had a frenemy relationship that eventually resulted in the ECMAScript 5 standard in 2009.
Earlier that same year, Node.js was released which allowed JavaScript to be run outside of a web browser.

### Why I Chose It

I played with JavaScript a very little bit on personal web sites around 2000.
I appreciated the ability to use it to do small scripting for validating user input on an HTML page but found it very limited for uses beyond that.
At the time, AJAX (Asynchronous JavaScript and XML) felt like magic, and overly-complex magic at that.

A decade later I was working at Reality Mobile, a startup with a video streaming product, and started using JavaScript professionally.
That was my introduction to jQuery, a popular JavaScript support library.
jQuery provided a lot of functionality that made JavaScript more useable.
But the language itself still felt quite hacky to me and it was hard to debug.

My real trial by fire with JavaScript came at Appian.
When I joined Appian our front-end was backed by GWT, Google's framework that transpiled Java to JavaScript, sparing web developers from having to develop in JavaScript themselves.
But GWT proved to be not much of an improvement over JavaScript and was being deprecated by Google.
So Appian decided to rewrite our front-end in React.

As someone who complained constantly about the disparity between Appian's web and mobile code, this was a chance to see if React Native could help solve that problem.
It turned out to do so quite nicely.
Although I much prefer Swift to JavaScript, sharing a large portion of our business logic across all clients was irresistable.
I led the team that ported our rendering logic in the mobile clients to React Native, sharing the JavaScript business logic with our web client.
React Native has worked out very well for Appian.

Modern JavaScript is quite a different language than it was even 10 years ago.
I don't expect it will ever be my favorite language.
But for what it does, it is quite good.
And React makes it fairly easy to write and maintain cross-platform front-end code.

### How It Went

Both parts went fine once I hit on an approach that wouldn't take hours or unlimited memory to solve.
Took me a while before I came up with that though.

JavaScript made it easy to parse the input (using a regex) and filter/map over the sensors to find where their coverage overlapped with the target row.
My only language-specific gotcha was that I wanted to store the beacons in a `Set`.
But JavaScript doesn't make it easy to store plain-old-JS-objects in a set.
So I stored them in a `Map` using the stringified value as the key.
Kind of annoying and hacky but it works.

### Tools

[Node.js](https://nodejs.org/)

### Running

To automatically fetch your input file and run the program, enter:

```
make run
```
