# Advent of Code 2022
## [Day 4](https://adventofcode.com/2022/day/4) - Camp Cleanup

### Programming Language 

[C](https://en.wikipedia.org/wiki/C_(programming_language)) was created by Dennis Ritchie at Bell Labs in 1972.
It gained popularity during the 1980s and still remains one of the most popular programming languages.

### Why I Chose It

I learned C my freshman year of college.
It was the primary language for the CS department at George Mason University.

I loved that it was a high level language with many of the structural programming features of Pascal but provided easy access to low level hardware features, such as bit manipulation.
Programs written in C were concise and younger me considered the lack of safety to be more of a challenge than a pitfall.
It became my language of choice for at least the next decade.
I continued to use it professionally for at least another decade after that.

For a college AI class, I wrote an Othello game in C that would, decades later, become the basis for my 
[Morocco](https://apps.apple.com/us/app/morocco/id284946595) iOS app.

### How It Went

This was the first day where I felt completely comfortable and proficient with the language I was using.

My first thought when I read the puzzle description was to use sets.
However C doesn't have set functions in its standard library and I didn't want to write my own.
So I decided to just compare the start and end points to find containing sections and hoped that part 2 would not require sets.
That worked out and turned out to be as simple or simpler than using sets to solve the problem would have been.

As with Pascal, a lot of code is taken up just reading the input file and storing it in an array.
I suppose I could have made this simpler by just operating on each line as I read it rather than storing it.
But I like having the full set of data in memory for handling both parts.
It does make me miss the ease of quickly reading and parsing files into a list that languages like Python provide.

### Tools

[Clang](https://clang.llvm.org/)

### Running

To automatically fetch your input file and run the program, enter:

```
make run
```
