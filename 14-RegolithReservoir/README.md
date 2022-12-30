# Advent of Code 2022
## [Day 14](https://adventofcode.com/2022/day/14) - Regolith Reservoir

### Programming Language 

[C#](https://en.wikipedia.org/wiki/C_Sharp_(programming_language)) was created in 2000 by Anders Hejlsberg at Microsoft.
There's a bit of controversy around its similarities to Java, in part because it was released during a time when Sun was suing Microsoft over its implementation of Java.
James Gosling, creator of Java, and Bill Joy, co-founder of Sun Microsystems, derided C# as a poor imitation of Java in 2002.

Despite the controversy, C# is one of the most popular programming languages.
It's mostly used in the .Net ecosystem; however, it's also used to write videogames.
Unity uses it as its scripting language for game objects.
And Microsoft had a videogame framework of their own, XNA, from the mid-1990s until 2010.
XNA and C# made it very easy to write cross-platform games for Windows and Xbox 360, and helped open up the Xbox to indie software developers.

### Why I Chose It

I started learning C# in 2007 because I thought XNA was a great videogame development platform.
It was pretty cool being able to run my own games on my Xbox without having to buy an Xbox development kit.

In 2008, I took a job with a startup called Reality Mobile that made one of the early video streaming platforms.
Their entire stack was written in C# with a Windows Mobile client.
Although I was hired as an iOS developer, working at a startup means that everyone does everything so I wrote my first commercial server-side code of my career.

I loved C#.
I found it to be close enough to Java to be easy to learn while fixing many of the issues that made Java development tedious.
The following features were all an improvement over Java, in my opinion:

- Structs as value types
- Type inference
- Not requiring thrown exception declarations
- Linq

I still use it a little for playing around in Unity.

### How It Went

Very well.
C# made reading and parsing the input trivial.
I used a Set to manage the points in the grid that are occupied, and a recursive depth-first search to add the next grain of sand.
That could be optimized to continue filling sand in as it unwinds but I couldn't quite get that working quickly.
For now it requires calling it again for each grain of sand.
Not as efficient as it could be but still takes less than 5 seconds to run both parts.

I should do more in C#.
It's a nice incremental improvement to Java.
And I think it has a lot of the advantages of Python but without some of the klunky syntax.

### Tools

[Mono](https://www.mono-project.com/)

### Running

To automatically fetch your input file and run the program, enter:

```
make run
```
