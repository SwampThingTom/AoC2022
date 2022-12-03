#!/usr/bin/env basic

10 rem Calorie Counting
20 rem https://adventofcode.com/2022/day/1
30 rem
100 most1 = 0 : most2 = 0 : most3 = 0
110 calories = 0
120 open "input.txt" for input as #1
130 while not eof(1)
140   input #1, line$
150   if line$ = "" then
160     gosub 500 ' Track the top 3 calorie counts.
170     calories = 0
180   else
190     calories = calories + int(line$)
200   endif
210 wend
220 close #1
300 print "Part 1: ";most1
310 print "Part 2: ";most1 + most2 + most3
320 end
500 rem
510 rem Track the top 3 calorie counts.
520 rem
530 if calories > most1 then
540   most3 = most2
550   most2 = most1
560   most1 = calories
570   return
580 endif
590 if calories > most2 then
600   most3 = most2
610   most2 = calories
620   return
630 endif
640 if calories > most3 then
650   most3 = calories
660   return
670 endif
680 return
