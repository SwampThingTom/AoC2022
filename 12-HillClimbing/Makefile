YEAR=2022
DAY=12
APP=HillClimbing.java
SESSION=~/.aoc-session

all: run

run: input.txt
	java $(APP)

clean:
	rm -f input.txt

input.txt:
	@test -s $(SESSION) || { echo "Advent of Code session file not found ($(SESSION))."; exit 1; }
	curl --cookie "session=$$(cat $(SESSION))" "https://adventofcode.com/$(YEAR)/day/$(DAY)/input" -o $@

.PHONY: all run clean
