YEAR=2022
DAY=5
APP=SupplyStacks.lisp
SESSION=~/.aoc-session

all: run

run: input-moves.txt
	./$(APP)

clean:
	rm -f input.txt input-moves.txt

input-moves.txt: input.txt
	scripts/convert_input.py

input.txt:
	@test -s $(SESSION) || { echo "Advent of Code session file not found ($(SESSION))."; exit 1; }
	curl --cookie "session=$$(cat $(SESSION))" "https://adventofcode.com/$(YEAR)/day/$(DAY)/input" -o $@

.PHONY: all run clean
