YEAR=2022
DAY=3
APP=RucksackReorg
SRC=$(APP).pas
SESSION=~/.aoc-session

all: run

run: input.txt build/$(APP)
	build/$(APP)

clean:
	rm -rf build input.txt

input.txt:
	@test -s $(SESSION) || { echo "Advent of Code session file not found ($(SESSION))."; exit 1; }
	curl --cookie "session=$$(cat $(SESSION))" "https://adventofcode.com/$(YEAR)/day/$(DAY)/input" -o $@

build:
	mkdir -p $@

build/$(APP): build $(SRC)
	fpc -o$@ $(SRC)

.PHONY: all run clean
