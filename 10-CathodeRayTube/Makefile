YEAR=2022
DAY=10
APP=CathodeRayTube
SRC=$(APP).cpp
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
	g++ -o $@ -std=c++11 $(SRC)

.PHONY: all run clean
