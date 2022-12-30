YEAR=2022
DAY=2
APP=RockPaperScissors
SESSION=~/.aoc-session

all: run

run: input.asm build/$(APP).d64
	x64sc build/$(APP).d64

clean:
	rm -rf build input.txt input.asm

input.asm: input.txt
	scripts/convert_input.py

input.txt:
	@test -s $(SESSION) || { echo "Advent of Code session file not found ($(SESSION))."; exit 1; }
	curl --cookie "session=$$(cat $(SESSION))" "https://adventofcode.com/$(YEAR)/day/$(DAY)/input" -o $@

build/$(APP).d64: build $(APP).asm
	acme --format cbm --outfile build/$(APP).prg $(APP).asm
	c1541 -format default,lodis d64 build/$(APP).d64 -write build/$(APP).prg

build:
	mkdir -p $@

.PHONY: all run clean
