YEAR=2022
DAY=9
APP=RopeBridge
SRC=$(APP).adb
SESSION=~/.aoc-session

all: run

run: input.txt $(APP)
	./$(APP)

clean:
	rm -f input.txt $(APP) *.o *.ali

input.txt:
	@test -s $(SESSION) || { echo "Advent of Code session file not found ($(SESSION))."; exit 1; }
	curl --cookie "session=$$(cat $(SESSION))" "https://adventofcode.com/$(YEAR)/day/$(DAY)/input" -o $@

$(APP): $(SRC)
	gnatmake $(SRC)

.PHONY: all run clean
