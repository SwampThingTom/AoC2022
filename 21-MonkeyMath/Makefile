YEAR=2022
DAY=21
APP=MonkeyMath.jar
SRC=$(basename $(APP)).kt
SESSION=~/.aoc-session

all: run

run: input.txt $(APP)
	java -jar $(APP)

clean:
	rm -f input.txt MonkeyMath.jar

input.txt:
	@test -s $(SESSION) || { echo "Advent of Code session file not found ($(SESSION))."; exit 1; }
	curl --cookie "session=$$(cat $(SESSION))" "https://adventofcode.com/$(YEAR)/day/$(DAY)/input" -o $@

$(APP): $(SRC)
	kotlinc $(SRC) -include-runtime -d $(APP)

.PHONY: all run clean
