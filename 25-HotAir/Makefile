YEAR=2022
DAY=25
SESSION=~/.aoc-session

# Because it isn't possible to execute SAIL from a command line, this makefile simply
# fetches the input and converts it into a format suitable for using in an expression
# rule.

all: run

run: input.sail
	@echo "Visit https://community.appian.com/ to get started with Appian SAIL."
	@echo "Your input has been placed in 'input.sail' as a SAIL list."

clean:
	rm -f input.txt input.sail

input.sail: input.txt
	scripts/convert_input.py

input.txt:
	@test -s $(SESSION) || { echo "Advent of Code session file not found ($(SESSION))."; exit 1; }
	curl --cookie "session=$$(cat $(SESSION))" "https://adventofcode.com/$(YEAR)/day/$(DAY)/input" -o $@

.PHONY: all clean
