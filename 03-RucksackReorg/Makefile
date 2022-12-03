all: build/RucksackReorg

.PHONY: all

build:
	mkdir -p $@

build/RucksackReorg: build RucksackReorg.pas
	fpc -o$@ RucksackReorg.pas

clean:
	rm -f build/*
