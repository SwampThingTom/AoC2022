all: build build/RegolithReservoir

.PHONY: all

build:
	mkdir -p $@

build/RegolithReservoir: RegolithReservoir.cs
	mcs -out:$@ RegolithReservoir.cs

clean:
	rm -f build/*
