all: build/DistressSignal

.PHONY: all

build:
	mkdir -p $@

build/DistressSignal: build DistressSignal.m
	clang -o $@ -framework Foundation DistressSignal.m

clean:
	rm -f build/*
