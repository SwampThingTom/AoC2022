all: build/CampCleanup

.PHONY: all

build:
	mkdir -p $@

build/CampCleanup: build CampCleanup.c
	cc -o $@ -Wno-deprecated-declarations CampCleanup.c

clean:
	rm -f build/*
