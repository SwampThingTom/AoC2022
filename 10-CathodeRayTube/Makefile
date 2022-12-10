all: build/CathodeRayTube

.PHONY: all

build:
	mkdir -p $@

build/CathodeRayTube: build CathodeRayTube.cpp
	g++ -o $@ -std=c++11 CathodeRayTube.cpp

clean:
	rm -f build/*
