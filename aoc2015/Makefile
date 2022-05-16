.PHONY: build
build:
	stack build --pedantic

.PHONY: day01
day01: build
	stack run -- d01 < inputs/day01.txt

.PHONY: clean
clean:
	stack clean

.PHONY: all
all: build day01
