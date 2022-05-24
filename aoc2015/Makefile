.PHONY: build
build:
	stack build --pedantic

.PHONY: day01
day01: build
	stack run -- d01 < inputs/day01.txt


.PHONY: day02
day02: build
	stack run -- d02 < inputs/day02.txt

.PHONY: day03
day03: build
	stack run -- d03 < inputs/day03.txt

.PHONY: day04
day04: build
	stack run -- d04 < inputs/day04.txt

.PHONY: day05
day05: build
	stack run -- d05 < inputs/day05.txt

.PHONY: clean
clean:
	stack clean

.PHONY: all
all: build day01
