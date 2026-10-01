CXX ?= c++
CXXFLAGS ?= -O3 -std=c++17 -Wall -Wextra -pedantic

.PHONY: all clean benchmark-p2 benchmark-p2-compare benchmark-p2-wilson-r1 benchmark-p2-wilson-r1-compare

all: build/p2_closed_bps

build:
	mkdir -p build

build/p2_closed_bps: src/p2_closed_bps.cpp | build
	$(CXX) $(CXXFLAGS) $< -o $@

build/p2_closed_bps_legacy: src/p2_closed_bps.cpp | build
	$(CXX) $(CXXFLAGS) -DP2_LEGACY_SPARSE=1 $< -o $@

benchmark-p2: build/p2_closed_bps
	./build/p2_closed_bps --degree 30 --quiet
	./build/p2_closed_bps --degree 42 --quiet

benchmark-p2-compare: build/p2_closed_bps build/p2_closed_bps_legacy
	./build/p2_closed_bps --degree 30 --quiet
	./build/p2_closed_bps_legacy --degree 30 --quiet
	./build/p2_closed_bps --degree 42 --quiet
	./build/p2_closed_bps_legacy --degree 42 --quiet

benchmark-p2-wilson-r1: build/p2_closed_bps
	./build/p2_closed_bps --wilson --max-r 1 --degree 41 --quiet --profile

benchmark-p2-wilson-r1-compare: build/p2_closed_bps build/p2_closed_bps_legacy
	./build/p2_closed_bps --wilson --max-r 1 --degree 41 --quiet
	./build/p2_closed_bps_legacy --wilson --max-r 1 --degree 41 --quiet

clean:
	rm -rf build
